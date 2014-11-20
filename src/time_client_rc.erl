%%%-------------------------------------------------------------------
%%% @author balegas
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Dez 2013 19:10
%%%-------------------------------------------------------------------
-module(time_client_rc).
-author("balegas").
-include("constants.hrl").

%% API
-export([init/7, loop/2, reset/4, start/2]).

-record(time_client_rc, {id :: term(), address :: string(), app_name  :: term(),
                         succ_count :: integer(), op_count :: integer(),
                         stats_pid :: term(),
                         generator :: fun(), time_limit :: erlang:timestamp(), 
                                          decrement_prob :: non_neg_integer()}).

loop(init, Client) ->
    random:seed(),
    Client#time_client_rc.stats_pid ! start,
    loop(now(),Client);

loop(Time, Client) ->
    Elapsed = timer:now_diff(now(),Time),
    case Elapsed < Client#time_client_rc.time_limit * 1000000 of
        true ->  loop(allowed,Time,Client);
        false ->
            Ref = make_ref(),
            Client#time_client_rc.stats_pid ! {self(),Ref,stop},
            Client#time_client_rc.stats_pid ! {self(),Ref,stop},
            receive
                {Ref,ok} -> ok
            end
    end.

loop(allowed,Time,Client) ->
    ClientMod = Client#time_client_rc{
                  op_count = Client#time_client_rc.op_count+1},
    R = random:uniform(),
    Op = if
             R < Client#time_client_rc.decrement_prob -> decrement;
             true -> increment
         end,

    Ref = make_ref(),
    Client#time_client_rc.generator ! {self(), Ref, request},
    receive
        {random,Ref,Random} ->
            RandomKeySeq = Random
    end,
    TT= ?MAX_INTERVAL - random:uniform(?MAX_INTERVAL div 3),
    timer:sleep(TT),
    RandomKey = erlang:list_to_binary(erlang:integer_to_list(RandomKeySeq) ++ "_" ++ Client#time_client_rc.id),

    InitTime = now(),
    case rpc:call(Client#time_client_rc.address, Client#time_client_rc.app_name, 
                  Op, [RandomKey]) of
        {ok, UpdValue,Per} ->
            Client#time_client_rc.stats_pid ! 
            {self(), RandomKey, UpdValue, Per, 
             timer:now_diff(now(),InitTime),InitTime,Op,success},
            ClientMod2 = 
            Client#time_client_rc{op_count=ClientMod#time_client_rc.op_count+1},
            loop(Time,ClientMod2);
        fail ->
            Client#time_client_rc.stats_pid !
            {self(), RandomKey, 0, 0, 
             timer:now_diff(now(),InitTime),InitTime,Op, fail},
            loop(Time,ClientMod);
        {forbidden,UpdValue} ->
            Client#time_client_rc.stats_pid ! 
            {self(), RandomKey, UpdValue, 0, timer:now_diff(now(),InitTime),
             InitTime,Op, forbidden},
            loop(Time,ClientMod);
        {finished, UpdValue} ->
            Client#time_client_rc.stats_pid ! 
            {self(), RandomKey, UpdValue, 0, timer:now_diff(now(),InitTime),
             InitTime,Op, finished},
            loop(Time,ClientMod);
        Other ->
            io:format("RPC fail ~p ~p ~p ~n",[Other,Op, RandomKey]),
            loop(Time, ClientMod)
    end
    .

init(NodeName,N,GeneratorPid,ExecutionTime,DecrementProb,Folder,Region)->
    Stats = client_stats:start(Folder,lists:concat(["T",N]),self()),
    init(Stats,NodeName,N,GeneratorPid,ExecutionTime,DecrementProb,Folder,Region).

init(_,_,0,_,_,_,_,_) ->
    receive
        finish -> ok
    end;

init(Stats,NodeName,N,GeneratorPid,ExecutionTime,DecrementProb,Folder,Region)->
    random:seed(erlang:now()),
    Client = #time_client_rc{
                id=Region, 
                address=NodeName,
                app_name= crdtdb,
                succ_count = 0, 
                op_count= 0, 
                stats_pid = Stats, 
                generator = GeneratorPid, 
                time_limit= ExecutionTime, 
                decrement_prob = DecrementProb
               },
    spawn_monitor(time_client_rc,loop,[init,Client]),
    init(Stats,NodeName,N-1,GeneratorPid,ExecutionTime,DecrementProb,Folder,Region).

reset(Address,NKeys,InitValue,AllAddresses)  ->
    rpc:call(Address, crdtdb, reset, [NKeys,InitValue,AllAddresses]).

start(Address,AllAddresses)  ->
    rpc:call(Address, crdtdb, start, [AllAddresses]).

