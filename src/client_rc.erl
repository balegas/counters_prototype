%%%-------------------------------------------------------------------
%%% @author balegas
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Dez 2013 19:10
%%%-------------------------------------------------------------------
-module(client_rc).
-author("balegas").
-include("constants.hrl").

%% API
-export([init/4, loop/2, start/3, reset/6, reset/5]).

-record(client_rc, 
        {id :: term(), address :: string(), app_name  :: term(),
         succ_count :: integer(), op_count :: integer(),
         stats_pid :: term()}).

loop(init, Client) ->
    Client#client_rc.stats_pid ! start,
    loop(
      rpc:call(Client#client_rc.address, Client#client_rc.app_name, get_value, [])
      ,Client);

loop(Value, Client) when Value =< 0 ->
    Ref = make_ref(),
    Client#client_rc.stats_pid ! {self(),Ref,stop},
    receive
        {Ref,ok} -> ok
    end;

loop(Value, Client) ->
    ClientMod = Client#client_rc{op_count=Client#client_rc.op_count+1},
    TT=?MAX_INTERVAL- random:uniform(?MAX_INTERVAL div 3),
    timer:sleep(TT),
    InitTime = now(),
    Key = erlang:list_to_binary(?DEFAULT_KEY ++ "_" ++ erlang:atom_to_list(Client#client_rc.id)),
    case 
        rpc:call(Client#client_rc.address, Client#client_rc.app_name, 
                 decrement, [Key])
    of
        {ok, UpdValue,Per} ->
            Client#client_rc.stats_pid ! 
                {self(), Key, UpdValue, Per, 
                 timer:now_diff(now(),InitTime),InitTime,decrement,success},
            ClientMod2 = 
            Client#client_rc{op_count=ClientMod#client_rc.op_count+1},
            loop(UpdValue,ClientMod2);
        fail ->
            Client#client_rc.stats_pid ! 
            {self(), Key, Value, 0, 
             timer:now_diff(now(),InitTime),InitTime,decrement,failure},
            loop(Value,ClientMod);
        {forbidden,UpdValue} ->
            loop(retry,UpdValue,ClientMod,InitTime);
        {finished, UpdValue} ->
            Client#client_rc.stats_pid ! 
            {self(),Key, UpdValue, 0,
             timer:now_diff(now(),InitTime),InitTime,decrement,finished},
            loop(UpdValue,ClientMod);
        Other -> 
            io:format("RPC fail ~p ~p~n",[Other,Key]),
            loop(Value, ClientMod)
    end.

%Quick Hack for retry client
loop(retry , Value, Client, InitTime) ->
    Key = erlang:list_to_binary(?DEFAULT_KEY ++ "_" ++ erlang:atom_to_list(Client#client_rc.id)),
    case rpc:call(Client#client_rc.address, Client#client_rc.app_name, 
                  decrement, [Key]) of
        {ok, UpdValue,Per} ->
            Client#client_rc.stats_pid !
            {self(), Key, UpdValue, Per, 
             timer:now_diff(now(),InitTime),InitTime,decrement,success},
            ClientMod2 = Client#client_rc{op_count=Client#client_rc.op_count+1},
            loop(UpdValue,ClientMod2);
        fail ->
            Client#client_rc.stats_pid !
            {self(), Key, Value, 0,
             timer:now_diff(now(),InitTime),InitTime,decrement,failure},
            loop(Value,Client);
        {forbidden,UpdValue} ->
            timer:sleep(500),
            loop(retry,UpdValue,Client,InitTime);
        {finished, UpdValue} ->
            Client#client_rc.stats_pid !
            {self(),Key, UpdValue, 0, 
             timer:now_diff(now(),InitTime),InitTime,decrement,finished},
            loop(UpdValue,Client);
        Other -> 
            io:format("RPC fail ~p ~p~n",[Other,Key]), 
            loop(Value, Client)
    end.

init(NodeName,N,Folder,Region) ->
    Stats = client_stats:start(Folder,
                               lists:concat(["T",N,"-","RIAK_CORE"]),self()),
    init(Stats,NodeName,N,Folder,Region).

init(_,_,0,_,_) ->
    receive
        finish -> ok
    end;

init(Stats,NodeName,N,Folder,Region) ->
    Client = 
    #client_rc{
       id = Region,
       address = NodeName, 
       app_name = crdtdb,
       succ_count = 0,
       op_count= 0,
       stats_pid = Stats
      },
    spawn_monitor(client_rc,loop,[init,Client]),
    init(Stats,NodeName,N-1,Folder).

reset(Region, Address,NKeys,InitValue,AllAddresses) ->
    rpc:call(Address, crdtdb, reset, [Region,NKeys,InitValue,AllAddresses]).

reset(Random, Region,Address,NKeys,InitValue,AllAddresses) ->
    rpc:call(Address, crdtdb, reset, [Region,NKeys,InitValue,AllAddresses,Random]).

start(Address,RegionId,AllAddresses) ->
    rpc:call(Address, crdtdb, start, [RegionId,AllAddresses]).
