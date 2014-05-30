%%%-------------------------------------------------------------------
%%% @author balegas
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Dez 2013 19:10
%%%-------------------------------------------------------------------
-module(client_counter).
-author("balegas").
-include("constants.hrl").

%% API
-export([init/5, loop/2]).

-record(client, {id :: term(), worker:: worker_counter:worker(), 
                 succ_count :: integer(), op_count :: integer(),
                 stats_pid :: term()}).

loop(init, Client) ->
  Client#client.stats_pid ! start,
  loop(worker_counter:get_value(Client#client.worker,?DEFAULT_KEY),Client);

loop(Value, Client) when Value =< 0 ->
  Ref = make_ref(),
  Client#client.stats_pid ! {self(),Ref,stop},
  receive
    {Ref,ok} -> ok
  end;

loop(_Value, Client) ->
    ClientMod = Client#client{op_count=Client#client.op_count+1},
    TT=?MAX_INTERVAL- random:uniform(?MAX_INTERVAL div 3),
    timer:sleep(TT),

    InitTime = now(),
    case worker_counter:get_value(Client#client.worker,?DEFAULT_KEY) of
        UpdValue when UpdValue > 0 ->
            worker_counter:decrement(Client#client.worker,?DEFAULT_KEY),
            Client#client.stats_pid ! 
            {self(), ?DEFAULT_KEY, UpdValue-1, 0, 
             timer:now_diff(now(),InitTime),InitTime,decrement,success},
            ClientMod2 = Client#client{op_count=ClientMod#client.op_count+1},
            loop(UpdValue,ClientMod2);
        UpdValue when UpdValue =< 0 ->
            Client#client.stats_pid ! 
            {self(), ?DEFAULT_KEY, UpdValue, 0, 
             timer:now_diff(now(),InitTime),InitTime,decrement,failure},
            loop(UpdValue,Client)
    end.

init(RiakAddress,RiakPort,N,Bucket,Folder) ->
    Stats = client_stats:start(
              Folder, lists:concat(["T",N,"-",erlang:binary_to_list(
                                                element(1,Bucket))]),self()),
    init(Stats,RiakAddress,RiakPort,N,Bucket,Folder).

init(_,_,_,0,_,_) ->
    receive
        finish -> ok
    end;

init(Stats,RiakAddress,RiakPort,N,Bucket,Folder)->
    Client = #client{
                id = client, succ_count = 0, op_count = 0, stats_pid = Stats,
                worker = worker_counter:init(RiakAddress,RiakPort,Bucket)
               },
    spawn_monitor(client_counter,loop,[init,Client]),
    init(Stats,RiakAddress,RiakPort,N-1,Bucket,Folder).

