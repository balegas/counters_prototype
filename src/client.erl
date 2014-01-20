%%%-------------------------------------------------------------------
%%% @author balegas
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Dez 2013 19:10
%%%-------------------------------------------------------------------
-module(client).
-author("balegas").
-include("constants.hrl").

%% API
-export([init/6, loop/2, reset/5]).

-record(client, {id :: term(), worker:: worker_rc:worker(), succ_count :: integer(), op_count :: integer(), stats_pid :: term()}).

loop(init, Client) ->
  Client#client.stats_pid ! start,
  loop(worker_rc:get_value(Client#client.worker,?DEFAULT_KEY),Client);

loop(Value, Client) when Value =< 0 ->
  Ref = make_ref(),
  Client#client.stats_pid ! {self(),Ref,stop},
  receive
    {Ref,ok} -> ok
  end;

loop(Value, Client) ->
  ClientMod = Client#client{op_count=Client#client.op_count+1},
  
  TT=?MAX_INTERVAL- random:uniform(?MAX_INTERVAL div 3),
  timer:sleep(TT),

  InitTime = now(),
  case worker_rc:decrement(Client#client.worker,?DEFAULT_KEY) of
    {ok, UpdValue} ->
      Client#client.stats_pid ! {self(), ?DEFAULT_KEY, UpdValue, 0, timer:now_diff(now(),InitTime),InitTime,decrement,success},
      ClientMod2 = Client#client{op_count=ClientMod#client.op_count+1},
      loop(UpdValue,ClientMod2);
    fail ->
      Client#client.stats_pid ! {self(), ?DEFAULT_KEY, Value, 0, timer:now_diff(now(),InitTime),InitTime,decrement,failure},
      loop(Value,Client);
    {forbidden,CRDT} ->
      loop(nncounter:value(CRDT),ClientMod);
    {finished, UpdValue} ->
      loop(UpdValue,ClientMod);
    _ -> io:format("RPC fail~n"), loop(0, ClientMod)
  end.

init(RiakAddress,RiakPort,N,Bucket,Id,Folder)->
  Stats = client_stats:start(Folder,lists:concat(["T",N,"-",erlang:binary_to_list(element(1,Bucket))]),self()),
  init(Stats,RiakAddress,RiakPort,N,Bucket,Id,Folder).

init(_,_,_,0,_,_,_) ->
  receive
    finish -> ok
  end;


init(Stats,RiakAddress,RiakPort,N,Bucket,Id,Folder)->
    Client = #client{id=client, worker=worker_rc:init(RiakAddress,RiakPort,Bucket,Id),
      succ_count = 0, op_count=0, stats_pid = Stats},
	spawn_monitor(client,loop,[init,Client]),
	init(Stats,RiakAddress,RiakPort,N-1,Bucket,Id,Folder).


reset(RiakAddress,RiakPort,InitialValue,Bucket,AllAddressIds)  ->
  worker_rc:reset_crdt(InitialValue,Bucket,?DEFAULT_KEY,RiakAddress,RiakPort, AllAddressIds).

