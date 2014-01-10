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
-export([init/4, loop/2, reset/4]).

-record(client_rc, {id :: term(), address :: string(), app_name  :: term(), succ_count :: integer(), op_count :: integer(), stats_pid :: term()}).

loop(init, Client) ->
  Client#client_rc.stats_pid ! start,
  loop(rpc:call(Client#client_rc.address, Client#client_rc.app_name, get_value, []),Client);

loop(Value, Client) when Value =< 0 ->
  Client#client_rc.stats_pid ! stop;

loop(Value, Client) ->
  InitTime = now(),
  ClientMod = Client#client_rc{op_count=Client#client_rc.op_count+1},

  TT=?MIN_INTERVAL- random:uniform(?MIN_INTERVAL div 3),
  timer:sleep(TT),

  case rpc:call(Client#client_rc.address, Client#client_rc.app_name, decrement, []) of
    {ok, UpdValue} ->
      Client#client_rc.stats_pid ! {self(), UpdValue, timer:now_diff(now(),InitTime),InitTime,success},
      ClientMod2 = Client#client_rc{op_count=ClientMod#client_rc.op_count+1},
      loop(UpdValue,ClientMod2);
    fail ->
      Client#client_rc.stats_pid ! {self(), Value, timer:now_diff(now(),InitTime),InitTime,failure},
      loop(Value,Client);
    {forbidden,CRDT} ->
      loop(nncounter:value(CRDT),ClientMod);
    {finished, V} ->
      loop(V,ClientMod)
  end.

init(N,NodeAddress,Bucket,Id)->
  Stats = client_stats:start(lists:concat(["T",N,"-",erlang:binary_to_list(element(1,Bucket))])),
  init(Stats,N,NodeAddress,Bucket,Id).

init(_,0,_,_,_) ->
  receive
    _ -> io:format("Statistics stopped"),
      timer:sleep(2000),
      ok
  end;


init(Stats,N,NodeAddress,Bucket,Id)->
  Client = #client_rc{id=client, address=list_to_atom(lists:concat(["crdtdb@",NodeAddress])),
  app_name=crdtdb, succ_count = 0, op_count=0, stats_pid = Stats},
  spawn_monitor(client,loop,[init,Client]),
  init(Stats,N-1,NodeAddress,Bucket,Id).


%Updates the CRDT directly on the database
reset(V,Bucket,LocalId,AllAddressIds)  ->
  worker_rc:reset_crdt(V,Bucket,LocalId,AllAddressIds).