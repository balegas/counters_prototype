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

%% API
-export([init/2, loop/2]).

-record(client_rc, {id :: term(), address :: string(), app_name :: term(), succ_count :: integer(), op_count :: integer(), stats_pid :: term()}).

loop(init, Client) ->
  Client#client_rc.stats_pid ! start,
  loop(infinite,Client);

loop(Value, Client) when Value =< 0 ->
  Client#client_rc.stats_pid ! stop;

loop(Value, Client) ->
  InitTime = now(),
  ClientMod = Client#client_rc{op_count=Client#client_rc.op_count+1},
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
    {finished, V} -> loop(V,ClientMod)
  end.

init(0,_NodeAddress,_Stats) -> started;

init(N,NodeAddress,Stats)->
  Client = #client_rc{id=client, address=list_to_atom(NodeAddress),
    app_name=crdtdb, succ_count = 0, op_count=0, stats_pid = Stats},
  spawn(client,loop,[init,Client]),
  init(N-1,NodeAddress,Stats).

init({NodeAddress,N},Stats) ->
  init(N,NodeAddress,Stats);

init(N,NodeAddresses)->
  Stats = client_stats:start(),
  lists:foreach(fun(Pair) -> init(Pair,Stats) end, lists:map(fun(Address) ->
    {Address,N div length(NodeAddresses)} end, NodeAddresses)).



