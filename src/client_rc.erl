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
-export([init/3, loop/2, reset/4]).

-define(KEY,<<"0">>).

-record(client_rc, {id :: term(), address :: string(), app_name  :: term(), succ_count :: integer(), op_count :: integer(), stats_pid :: term()}).

loop(init, Client) ->
  Client#client_rc.stats_pid ! start,
  loop(rpc:call(Client#client_rc.address, Client#client_rc.app_name, get_value, []),Client);

loop(Value, Client) when Value =< 0 ->
  Client#client_rc.stats_pid ! stop;

loop(Value, Client) ->
  ClientMod = Client#client_rc{op_count=Client#client_rc.op_count+1},

  TT=?MAX_INTERVAL- random:uniform(?MAX_INTERVAL div 3),
  timer:sleep(TT),

  InitTime = now(),
  case rpc:call(Client#client_rc.address, Client#client_rc.app_name, decrement, [?DEFAULT_KEY]) of
    {ok, UpdValue} ->
      Client#client_rc.stats_pid ! {self(), ?DEFAULT_KEY, UpdValue, timer:now_diff(now(),InitTime),InitTime,success},
      ClientMod2 = Client#client_rc{op_count=ClientMod#client_rc.op_count+1},
      loop(UpdValue,ClientMod2);
    fail ->
      Client#client_rc.stats_pid ! {self(), ?DEFAULT_KEY, Value, timer:now_diff(now(),InitTime),InitTime,failure},
      loop(Value,ClientMod);
    {forbidden,UpdValue} ->
      Client#client_rc.stats_pid ! {self(),?DEFAULT_KEY, UpdValue, timer:now_diff(now(),InitTime),InitTime,forbidden},
      loop(Value,ClientMod);
    {finished, UpdValue} ->
      loop(UpdValue,ClientMod);
    _ -> io:format("RPC fail~n"), loop(Value, ClientMod)
  end.

init(N,NodeAddress,Folder)->
  Stats = client_stats:start(Folder,lists:concat(["T",N])),
  init(Stats,N,NodeAddress,Folder).

init(_,0,_,_) ->
  receive
    _ -> timer:sleep(2000),
      ok
  end;


init(Stats,N,NodeAddress,Folder)->
  Client = #client_rc{id=client, address=NodeAddress,
  app_name=crdtdb, succ_count = 0, op_count=0, stats_pid = Stats},
  spawn_monitor(client_rc,loop,[init,Client]),
  init(Stats,N-1,NodeAddress,Folder).


reset(Address,NKeys,InitValue,AllAddresses)  ->
  rpc:call(Address, crdtdb, start, [NKeys,InitValue,AllAddresses]).