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
-export([init/6, loop/2, reset/4, uniform_generator/1, zipf_generator/2]).

-define(KEY,<<"0">>).

-record(time_client_rc, {id :: term(), address :: string(), app_name  :: term(), succ_count :: integer(), op_count :: integer(), stats_pid :: term(), generator :: fun(), time_limit :: erlang:timestamp()}).

loop(init, Client) ->
  Client#time_client_rc.stats_pid ! start,
  loop(now(),Client);

loop(Time, Client) ->
  Elapsed = timer:now_diff(now(),Time) div 1000000,
  case Elapsed < Client#time_client_rc.time_limit of
    true ->  loop(allowed,Time,Client);
    false -> Client#time_client_rc.stats_pid ! stop
  end.

loop(allowed,Time,Client) ->
  InitTime = now(),
  ClientMod = Client#time_client_rc{op_count=Client#time_client_rc.op_count+1},

  Ref = make_ref(),
  Client#time_client_rc.generator ! {self(), Ref, request},
  receive
    {random,Ref,Random} -> RandomKey = integer_to_binary(Random)
  end,

  TT=?MAX_INTERVAL- random:uniform(?MAX_INTERVAL div 3),
  timer:sleep(TT),

  case rpc:call(Client#time_client_rc.address, Client#time_client_rc.app_name, decrement, [RandomKey]) of
    {ok, UpdValue} ->
      Client#time_client_rc.stats_pid ! {self(), RandomKey, UpdValue, timer:now_diff(now(),InitTime),InitTime,success},
      ClientMod2 = Client#time_client_rc{op_count=ClientMod#time_client_rc.op_count+1},
      loop(Time, ClientMod2);
    fail ->
      Client#time_client_rc.stats_pid ! {self(), RandomKey, 0, timer:now_diff(now(),InitTime),InitTime,failure},
      loop(Time, ClientMod);
    {forbidden,V} ->
      Client#time_client_rc.stats_pid ! {self(), RandomKey, V, timer:now_diff(now(),InitTime),InitTime,forbidden},
      loop(Time, ClientMod);
    {finished, V} ->
      Client#time_client_rc.stats_pid ! {self(), RandomKey, V, timer:now_diff(now(),InitTime),InitTime,finished},
      loop(Time, ClientMod);
    _ -> io:format("RPC fail~n"), loop(Time, ClientMod)
  end.

init(N,NKeys,ExecutionTime,NodeAddress,Generator,Folder)->
  Stats = client_stats:start(Folder,lists:concat(["T",N])),
  GeneratorPid = spawn_link(time_client_rc,Generator,[NKeys]),
  init(Stats,N,GeneratorPid,ExecutionTime,NodeAddress,Generator,Folder).

init(_,0,_,_,_,_,_) ->
  receive
    _ -> timer:sleep(2000),
      ok
  end;


init(Stats,N,GeneratorPid,ExecutionTime,NodeAddress,Generator,Folder)->

  Client = #time_client_rc{id=client, address=NodeAddress,
  app_name=crdtdb, succ_count = 0, op_count=0, stats_pid = Stats, generator = GeneratorPid, time_limit=ExecutionTime},
  spawn_monitor(time_client_rc,loop,[init,Client]),
  init(Stats,N-1,GeneratorPid,ExecutionTime,NodeAddress,Generator,Folder).


reset(Address,NKeys,InitValue,AllAddresses)  ->
  rpc:call(Address, crdtdb, start, [NKeys,InitValue,AllAddresses]).

uniform_generator(NKeys) ->
  receive
    {Pid, Ref, request} ->
      Pid ! {random,Ref,random:uniform(NKeys)}
  end,
  uniform_generator(NKeys).

zipf_generator(Size, Skew) ->
  Bottom = lists:foldl(fun(X,Sum) -> Sum+1/math:pow(X,Skew) end,0,lists:seq(1,Size-1)),
  zipf_generator(Size,Skew,Bottom)
.

zipf_generator(Size,Skew,Bottom) ->
  receive
    {Pid, Ref, request} ->
      Rank = random:uniform(Size),
      Frequency = (1.0 / math:pow(Rank,Skew))/Bottom,
      Dice = random:uniform(),
      Next = next(Rank,Frequency,Dice,Size,Skew,Bottom),
      Pid ! {random,Ref,Next}
  end,
  zipf_generator(Size,Skew,Bottom).

next(Rank,Frequency,Dice,_Size,_Skew,_Bottom) when Dice >= Frequency -> Rank;

next(Rank,Frequency,Dice,Size,Skew,Bottom) when Dice >= Frequency ->
  Rank = random:uniform(Size),
  Frequency = (1.0 / math:pow(Rank,Skew))/Bottom,
  Dice = random:uniform(),
  next(Rank,Frequency,Dice,Size,Skew,Bottom)
.
