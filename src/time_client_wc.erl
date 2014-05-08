%%%-------------------------------------------------------------------
%%% @author balegas
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Dez 2013 19:10
%%%-------------------------------------------------------------------
-module(time_client_wc).
-author("balegas").
-include("constants.hrl").

%% API
-export([init/8, loop/2]).

-record(time_client_wc, {id :: term(), app_name  :: term(), succ_count :: integer(), op_count :: integer(), stats_pid :: term(), generator :: fun(), time_limit :: erlang:timestamp(), decrement_prob :: non_neg_integer(), worker:: worker_counter:worker()}).

loop(init, Client) ->
  random:seed(),
  Client#time_client_wc.stats_pid ! start,
  loop(now(),Client);

loop(Time, Client) ->
  Elapsed = timer:now_diff(now(),Time),
  case Elapsed < Client#time_client_wc.time_limit * 1000000 of
    true ->  loop(allowed,Time,Client);
    false ->
      Ref = make_ref(),
      Client#time_client_wc.stats_pid ! {self(),Ref,stop},
      Client#time_client_wc.stats_pid ! {self(),Ref,stop},
      receive
        {Ref,ok} -> ok
      end
  end.

loop(allowed,Time,Client) ->
  ClientMod = Client#time_client_wc{op_count=Client#time_client_wc.op_count+1},
  R = random:uniform(),
  Op = if
         R < Client#time_client_wc.decrement_prob -> decrement;
         true -> increment
       end,

  Ref = make_ref(),
  Client#time_client_wc.generator ! {self(), Ref, request},
  receive
    {random,Ref,Random} -> RandomKey = integer_to_binary(Random)
  end,
  TT=?MAX_INTERVAL- random:uniform(?MAX_INTERVAL div 3),
  timer:sleep(TT),

  InitTime = now(),
  case Op of
    decrement ->
      case worker_counter:get_value(Client#time_client_wc.worker,RandomKey) of
        UpdValue when UpdValue > 0 ->
          worker_counter:decrement(Client#time_client_wc.worker,RandomKey),
          Client#time_client_wc.stats_pid ! {self(), RandomKey, UpdValue-1, 0, timer:now_diff(now(),InitTime),InitTime,decrement,success},
          ClientMod2 = Client#time_client_wc{op_count=ClientMod#time_client_wc.op_count+1},
          loop(Time,ClientMod2);
        UpdValue when UpdValue =< 0 ->
          Client#time_client_wc.stats_pid ! {self(), RandomKey, UpdValue, 0, timer:now_diff(now(),InitTime),InitTime,decrement,failure},
          loop(Time,ClientMod)
      end;
    increment ->
      worker_counter:increment(Client#time_client_wc.worker,RandomKey),
      loop(Time,ClientMod)
  end.





init(RiakAddress,RiakPort,Bucket,N,GeneratorPid,ExecutionTime,DecrementProb,Folder)->
  Stats = client_stats:start(Folder,lists:concat(["T",N]),self()),
  init(Stats,RiakAddress,RiakPort,Bucket,N,GeneratorPid,ExecutionTime,DecrementProb,Folder).

init(_,_,_,_,0,_,_,_,_) ->
  receive
    finish -> ok
  end;

init(Stats,RiakAddress,RiakPort,Bucket,N,GeneratorPid,ExecutionTime,DecrementProb,Folder)->
  random:seed(erlang:now()),
  Client = #time_client_wc{id=client, succ_count = 0, op_count=0, stats_pid = Stats, generator = GeneratorPid, time_limit=ExecutionTime, decrement_prob = DecrementProb, worker = worker_counter:init(RiakAddress,RiakPort,Bucket)},
  spawn_monitor(time_client_wc,loop,[init,Client]),
  init(Stats,RiakAddress,RiakPort,Bucket,N-1,GeneratorPid,ExecutionTime,DecrementProb,Folder).

