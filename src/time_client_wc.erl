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
-export([init/11, loop/3]).

-record(time_client_wc, {id :: term(), app_name  :: term(), succ_count :: integer(), stats_pid :: term(), generator :: fun(), time_limit :: erlang:timestamp(), decrement_prob :: non_neg_integer(), think_time :: non_neg_integer(), worker:: worker_counter:worker()}).

loop(init, Client, Consistency) ->
  random:seed(),
  Client#time_client_wc.stats_pid ! start,
  loop(now(),Client,Consistency);

loop(Time, Client, Consistency) ->
  Elapsed = timer:now_diff(now(),Time),
  case Elapsed < Client#time_client_wc.time_limit * 1000000 of
    true ->  loop(allowed,Time,Client,Consistency);
    false ->
      Ref = make_ref(),
      Client#time_client_wc.stats_pid ! {self(),Ref,stop},
      Client#time_client_wc.stats_pid ! {self(),Ref,stop},
      receive
        {Ref,ok} -> ok
      end
  end.

loop(allowed,Time,Client,Consistency) ->
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
  TT= Client#time_client_wc.think_time - random:uniform(Client#time_client_wc.think_time div 3),
  timer:sleep(TT),

  InitTime = now(),
  {Status, NewValue} = case Op of
    decrement -> worker_counter:decrement(Consistency,Client#time_client_wc.worker,RandomKey);
    increment -> worker_counter:increment(Consistency,Client#time_client_wc.worker,RandomKey)
  end,
  Client#time_client_wc.stats_pid ! {self(), RandomKey, NewValue, 0, timer:now_diff(now(),InitTime), InitTime, Op, Status},
  loop(Time,Client,Consistency).

init(RiakAddress,RiakPort,Bucket,N,GeneratorPid,ExecutionTime,DecrementProb,Folder,Consistency,Region,ThinkTime)->
  Stats = client_stats:start(Folder,lists:concat(["T",N]),self()),
  init(Stats,RiakAddress,RiakPort,Bucket,N,GeneratorPid,ExecutionTime,DecrementProb,Folder,Consistency,Region,ThinkTime).

init(_,_,_,_,0,_,_,_,_,_,_,_) ->
  receive
    finish -> ok
  end;

init(Stats,RiakAddress,RiakPort,Bucket,N,GeneratorPid,ExecutionTime,DecrementProb,Folder,Consistency,Region,ThinkTime)->
  random:seed(erlang:now()),
  Client = #time_client_wc{id=client, succ_count = 0, stats_pid = Stats, generator = GeneratorPid, time_limit=ExecutionTime, decrement_prob = DecrementProb, think_time = ThinkTime, worker = worker_counter:init(RiakAddress,RiakPort,Bucket,Region)},
  spawn_monitor(time_client_wc,loop,[init,Client,Consistency]),
  init(Stats,RiakAddress,RiakPort,Bucket,N-1,GeneratorPid,ExecutionTime,DecrementProb,Folder,Consistency,Region,ThinkTime).

