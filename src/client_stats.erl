%%%-------------------------------------------------------------------
%%% @author balegas
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Nov 2013 21:49
%%%-------------------------------------------------------------------
-module(client_stats).
-author("balegas").

%% API
-export([stats/11, start/3]).

-include("constants.hrl").

start(Folder,Suffix,ParentPid) ->
  {Day,Time} = calendar:local_time(),
  DayS = lists:concat(
           lists:concat(
             lists:map(
               fun(X) ->
                       [X,"-"]
               end, erlang:tuple_to_list(Day)))),
  TimeS = lists:concat(
            lists:concat(
              lists:map(
                fun(X) ->
                        [X,"-"] 
                end, erlang:tuple_to_list(Time)))),
  Filename = lists:concat([Folder,DayS, TimeS, Suffix]),
  {ok, File} = file:open(Filename , [read, write]),
  io:fwrite(File,"~s\t~s\t~s\t~s\t~s\t~s\t~s\t~s~n",
            ["THREAD", "KEY", "VALUE", "PERMISSIONS", "LATENCY", "START_TIME", 
             "OPERATION", "OP_STATUS"]),
  spawn(client_stats, stats, 
        [orddict:new(),0,0,0,0,infinite,now(),0,0,File,ParentPid]).



stats(Bins,Success,Fail,Forbidden,Finished,MinValue,StartTime,Running,Total,File,ParentPid) ->
receive
  {Pid, Key, Value, Permissions, Latency, Timestamp, Operation, Status} ->
    case Status of
      success ->
        io:fwrite(File, "~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~n", 
                  [Pid,Key,Value, Permissions, Latency, now_microsec(Timestamp), Operation, Status]),
        NewBins = orddict:update(timer:now_diff(Timestamp,StartTime) div ?PLOT_INTERVAL,
          fun({Sum,Count}) -> {Sum+Latency,Count+1} end, {0,0}, Bins),
        stats(NewBins,Success+1,Fail,Forbidden,Finished, min(Value,MinValue),
              StartTime,Running,Total,File,ParentPid);
      failure ->
        io:fwrite(File, "~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~n", 
                  [Pid,Key,Value, Permissions, Latency, now_microsec(Timestamp), Operation, Status]),
        stats(Bins,Success,Fail+1,Forbidden,Finished, min(Value,MinValue),
              StartTime,Running,Total,File,ParentPid);
      forbidden ->
        io:fwrite(File, "~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~n", 
                  [Pid,Key,Value, Permissions, Latency, now_microsec(Timestamp), Operation, Status]),
        stats(Bins,Success,Fail,Forbidden+1,Finished, min(Value,MinValue),
              StartTime,Running,Total,File,ParentPid);
      finished ->
        io:fwrite(File, "~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~n", 
                  [Pid,Key,Value, Permissions, Latency, now_microsec(Timestamp), Operation, Status]),
        stats(Bins,Success,Fail,Forbidden,Finished+1, min(Value,MinValue),
              StartTime,Running,Total,File,ParentPid)
    end;
  start ->
    stats(Bins,Success,Fail,Forbidden,Finished,MinValue,StartTime,Running+1,
          Total+1,File,ParentPid);
  {Pid,Ref,stop} when Running == 1 ->
    io:format("Stop Running ~p~n",[Running]),
    print_output(Bins,Success,Fail,Forbidden,Finished,StartTime,Total),
    file:close(File),
    Pid ! {Ref,ok},
    ParentPid ! finish;
  {Pid,Ref,stop} ->
    Pid ! {Ref,ok},
    stats(Bins,Success,Fail,Forbidden,Finished,MinValue,StartTime,Running-1,
          Total,File,ParentPid)
  end.

print_output(Bins,Success,Fail,Forbidden,Finished,StartTime,Total) ->
  io:format("T\tAVG_L\t~n"),
  orddict:fold(fun(K,{Sum,Count},any) ->
    if(Count > 0) ->
      io:format("~p\t~p~n",[K,Sum/Count/?TIME_UNIT]);
    true -> ok
    end,
    any end,any,Bins),
  io:format("Success:~p\tFail:~p\tForbidden:~p\Finished:~p~n",
            [Success,Fail,Forbidden,Finished]),
  io:format("N_CLIENTS:~p~nDURATION:~p seconds ~n",
            [Total,timer:now_diff(now(),StartTime)/math:pow(10,6)]).

now_microsec({MegaSecs, Secs, MicroSecs}) ->
    (MegaSecs * 1000000 + Secs) * 1000000 + MicroSecs.

