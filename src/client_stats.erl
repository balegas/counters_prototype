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
-export([stats/8, start/1]).

-include("constants.hrl").

start(Suffix) ->
  {Day,Time} = calendar:local_time(),
  DayS = lists:concat(lists:concat(lists:map(fun(X)->[X,"-"] end, erlang:tuple_to_list(Day)))),
  TimeS = lists:concat(lists:concat(lists:map(fun(X)->[X,"-"] end, erlang:tuple_to_list(Time)))),
  Filename = lists:concat([DayS, TimeS, Suffix]),
  {ok, File} = file:open(Filename , [read, write]),
  spawn(client_stats, stats, [orddict:new(),0,0,infinite,now(),0,0,File]).



stats(Bins,Success,Fail,MinValue,StartTime,Running,Total,File) ->
receive
  %Raw ={Pid,Value, Latency, Timestamp, Status} ->
  {Pid,Value, Latency, Timestamp, Status} ->
    case Status of
      success ->
        io:fwrite(File, "~p\t~p\t~p\t~p\t~p\t~n", [Pid,Value, Latency, Timestamp, Status]),
        NewBins = orddict:update(timer:now_diff(Timestamp,StartTime) div ?PLOT_INTERVAL,
          fun({Sum,Count}) -> {Sum+Latency,Count+1} end, {0,0}, Bins),
        stats(NewBins,Success+1,Fail, min(Value,MinValue),StartTime,Running,Total,File);
      failure ->
        io:fwrite(File, "~p\t~p\t~p\t~p\t~p\t~n", [Pid,Value, Latency, Timestamp, Status]),
        stats(Bins,Success,Fail+1, min(Value,MinValue),StartTime,Running,Total,File)
    end;
  start ->
    stats(Bins,Success,Fail,MinValue,StartTime,Running+1,Total+1,File);
  stop when Running == 1 ->
    print_output(Bins,Success,Fail,StartTime,Total),
    file:close(File);
  stop ->
    stats(Bins,Success,Fail, MinValue,StartTime,Running-1,Total,File)
  end.

print_output(Bins,Success,Fail,StartTime,Total) ->
  io:format("T\tAVG_L\t~n"),
  orddict:fold(fun(K,{Sum,Count},any) ->
    io:format("~p\t~p~n",[K,Sum/Count/?TIME_UNIT]),any end,any,Bins),
  io:format("Success:~p\tFail:~p\t~n",[Success,Fail]),
  io:format("N_CLIENTS:~p\tSTART_TIME:~p\tEND_TIME:~p\t~n",[Total,StartTime,now()]).
