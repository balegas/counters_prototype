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
-export([stats/8, start/0]).

-include("constants.hrl").

start() ->
  {Day,Time} = calendar:local_time(),
  Filename = lists:map(fun(X) -> integer_to_list(X) end, lists:flatten([tuple_to_list(Day),tuple_to_list(Time)])),
  {ok, File} = file:open(Filename , [read, write]),
  spawn(client_stats, stats, [orddict:new(),0,0,infinite,now(),0,0,File]).



stats(Bins,Success,Fail,MinValue,StartTime,Running,Total,File) ->
receive
  Raw ={_Pid,Value, Latency, Timestamp, Status} ->
    case Status of
      success ->
        io:fwrite(File, "~p~n", [Raw]),
        NewBins = orddict:update(timer:now_diff(Timestamp,StartTime) div ?PLOT_INTERVAL,
          fun({Sum,Count}) -> {Sum+Latency,Count+1} end, {0,0}, Bins),
        stats(NewBins,Success+1,Fail, min(Value,MinValue),StartTime,Running,Total,File);
      failure ->
        io:fwrite(File, "~p~n", [Raw]),
        stats(Bins,Success,Fail+1, min(Value,MinValue),StartTime,Running,Total,File)
    end;
  start ->
    io:format("Started Stats~n"),
    stats(Bins,Success,Fail,MinValue,StartTime,Running+1,Total+1,File);
  stop when Running == 1 ->
    io:format("Stopped Stats----~n"),
    print_output(Bins,Success,Fail,StartTime,Total),
    file:close(File);
  stop ->
    io:format("Stopped Stats~n"),
    stats(Bins,Success,Fail, MinValue,StartTime,Running-1,Total,File)
  end.

print_output(Bins,Success,Fail,StartTime,Total) ->
  io:format("T\tAVG_L\t~n"),
  orddict:fold(fun(K,{Sum,Count},any) ->
    io:format("~p\t~p~n",[K,Sum/Count/?TIME_UNIT]),any end,any,Bins),
  io:format("Success:~p\tFail:~p\t~n",[Success,Fail]),
  io:format("N_CLIENTS:~p\tSTART_TIME:~p\tEND_TIME:~p\t~n",[Total,StartTime,now()]).
