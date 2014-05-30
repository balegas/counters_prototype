%%%-------------------------------------------------------------------
%%% @author balegas
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2013 15:37
%%%-------------------------------------------------------------------
-module(worker_counter).
-include("constants.hrl").
-author("balegas").

%% API
-export( [
          init/3,
          decrement/2,
          increment/2,
          reset_bucket/5,
          reset_bucket/6,
          reset_crdt/5,
          get_value/2
         ]).
-include("worker.hrl").

-type worker() :: #worker{}.
-export_type([worker/0]).

init(Address, Port, Bucket) ->
    {ok, Pid} = riakc_pb_socket:start_link(Address, Port),
    #worker{lnk = Pid, bucket = Bucket, port = Port}.

reset_bucket(random, NKeys, MaxInitValue, Bucket, RiakAddress, RiakPort) ->
    lists:foreach(
      fun(Key) -> reset_crdt(
                    random:uniform(MaxInitValue),
                    Bucket,integer_to_binary(Key),RiakAddress,RiakPort)
      end,
      lists:seq(0,NKeys)).

reset_bucket(NKeys,InitValue,Bucket, RiakAddress, RiakPort) ->
    lists:foreach(
      fun(Key) -> reset_crdt(InitValue,Bucket,
                             integer_to_binary(Key),RiakAddress,RiakPort)
      end,
      lists:seq(0,NKeys)).

reset_crdt(InitValue, Bucket, Key, RiakAddress, RiakPort) ->
    {ok, Pid} = riakc_pb_socket:start_link(RiakAddress, RiakPort),
    Result = riakc_pb_socket:counter_val(Pid,Bucket, Key,[{r,1}]),
    case Result of
        {ok, Value} ->
            riakc_pb_socket:counter_incr(Pid,Bucket, Key,(InitValue-Value),
                                         [{w,?REPLICATION_FACTOR}]);
        {error,_} ->
            riakc_pb_socket:counter_incr(Pid,Bucket, Key,InitValue,[{w,1}])
    end.

decrement(Worker, Key) ->
    Result = riakc_pb_socket:counter_incr(
               Worker#worker.lnk, Worker#worker.bucket, Key,  -1, 
               [{w,?REPLICATION_FACTOR}]),
    case Result of
        ok -> ok;
        {error,_} -> fail
    end.

increment(Worker, Key) ->
    Result = riakc_pb_socket:counter_incr(
               Worker#worker.lnk, Worker#worker.bucket, Key, 1,
               [{w,?REPLICATION_FACTOR}]),
    case Result of
        ok ->
            riakc_pb_socket:counter_val(Worker#worker.lnk, Worker#worker.bucket,
                                        Key, [{r,1}]);
        {error,_} -> fail
    end.

get_value(Worker,Key) ->
    {ok,Value} = riakc_pb_socket:counter_val(
                   Worker#worker.lnk, Worker#worker.bucket, Key, [{r,1}]),
    Value.
