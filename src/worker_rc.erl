%%%-------------------------------------------------------------------
%%% @author balegas
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2013 15:37
%%%-------------------------------------------------------------------
-module(worker_rc).
-author("balegas").

%% API
-export([init/3, update_value_crdt/1, reset_crdt/4, get_value/1]).

-record(worker, {id :: term(), lnk:: term() , bucket :: binary() | {binary(),binary()}}).

-include("constants.hrl").

-type worker() :: #worker{}. %% The record/type containing the entire Riak object.
-export_type([worker/0]).

init(Address, Bucket,Id) ->
  {ok, Pid} = riakc_pb_socket:start_link(Address, 8087),
  #worker{id = Id, lnk = Pid, bucket = Bucket}.

% Update and reset function for CRDT version

reset_crdt(InitValue, Bucket,Id, All=[ _ | Remote]) ->
	Counter = nncounter:new(Id,InitValue),
	PartitionedCounter = lists:foldl(fun({OtherId,_},InCounter) -> 
		{ok, OutCounter} = nncounter:transfer(Id,OtherId,InitValue div (length(Remote)+1),InCounter),
		OutCounter end, Counter, Remote),
	lists:foldl(fun({_,Address},_)-> reset_crdt(PartitionedCounter, Address, Bucket) end,
		PartitionedCounter,All).

reset_crdt(Object, Address, Bucket) ->
	{ok, Pid} = riakc_pb_socket:start_link(Address, 8087),
    Result = riakc_pb_socket:get(Pid, Bucket, ?KEY,[{r,1}],?DEFAULT_TIMEOUT),
    NewObj = case Result of
               {ok, Fetched} ->
                 riakc_obj:update_value(Fetched, nncounter:to_binary(Object));
               {error,notfound} ->
                 riakc_obj:new(Bucket, ?KEY, nncounter:to_binary(Object))
             end,
    riakc_pb_socket:put(Pid, NewObj,[{w,?REPLICATION_FACTOR},return_body],?DEFAULT_TIMEOUT).

update_value_crdt(Worker) ->
  {ok, Fetched} = riakc_pb_socket:get(Worker#worker.lnk,Worker#worker.bucket, ?KEY,[],?DEFAULT_TIMEOUT),
  CRDT = nncounter:from_binary(riakc_obj:get_value(Fetched)),
  Int = nncounter:value(CRDT),
  case Int > 0 of
	  true ->  
	  	case nncounter:decrement(Worker#worker.id,1,CRDT) of
			{ok,New_CRDT} ->
			  PutResult = riakc_pb_socket:put(Worker#worker.lnk,
          riakc_obj:update_value(Fetched,nncounter:to_binary(New_CRDT)),[{w,?REPLICATION_FACTOR}],?DEFAULT_TIMEOUT),
			  case PutResult of
				  ok ->
					{ok,nncounter:value(New_CRDT)};
					{error, _} -> fail;
				  _ -> fail
				end;
				%% Stops when no permissions are available
			forbidden -> {forbidden,CRDT}
			end;
		false -> {finished,Int}
   end.
   
get_value(Worker) ->
    {ok, Fetched} = riakc_pb_socket:get(Worker#worker.lnk,Worker#worker.bucket, ?KEY,[],?DEFAULT_TIMEOUT),
    CRDT = nncounter:from_binary(riakc_obj:get_value(Fetched)),
    nncounter:value(CRDT).
