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
          init/4,
          decrement/3,
          increment/3,
          reset/7
         ]).
-include("worker.hrl").

-type worker() :: #worker{}.
-export_type([worker/0]).

init(Address, Port, Bucket, Id) ->
    {ok, Pid} = riakc_pb_socket:start_link(Address, Port),
    #worker{lnk = Pid, bucket = Bucket, port = Port, id = Id}.

decrement(nncounter, Worker, Key) -> decrement_nncounter(Worker, Key);
decrement(strong, Worker, Key) -> increment_strong(Worker, Key, -1);
decrement(causal, Worker, Key) -> increment_causal(Worker, Key, -1);
decrement(weak, Worker, Key) -> inc_counter(Worker#worker.lnk, Worker#worker.bucket, Key, -1).

increment(nncounter, Worker, Key) -> increment_nncounter(Worker, Key);
increment(strong, Worker, Key) -> increment_strong(Worker, Key, 1);
increment(causal, Worker, Key) -> increment_causal(Worker, Key, 1);
increment(weak, Worker, Key) -> inc_counter(Worker#worker.lnk, Worker#worker.bucket, Key, 1).

increment_nncounter(Worker, Key) ->
	Result = get_obj(Worker#worker.lnk,Worker#worker.bucket,Key),
    case Result of
        {ok, Obj} ->
	  		Counter = nncounter:from_binary(riakc_obj:get_value(Obj)),
	  		{ok,NewCounter} = nncounter:increment(Worker#worker.id,1,Counter),
	  		put_nncounter(Worker#worker.lnk,Obj,NewCounter);
        fail -> {failure,0}
    end.

decrement_nncounter(Worker, Key) ->
	Get = get_obj(Worker#worker.lnk,Worker#worker.bucket,Key),
    case Get of
        {ok, Obj} ->
	  		Counter = nncounter:from_binary(riakc_obj:get_value(Obj)),
	  		Dec = nncounter:decrement(Worker#worker.id,1,Counter),
	  		case Dec of
	  			{ok,NewCounter} -> put_nncounter(Worker#worker.lnk,Obj,NewCounter);
  				forbidden -> {forbidden,nncounter:value(Counter)}
	  		end;
        fail -> {failure,0}
    end.

put_nncounter(Pid,Obj,Counter) ->
	NewObj = riakc_obj:update_value(Obj,nncounter:to_binary(Counter)),
	case riakc_pb_socket:put(Pid,NewObj,[{w,?REPLICATION_FACTOR}]) of
		ok -> {success,nncounter:value(Counter)};
		{error,_} -> {failure,0}
	end.

increment_strong(Worker, Key, Amount) ->
	Result = get_obj(Worker#worker.lnk,Worker#worker.bucket,Key),
    case Result of
        {ok, Obj} -> inc_obj(Worker#worker.lnk,Obj,binary_to_integer(riakc_obj:get_value(Obj)),Amount);
        fail -> {failure,0}
    end.
    
get_obj(Pid,Bucket,Key) -> 
	Result = riakc_pb_socket:get(Pid,Bucket,Key),
	case Result of
		{ok, Obj} -> {ok, Obj};
		{error,_} -> fail
	end.
	
inc_obj(Pid,Obj,Value,Amount) ->
	if	Value < -Amount -> {forbidden,Value};
		Value >= -Amount ->
			NewValue = Value + Amount,
			NewObj = riakc_obj:update_value(Obj,integer_to_binary(NewValue)),
			case riakc_pb_socket:put(Pid,NewObj,[{w,?REPLICATION_FACTOR}]) of
				ok -> {success,NewValue};
				{error,_} -> {failure,0}
			end
	end.

increment_causal(Worker, Key, Amount) ->
	Result = get_crdt(Worker#worker.lnk,Worker#worker.bucket,Key),
    case Result of
        {ok, Counter} -> inc_crdt(Worker#worker.lnk,Worker#worker.bucket,Key,Counter,Amount);
        fail -> {failure,0}
    end.

get_crdt(Pid,Bucket,Key) -> 
	Result = riakc_pb_socket:fetch_type(Pid,Bucket,Key,[{r,1}]),
	case Result of
		{ok, Counter} -> {ok, Counter};
		{error,_} -> fail
	end.
	
inc_crdt(Pid,Bucket,Key,Counter,Amount) ->
	case riakc_counter:value(Counter) of
		OldValue when OldValue < -Amount -> {forbidden,OldValue};
		OldValue when OldValue >= -Amount ->
			NewCounter = riakc_counter:increment(Amount, Counter),
			Result = riakc_pb_socket:update_type(Pid,Bucket,Key,riakc_counter:to_op(NewCounter),
												[{w,?REPLICATION_FACTOR}]),
			case Result of
				ok -> {success,OldValue + Amount};
				{error,_} -> {failure,0}
			end
	end.
	
get_counter(Pid,Bucket,Key) -> 
	Result = riakc_pb_socket:counter_val(Pid,Bucket,Key,[{r,1}]),
	case Result of
		{ok, Value} -> {ok, Value};
		{error,_} -> fail
	end.
	
inc_counter(Pid,{_Type,Bucket},Key,Amount) ->
	case get_counter(Pid,Bucket,Key) of
		{ok,OldValue} when OldValue < -Amount -> {forbidden,OldValue};
		{ok,OldValue} when OldValue >= -Amount ->
			Result = riakc_pb_socket:counter_incr(Pid,Bucket,Key,(Amount),
													[{w,?REPLICATION_FACTOR}]),
			case Result of
				ok -> {success,OldValue + Amount};
				{error,_} -> {failure,0}
			end;
		fail -> {failure,0}
	end.
	
new_counter(InitValue, Ids) ->
	N = length(Ids),
	[Id | OtherIds] = Ids,
	Counter = nncounter:new(Id,InitValue),
  	lists:foldl(fun(OtherId,InCounter) ->
	    {ok, OutCounter} = nncounter:transfer(Id,OtherId,InitValue div N,InCounter),
	    OutCounter end, Counter, OtherIds).

reset(nncounter, InitValue, Bucket, Key, RiakAddress, RiakPort, Ids) ->
    {ok, Pid} = riakc_pb_socket:start_link(RiakAddress, RiakPort),
    Obj = case get_obj(Pid,Bucket,Key) of
		{ok, OldObj} -> OldObj;
		fail -> riakc_obj:new(Bucket,Key)
    end,
    Counter = new_counter(InitValue,Ids),
    put_nncounter(Pid,Obj,Counter);

reset(strong, InitValue, Bucket, Key, RiakAddress, RiakPort, _Ids) ->
    {ok, Pid} = riakc_pb_socket:start_link(RiakAddress, RiakPort),
    Obj = case get_obj(Pid,Bucket,Key) of
		{ok, OldObj} -> OldObj;
		fail -> riakc_obj:new(Bucket,Key)
    end,
    inc_obj(Pid,Obj,0,InitValue);
    
reset(causal, InitValue, Bucket, Key, RiakAddress, RiakPort, _Ids) ->
    {ok, Pid} = riakc_pb_socket:start_link(RiakAddress, RiakPort),
    Counter = case get_crdt(Pid,Bucket,Key) of
    	{ok, OldCounter} -> OldCounter;
    	fail -> riakc_counter:new()
    end,
    Value = riakc_counter:value(Counter),
    inc_crdt(Pid,Bucket,Key,Counter,InitValue-Value);
    
reset(weak, InitValue, {_Type,Bucket}, Key, RiakAddress, RiakPort, _Ids) ->
    {ok, Pid} = riakc_pb_socket:start_link(RiakAddress, RiakPort),
    Value = case get_counter(Pid,Bucket,Key) of
    	{ok, OldValue} -> OldValue;
    	fail -> 0
    end,
	Result = riakc_pb_socket:counter_incr(Pid,Bucket,Key,(InitValue-Value),
													[{w,?REPLICATION_FACTOR}]),
	case Result of
		ok -> {success,InitValue};
		{error,_} -> {failure,0}
	end.
