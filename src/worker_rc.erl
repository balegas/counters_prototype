%%%-------------------------------------------------------------------
%%% @author balegas
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Nov 2013 15:37
%%%-------------------------------------------------------------------
-module(worker_rc).
-include("constants.hrl").
-author("balegas").

%% API
-export([init/2,
  init/3,
  init/4,
  decrement/2,
  increment/2,
  decrement_and_check_permissions/2,
  empty_bucket/3,
  reset_bucket/6,
  reset_bucket/7,
  reset_crdt/6,
  add_key/3,
  get_crdt/2,
  get_value/2,
  merge_crdt/3,
  transfer_permissions/4
]).
-include("worker.hrl").

-type worker() :: #worker{}. %% The record/type containing the entire Riak object.
-export_type([worker/0]).

init(Bucket, Id) ->
  init(?DEFAULT_RIAK_ADDRESS, ?DEFAULT_PB_PORT, Bucket, Id).

init(Address, Bucket, Id) ->
  init(Address,?DEFAULT_PB_PORT, Bucket,Id).

init(Address, Port, Bucket, Id) ->
  {ok, Pid} = riakc_pb_socket:start_link(Address, Port),
  #worker{id = Id, lnk = Pid, bucket = Bucket, port = Port}.

empty_bucket(Bucket, Address, Port) ->
  {ok, Pid} = riakc_pb_socket:start_link(Address, Port),
  {ok, Keys} = riakc_pb_socket:list_keys(Pid,Bucket),
  lists:foreach(fun(Key)-> riakc_pb_socket:delete(Pid,Bucket,Key) end, Keys).

reset_bucket(random, NKeys, MaxInitValue, Bucket, RiakAddress, RiakPort, AddressesIds) ->
  lists:foreach(fun(Key)-> reset_crdt(random:uniform(MaxInitValue),Bucket,integer_to_binary(Key),RiakAddress,RiakPort,AddressesIds) end,
    lists:seq(0,NKeys)).

reset_bucket(NKeys,InitValue,Bucket, RiakAddress, RiakPort, AddressesIds) ->
  lists:foreach(fun(Key)-> reset_crdt(InitValue,Bucket,integer_to_binary(Key),RiakAddress,RiakPort,AddressesIds) end,
    lists:seq(0,NKeys)).

reset_crdt(InitValue, Bucket, Key, RiakAddress, RiakPort, [ {Id,_Address} | Remote]) ->
  Counter = nncounter:new(Id,InitValue),
  PartitionedCounter = lists:foldl(fun({OtherId,_},InCounter) ->
    {ok, OutCounter} = nncounter:transfer(Id,OtherId,InitValue div (length(Remote)+1),InCounter),
    OutCounter end, Counter, Remote),
  reset_crdt(PartitionedCounter, Bucket, Key, RiakAddress, RiakPort).

reset_crdt(Object, Bucket, Key, Address, Port) ->
  {ok, Pid} = riakc_pb_socket:start_link(Address, Port),
  Result = riakc_pb_socket:get(Pid, Bucket, Key,[{r,1}],?DEFAULT_TIMEOUT),
  NewObj = case Result of
             {ok, Fetched} ->
               riakc_obj:update_value(Fetched, nncounter:to_binary(Object));
             {error,notfound} ->
               riakc_obj:new(Bucket, Key, nncounter:to_binary(Object))
           end,
  riakc_pb_socket:put(Pid, NewObj,[{w,?REPLICATION_FACTOR},return_body],?DEFAULT_TIMEOUT).

add_key(Worker,Key,CRDT) ->
  NewObj = riakc_obj:new(Worker#worker.bucket, Key, nncounter:to_binary(CRDT)),
  riakc_pb_socket:put(Worker#worker.lnk, NewObj,[{w,?REPLICATION_FACTOR}],?DEFAULT_TIMEOUT).

decrement(Worker, Key) ->
  {ok, Fetched} = riakc_pb_socket:get(Worker#worker.lnk,Worker#worker.bucket, Key,[],?DEFAULT_TIMEOUT),
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

increment(Worker, Key) ->
  {ok, Fetched} = riakc_pb_socket:get(Worker#worker.lnk,Worker#worker.bucket, Key,[],?DEFAULT_TIMEOUT),
  CRDT = nncounter:from_binary(riakc_obj:get_value(Fetched)),
  {ok,New_CRDT} = nncounter:increment(Worker#worker.id,1,CRDT),
  PutResult = riakc_pb_socket:put(Worker#worker.lnk,
  riakc_obj:update_value(Fetched,nncounter:to_binary(New_CRDT)),[{w,?REPLICATION_FACTOR}],?DEFAULT_TIMEOUT),
  case PutResult of
    ok ->
      {ok,nncounter:value(New_CRDT),nncounter:localPermissions(Worker#worker.id,New_CRDT)};
      {error, _} -> fail;
      _ -> fail
  end.

decrement_and_check_permissions(Worker, Key) ->
  {ok, Fetched} = riakc_pb_socket:get(Worker#worker.lnk,Worker#worker.bucket, Key,[],?DEFAULT_TIMEOUT),
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
              case nncounter:manage_permissions(nncounter:below_threshold(),[?PERMISSIONS_THRESHOLD,Worker#worker.id],
                nncounter:higher_permissions(),[],CRDT) of
                nil -> {ok,nncounter:value(New_CRDT), nncounter:localPermissions(Worker#worker.id,New_CRDT)};
                TargetId when TargetId /= Worker#worker.id ->
                  {request,TargetId,nncounter:value(New_CRDT), nncounter:localPermissions(Worker#worker.id,New_CRDT)};
                _ -> {ok,nncounter:value(New_CRDT),nncounter:localPermissions(Worker#worker.id,New_CRDT)}
              end;
            {error, _} -> fail;
            _ -> fail
          end;
        forbidden ->
          case nncounter:manage_permissions(nncounter:below_threshold(),[?PERMISSIONS_THRESHOLD,Worker#worker.id],
            nncounter:higher_permissions(),[],CRDT) of
            nil -> {ok,nncounter:value(CRDT), nncounter:localPermissions(Worker#worker.id,CRDT)};
            TargetId when TargetId /= Worker#worker.id ->
              {forbidden,TargetId,nncounter:value(CRDT)};
            _ -> {forbidden,nncounter:value(CRDT)}
          end
      end;
    false -> {finished,Int}
  end.

get_crdt(Worker,Key) ->
  {ok, Fetched} = riakc_pb_socket:get(Worker#worker.lnk,Worker#worker.bucket, Key,[],?DEFAULT_TIMEOUT),
  CRDT = nncounter:from_binary(riakc_obj:get_value(Fetched)),
  CRDT.


get_value(Worker,Key) ->
  CRDT = worker_rc:get_crdt(Worker,Key),
  nncounter:value(CRDT).

merge_crdt(Worker,Key,CRDT) ->
  case riakc_pb_socket:get(Worker#worker.lnk,Worker#worker.bucket, Key,[],?DEFAULT_TIMEOUT) of
    {ok, Fetched} -> LocalCRDT = nncounter:from_binary(riakc_obj:get_value(Fetched)),
      Merged = nncounter:merge(LocalCRDT,CRDT),
     UpdObj = riakc_obj:update_value(Fetched,nncounter:to_binary(Merged)),
      riakc_pb_socket:put(Worker#worker.lnk,UpdObj,[{w,?REPLICATION_FACTOR},return_body],?DEFAULT_TIMEOUT);
    {error, _} -> notfound
end.

transfer_permissions(Key,To,Worker,TransferPolicy)->
  {ok, Fetched} = riakc_pb_socket:get(Worker#worker.lnk,Worker#worker.bucket, Key,[],?DEFAULT_TIMEOUT),
  LocalCRDT = nncounter:from_binary(riakc_obj:get_value(Fetched)),
  Permissions = TransferPolicy([Worker#worker.id],LocalCRDT),
  io:format("Will transfer ~p permissions.",[Permissions]),
  {ok,UpdatedCRDT} = nncounter:transfer(Worker#worker.id,To,Permissions,LocalCRDT),
  UpdObj = riakc_obj:update_value(Fetched,nncounter:to_binary(UpdatedCRDT)),
  riakc_pb_socket:put(Worker#worker.lnk,UpdObj,[{w,?REPLICATION_FACTOR}],?DEFAULT_TIMEOUT),
  UpdatedCRDT.





