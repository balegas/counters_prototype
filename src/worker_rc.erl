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
-export([
  init/4,
  decrement/2,
  increment/2,
  empty_bucket/3,
  reset_bucket/6,
  reset_bucket/7,
  reset_crdt/6,
  add_key/3,
  get_crdt/2,
  get_value/2,
  merge_crdt/3,
  transfer_permissions/4,
  check_permissions/2
]).
-include("worker.hrl").

-type worker() :: #worker{}. %% The record/type containing the entire Riak object.
-export_type([worker/0]).

init(Address, Port, Bucket, Id) ->
  {ok, Pid} = riakc_pb_socket:start_link(Address, Port),
  #worker{id = Id, lnk = Pid, bucket = Bucket, port = Port}.

empty_bucket(Bucket, Address, Port) ->
  {ok, Pid} = riakc_pb_socket:start_link(Address, Port),
  {ok, Keys} = riakc_pb_socket:list_keys(Pid,Bucket),
  lists:foreach(fun(Key)->
    io:format("delete key ~p~n",[Key]),
    riakc_pb_socket:delete(Pid,Bucket,Key) end, Keys).

reset_bucket(random, NKeys, MaxInitValue, Bucket, RiakAddress, RiakPort, AddressesIds) ->
    Fun = 
    fun({Id, _Address}) ->
            lists:foreach(
              fun(KeySeq)->
                      Key = erlang:integer_to_list(KeySeq) ++ "_" ++ erlang:atom_to_list(Id),
                      lager:info("Node with id: ~p creates key ~p",[Id, Key]),
                      reset_crdt(random:uniform(MaxInitValue),Bucket,list_to_binary(Key),RiakAddress,RiakPort,AddressesIds) 
              end, lists:seq(1, NKeys))
    end,
    lists:foreach(Fun,AddressesIds).

reset_bucket(NKeys,InitValue,Bucket, RiakAddress, RiakPort, AddressesIds) ->
    Fst = fun({A,_}) -> A end,
    Ids = lists:usort(lists:map(Fst,AddressesIds)),
    Fun = fun(Key, Keys) -> 
      MoreKeys = lists:map(fun(Region) -> erlang:integer_to_list(Key) ++ "_" ++ erlang:atom_to_list(Region) end, Ids),
      lists:append(MoreKeys,Keys)
    end,
    Keys = lists:foldl(Fun, [], lists:seq(1, NKeys)),
    lager:info("Node creates keys ~p",[Keys]),
    reset_queue(InitValue,Bucket, RiakAddress, RiakPort, Keys, Ids).

reset_queue(_,_,_,_,[],_) -> ok;
reset_queue(InitValue,Bucket, RiakAddress, RiakPort, [Key | Keys], Ids) -> 
  reset_crdt(InitValue, Bucket, Key, RiakAddress, RiakPort, Ids),
  timer:sleep(100),
  reset_queue(InitValue,Bucket, RiakAddress, RiakPort, Keys, Ids).

reset_crdt(InitValue, Bucket, Key, RiakAddress, RiakPort, [ Id | Ids]) ->
  Counter = nncounter:new(Id,InitValue),
  PartitionedCounter = lists:foldl(fun(OtherId,InCounter) ->
    {ok, OutCounter} = nncounter:transfer(Id,OtherId,InitValue div (length(Ids)+1),InCounter),
    OutCounter end, Counter, Ids),
  reset_crdt(PartitionedCounter, Bucket, Key, RiakAddress, RiakPort).

reset_crdt(CRDT, Bucket, Key, Address, Port) ->
  Result = riakc_pb_socket:start_link(Address, Port),
  case Result of
    {ok, Pid} ->
      update_key(Pid,Bucket,list_to_binary(Key),CRDT,[{w,?REPLICATION_FACTOR},return_body]);
    {error,Error} -> 
      lager:info("Error getting ~p: ~p", [Key, Error]),
      timer:sleep(random:uniform(100)),
      reset_crdt(CRDT, Bucket, Key, Address, Port)
  end.

update_key(Pid,Bucket,Key,CRDT,Options) ->
  Bin = nncounter:to_binary(CRDT),
  update_object(Pid,Bucket,Key,Bin,Options).

update_object(Pid,Bucket,Key,Bin,Options) ->
  Obj = get_object(Pid,Bucket,Key,Bin,Options),
  Result = riakc_pb_socket:put(Pid,Obj,Options,?DEFAULT_TIMEOUT),
  case Result of
    {error,Error} -> lager:info("Error getting ~p: ~p", [Key, Error]), update_object(Pid,Bucket,Key,Bin,Options);
    Result -> Result
  end.

get_object(Pid,Bucket,Key,Bin,Options) ->
  Result = riakc_pb_socket:get(Pid, Bucket, Key, [{r,1}], ?DEFAULT_TIMEOUT),
  case Result of
    {ok,Obj} -> Obj;
    {error,notfound} -> riakc_obj:new(Bucket, Key, Bin);
    {error,Error} -> lager:info("Error getting ~p: ~p", [Key, Error]), get_object(Pid,Bucket,Key,Bin,Options)
  end.

add_key(Worker,Key,CRDT) ->
  update_key(Worker#worker.lnk,Worker#worker.bucket,Key,CRDT,[{w,?REPLICATION_FACTOR}]).

decrement(Worker, Key) ->
  {ok, Fetched} = riakc_pb_socket:get(Worker#worker.lnk,Worker#worker.bucket, Key,[{r,1}],?DEFAULT_TIMEOUT),
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
  {ok, Fetched} = riakc_pb_socket:get(Worker#worker.lnk,Worker#worker.bucket, Key,[{r,1}],?DEFAULT_TIMEOUT),
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

check_permissions(Worker,CRDT) ->
    case nncounter:value(CRDT) of
        0 -> finished;
        _ ->
      case nncounter:manage_permissions(nncounter:below_threshold(),[?PERMISSIONS_THRESHOLD,Worker#worker.id],
                                              nncounter:all_positive(),[],CRDT) of
                nil ->
                    ok;
                [] ->
                    {forbidden_not_available,nncounter:value(CRDT)};
                List = [_X | _] ->
                    LocalPermissions = nncounter:localPermissions(Worker#worker.id,CRDT),
                    if
                        LocalPermissions =:= 0 ->
                            {forbidden,List};
                        true ->
                            {request,List}
                    end;
                _ ->
                    ok
            end
    end.

get_crdt(Worker,Key) ->
  Result = riakc_pb_socket:get(Worker#worker.lnk,Worker#worker.bucket, Key,[{r,1}],?DEFAULT_TIMEOUT),
  case Result of
    {ok, Fetched} -> nncounter:from_binary(riakc_obj:get_value(Fetched));
    {error, Error} -> 
      lager:info("Error getting key ~p: ~p", [Key, Error]),
      get_crdt(Worker,Key)
  end.

get_value(Worker,Key) ->
  CRDT = worker_rc:get_crdt(Worker,Key),
  nncounter:value(CRDT).

merge_crdt(Worker,Key,CRDT) ->
  case riakc_pb_socket:get(Worker#worker.lnk,Worker#worker.bucket, Key,[{r,1}],?DEFAULT_TIMEOUT) of
    {ok, Fetched} -> LocalCRDT = nncounter:from_binary(riakc_obj:get_value(Fetched)),
      Merged = nncounter:merge(LocalCRDT,CRDT),
      UpdObj = riakc_obj:update_value(Fetched,nncounter:to_binary(Merged)),
      PutResult = riakc_pb_socket:put(Worker#worker.lnk,UpdObj,[{w,?REPLICATION_FACTOR}],?DEFAULT_TIMEOUT),
      case PutResult of
                ok -> Merged;
                %I'm intentionally not treating this message, because i expect it does not match ever!
                {error, Error} ->
                    lager:info("Error Writing Key ~p: ~p~n",[Key, Error]),
                    merge_crdt(Worker,Key,CRDT)
            end;
    {error, Error} -> lager:info("Error Reading Key ~p: ~p~n",[Key, Error]), Error
end.

%TODO: Check that transferred
transfer_permissions(Key,To,Worker,TransferPolicy)->
    {ok, Fetched} = riakc_pb_socket:get(Worker#worker.lnk,Worker#worker.bucket, Key,[],?DEFAULT_TIMEOUT),
    LocalCRDT = nncounter:from_binary(riakc_obj:get_value(Fetched)),
    Permissions = TransferPolicy([Worker#worker.id],LocalCRDT),
    case Permissions > 0 of
        true ->
            {ok,UpdatedCRDT} = nncounter:transfer(Worker#worker.id,To,Permissions,LocalCRDT),
            UpdObj = riakc_obj:update_value(Fetched,nncounter:to_binary(UpdatedCRDT)),
            PutResult = riakc_pb_socket:put(Worker#worker.lnk,UpdObj,[{w,?REPLICATION_FACTOR}],?DEFAULT_TIMEOUT),
            case PutResult of
                ok -> {transferred,UpdatedCRDT};
                {error,_} -> {fail, LocalCRDT}
            end;
        false -> {not_allowed, LocalCRDT}
    end.





