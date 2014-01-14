-module(crdtdb_vnode).
-behaviour(riak_core_vnode).
-include("crdtdb.hrl").
-include("worker.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3,
         merge_remote/2,
         asynchronous_request_mode/0,
         do_merge/1
]).

-ignore_xref([
             start_vnode/1, merge_remote/2
             ]).

-include("constants.hrl").
-include("state.hrl").

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->

  %Dumb local testing
  Worker = case node() of
      'crdtdb1@127.0.0.1' -> worker_rc:init("127.0.0.1",10017,?BUCKET,node());
      'crdtdb2@127.0.0.1' -> worker_rc:init("127.0.0.1",10027,?BUCKET,node());
      _ -> worker_rc:init(?BUCKET,node())
  end,
  State = #state {partition=Partition, worker=Worker, sync_timer = nil, synch_pid = nil,
  last_permission_request=orddict:new(),  request_mode = crdtdb_vnode:asynchronous_request_mode(),
    transfer_policy = nncounter:half_permissions(), ids_addresses = orddict:new()},
  {ok, State}.

%%Normalize addresses
handle_command({start, NumKeys, InitValue, AddressesIds}, _Sender, State) ->
  {Address,Port} = case node() of
    'crdtdb1@127.0.0.1' -> {"127.0.0.1",10017};
    'crdtdb2@127.0.0.1' -> {"127.0.0.1",10027};
    _ ->   {?DEFAULT_RIAK_ADDRESS, ?DEFAULT_PB_PORT}
  end,
  worker_rc:empty_bucket(?BUCKET,Address,Port),
  worker_rc:reset_bucket(NumKeys,InitValue, ?BUCKET, Address, Port, AddressesIds),
  {reply, ok, State};

handle_command({start, Addresses},_Sender,State) ->
  DictAddresses = lists:foldl(fun({Id,Address},Dict) ->
    orddict:store(Id,Address,Dict) end,State#state.ids_addresses,Addresses),
  NewState = State#state{ids_addresses = DictAddresses, sync_addresses = Addresses},
  Pid = spawn_link(crdtdb_vnode,merge_remote,[ordsets:new(),NewState]),
  {ok,Ref} = timer:apply_interval(?SYNC_INTERVAL,crdtdb_vnode,do_merge,[Pid]),
  {reply, ok, NewState#state{sync_timer=Ref, synch_pid = Pid}};

handle_command({decrement,Key}, _Sender, State) ->
  Reply = case worker_rc:decrement_and_check_permissions(State#state.worker,Key) of
    {ok,Int} -> {reply, {ok,Int}, State};
    {request,TargetId,Int} ->
      CanRequestPermissions = permissionsRequestAllowed(Key,State),
      case CanRequestPermissions of
        false ->
          {reply, {ok,Int}, State};
        _ -> (State#state.request_mode)(Key,TargetId,State),
             State1 = State#state{last_permission_request=orddict:store(Key,now(),
             State#state.last_permission_request)},
             {reply, {ok,Int}, State1}
      end;
    {forbidden,TargetId,Int} ->
      CanRequestPermissions = permissionsRequestAllowed(Key,State),
      case CanRequestPermissions of
        false ->
          {reply,{forbidden,Int},State};
        _ -> (State#state.request_mode)(Key,TargetId,State),
              State1 = State#state{last_permission_request=orddict:store(Key,now(),
              State#state.last_permission_request)},
              {reply,{forbidden,Int},State1}
      end;
    {forbidden,Int} -> {reply,{forbidden,Int},State};
    {finished,Int}  -> {reply,{finished,Int},State};
    fail -> {reply,fail,State}
  end,
  Reply;


handle_command({merge_value,Key,CRDT}, _Sender, State) ->
  case (State#state.synch_pid) of
    nil -> io:format("WARNING SYNCHRONIZER NOT ON");
    Pid ->
      Pid ! [Key],
      case worker_rc:merge_crdt(State#state.worker,Key,CRDT) of
        {ok,Merged} -> {reply, Merged, State};
        notfound ->
          worker_rc:add_key(State#state.worker,Key,CRDT),
          {reply, ok, State};
        {error,_} ->
          ?PRINT("Failed merge"),
        {reply, merge_fail, State}
      end
  end;

handle_command({track_keys,Keys}, _Sender, State) ->
  State#state.synch_pid ! Keys,
  {reply, ok, State};

handle_command({get_value,Key}, _Sender, State) ->
  Result = worker_rc:get_value(State#state.worker,Key),
  {reply, Result, State};

%Must filter stall messages
handle_command({request_permissions,Key,RequesterId},_Sender,State) ->
  io:format("~p Received request permissions", [(State#state.worker)#worker.id]),
  CRDT = worker_rc:transfer_permissions(Key,RequesterId,State#state.worker,State#state.transfer_policy),
  rpc:async_call(orddict:fetch(RequesterId,State#state.ids_addresses), crdtdb, merge_value, [Key,CRDT]),
  {noreply, State};


handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(_Message, _Sender, State) ->
    {noreply, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(_Data, State) ->
    {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    {true, State}.

delete(State) ->
    {ok, State}.

handle_coverage(_Req, _KeySpaces, _Sender, State) ->
    {stop, not_implemented, State}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

do_merge(Pid) -> Pid ! doIt.

merge_remote(Keys,State) ->
  receive
    doIt ->
      lists:foreach(fun(Key) ->
        Object = worker_rc:get_crdt(State#state.worker,Key),
        lists:foreach(fun({_Id,Address}) ->
          if Address /= node() -> rpc:call(Address, crdtdb, merge_value, [Key,Object]);
          true -> ok end end, State#state.sync_addresses)
      end,ordsets:to_list(Keys)),
      merge_remote(Keys,State);
    terminate -> ok;
    NewKeys ->
      merge_remote(lists:foldl(fun(K,Set) -> ordsets:add_element(K,Set)end,Keys,NewKeys),State)
  end.

permissionsRequestAllowed(Key,State) ->
  Found = orddict:find(Key,State#state.last_permission_request),
  case Found of
    {ok,Last} ->
      Diff = timer:now_diff(now(),Last),
      case Diff > (?PERMISSIONS_DELAY) of
        true -> true;
        false -> false
      end;
    error -> notFound
  end.



%% ===================================================================
%% Permissions request protocols
%% ===================================================================

%Avoid repeating requests
asynchronous_request_mode() -> fun(Key,TargetId,State) ->
  Target = orddict:fetch(TargetId,State#state.ids_addresses),
  io:format("~p sent request permissions to ~p", [(State#state.worker)#worker.id,Target]),
  rpc:async_call(Target, crdtdb, request_permissions, [Key,(State#state.worker)#worker.id])
end.

