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
         synchronize_remote/3,
         asynchronous_request_policy/0
]).

-ignore_xref([
             start_vnode/1, synchonize_remote/3
             ]).

-include("constants.hrl").
-record(state, {partition, worker, sync_timer, ids_addresses, request_policy, transfer_policy}).

%% WARNING -- It is necessary to shutdown the node after each experiment to stop the periodic task

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->

  %Dumb local testing
  Worker = case node() of
      'crdtdb1@127.0.0.1' -> worker_rc:init("127.0.0.1",10017,?BUCKET,node());
      'crdtdb2@127.0.0.1' -> worker_rc:init("127.0.0.1",10027,?BUCKET,node());
      _ ->   worker_rc:init(?BUCKET,node())
  end,
  {ok, #state {partition=Partition, worker=Worker, sync_timer = nil,
    request_policy = crdtdb_vnode:asynchronous_request_policy(), transfer_policy = nncounter:half_permissions(), ids_addresses = orddict:new()}}.

%%Normalize addresses
handle_command({reset, NumKeys, InitValue, AddressesIds}, _Sender, State) ->

  {Address,Port} = case node() of
    'crdtdb1@127.0.0.1' -> {"127.0.0.1",10017};
    'crdtdb2@127.0.0.1' -> {"127.0.0.1",10027};
    _ ->   {?DEFAULT_RIAK_ADDRESS, ?DEFAULT_PB_PORT}
  end,

  worker_rc:empty_bucket(?BUCKET,Address, Port),
  worker_rc:reset_bucket(NumKeys,InitValue, ?BUCKET, Address, Port, AddressesIds),
  {reply, ok, State};

handle_command({decrement,Key}, _Sender, State) ->
  Result = worker_rc:update_value_crdt(State#state.worker,Key),
  case nncounter:manage_permissions(nncounter:below_threshold(),[?PERMISSIONS_THRESHOLD,State#state.worker#worker.id],nncounter:higher_permissions(),[]) of
    nil -> io:format("Enough Permissions ~p",[State#state.worker#worker.id]), ok;
    TargetId when TargetId /= State#state.worker#worker.id ->
      io:format("~p Will request permissions to ~p",[State#state.worker#worker.id,TargetId]),
      (State#state.request_policy)(orddict:fetch(TargetId,State#state.ids_addresses),State);
    _ -> io:format("Local client has the highest number of permissions")
  end,
  {reply, Result, State};

handle_command({synchronize,Keys,Addresses}, _Sender, State) ->
  if State#state.sync_timer /= nil ->
    timer:cancel(State#state.sync_timer);
    true -> false
  end,
  State1 = State#state{ids_addresses = lists:foldl(fun({Id,Address},Dict) -> orddict:store(Id,Address,Dict) end,State#state.ids_addresses,Addresses)},
  {ok,Ref} = timer:apply_interval(?SYNC_INTERVAL,crdtdb_vnode, synchronize_remote, [Keys,Addresses,State]),
  State2 = State1#state{sync_timer=Ref},
  {reply, ok, State2};

handle_command({get_value,Key}, _Sender, State) ->
  Result = worker_rc:get_value(State#state.worker,Key),
  {reply, Result, State};

handle_command({merge_value,Key,CRDT}, _Sender, State) ->
  case worker_rc:merge_crdt(State#state.worker,Key,CRDT) of
    {ok,Merged} -> {reply, Merged, State};
    {error,_} ->
      ?PRINT("Failed merge"),
      {reply, merge_fail, State}
  end;

%Must filter stall messages
%%Could have different reply policies
handle_command({request_permissions,Key,RequesterId},_Sender,State) ->
  CRDT = worker_rc:transfer_permissions(Key,RequesterId,State#state.worker,State#state.transfer_policy),
  rpc:async_call(orrdict:fetch(RequesterId,State#state.ids_addresses), crdtdb, merge_value, [Key,CRDT]),
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

synchronize_remote(Keys,Addresses,State) ->
  lists:foreach(fun(Key) ->
    Object = worker_rc:get_crdt(State#state.worker,Key),
    lists:foreach(fun({_Id,Address}) -> rpc:call(Address, crdtdb, merge_value, [Key,Object]) end, Addresses)
  end,Keys).

%Avoid repeating requests
asynchronous_request_policy() -> fun(Target,State) ->
  io:format("~p sent request permissions to ~p", [(State#state.worker)#worker.id,Target]),
  rpc:async_call(Target, crdtdb, request_permissions, [(State#state.worker)#worker.id])
end.