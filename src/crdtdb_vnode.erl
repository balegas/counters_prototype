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
         exhaustive_request_mode/0,
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
  State = #state {partition=Partition, sync_timer = nil, synch_pid = nil, worker = nil, cache = orddict:new(),
  last_permission_request=orddict:new(), transfer_policy = nncounter:half_permissions(), ids_addresses = orddict:new(), port = app_helper:get_env(riak_core, pb_port,?DEFAULT_PB_PORT)
  },
  {ok, State}.

%%Normalize addresses
handle_command({reset, NumKeys, InitValue, AddressesIds}, _Sender, State) ->
  {Address,Port} = {?DEFAULT_RIAK_ADDRESS, State#state.port},
  worker_rc:empty_bucket(?BUCKET,Address,Port),
  worker_rc:reset_bucket(NumKeys,InitValue, ?BUCKET, Address, Port, AddressesIds),
  {reply, ok, State};

%%Normalize addresses
handle_command({reset, random, NumKeys, InitValue, AddressesIds}, _Sender, State) ->
  {Address,Port} = {?DEFAULT_RIAK_ADDRESS, State#state.port},
  worker_rc:empty_bucket(?BUCKET,Address,Port),
  worker_rc:reset_bucket(random,NumKeys,InitValue, ?BUCKET, Address, Port, AddressesIds),
  {reply, ok, State};


handle_command({start, Region, Addresses},_Sender,State) ->
  DictAddresses = lists:foldl(fun({Id,Address},Dict) ->
    orddict:store(Id,Address,Dict) end,State#state.ids_addresses,Addresses),
  Worker = worker_rc:init(?DEFAULT_RIAK_ADDRESS,State#state.port,?BUCKET,Region),
  NewState = State#state{worker = Worker, ids_addresses = DictAddresses, sync_addresses = Addresses},
  case State#state.synch_pid /= nil of
    true -> State#state.synch_pid ! terminate,
      timer:cancel(State#state.sync_timer);
    false -> ok
  end,
  Pid = spawn_link(crdtdb_vnode,merge_remote,[ordsets:new(),NewState]),
  {ok,Ref} = timer:apply_interval(?SYNC_INTERVAL,crdtdb_vnode,do_merge,[Pid]),
  Pid ! doIt,
  {reply, ok, NewState#state{sync_timer=Ref, synch_pid = Pid}};

handle_command({increment,Key}, _Sender, State) ->
  {ok,CRDT,ModifiedState} = cache_increment(Key,State),
  {reply, nncounter:value(CRDT), ModifiedState};

handle_command({decrement,Key}, _Sender, State) ->
  {ok,CRDT} = cache_get_value(Key,State),
  CanRequestPermissions = permissionsRequestAllowed(Key,State),
  Result = worker_rc:check_permissions(State#state.worker,CRDT),
  case Result of
    finished ->
      {reply,{finished,0},State};
    ok ->
      {ok,Counter,LastState} = cache_decrement(Key,State),
        Val=nncounter:value(Counter),
        Per=nncounter:localPermissions(State#state.worker#worker.id,Counter),
        {reply,{ok,Val,Per},LastState};
    {request, _Preflist} when not CanRequestPermissions->
      {ok,Counter,LastState} = cache_decrement(Key,State),
      Val=nncounter:value(Counter),
      Per=nncounter:localPermissions(State#state.worker#worker.id,Counter),
      {reply,{ok,Val,Per},LastState};
    _ ->
      %If cannot satisfy from cache, refresh it
      FreshCacheState = State#state{cache=refresh_cache(Key,State)},
      FreshCRDT = orddict:fetch(Key,FreshCacheState#state.cache),
      case worker_rc:check_permissions(State#state.worker,FreshCRDT) of
        finished ->
          {reply,{finished,0},FreshCacheState};
        ok ->
          {ok, Counter, LastState} = cache_decrement(Key,FreshCacheState),
          Val=nncounter:value(Counter),
          Per=nncounter:localPermissions(State#state.worker#worker.id,Counter),
          {reply, {ok,Val,Per}, LastState};
        {forbidden_not_available,Val} ->
          {reply,{forbidden,Val},FreshCacheState};
        {forbidden,PrefList} when CanRequestPermissions ->
          case request_permissions(FreshCacheState,Key,PrefList,sync) of
            forbidden ->
              %Get the most recent value
              Cache = refresh_cache(Key,FreshCacheState),
              ModifiedState = State#state{cache = Cache},
              {ok,CacheValue} = cache_get_value(Key,ModifiedState),
              {reply,{forbidden, nncounter:value(CacheValue)},ModifiedState};
            {ok,StateUpdated} ->
              {ok,Counter,LastState} = cache_decrement(Key,StateUpdated),
              Val=nncounter:value(Counter),
              Per=nncounter:localPermissions(State#state.worker#worker.id,Counter),
              {reply,{ok,Val,Per},LastState}
          end;
        {forbidden,_PrefList} ->
          Val = nncounter:value(FreshCRDT),
          {reply,{forbidden,Val},FreshCacheState};
        {request,PrefList} when CanRequestPermissions ->
          {_,StateUpdated} = request_permissions(FreshCacheState,Key,PrefList, async),
          Value = nncounter:value(FreshCRDT),
          Perm = nncounter:localPermissions(State#state.worker#worker.id,FreshCRDT),
          io:format("Value: ~p Permissions: ~p ~n",[Value,Perm]),
          {ok,Counter,LastState} = cache_decrement(Key,StateUpdated),
          Val=nncounter:value(Counter),
          Per=nncounter:localPermissions(State#state.worker#worker.id,Counter),
          {reply,{ok,Val,Per},LastState};
        {request,_PrefList} ->
          {ok,Counter,LastState} = cache_decrement(Key,FreshCacheState),
          Val=nncounter:value(Counter),
          Per=nncounter:localPermissions(State#state.worker#worker.id,Counter),
          {reply,{ok,Val,Per},LastState}
      end
  end;

%DUMB pattern match... too tired.
handle_command({merge_value,Key,CRDT}, _Sender, State) ->
  case (State#state.synch_pid) of
    nil ->
      io:format("WARNING SYNCHRONIZER NOT ON ~n"),
      {noreply,State};
    Pid ->
      Pid ! [Key],
      {ok,CachedCRDT} = cache_get_value(Key,State),
      MergedWithCache =nncounter:merge(CRDT,CachedCRDT),
      io:format("received merge value~n"),
      case worker_rc:merge_crdt(State#state.worker,Key,MergedWithCache) of
        notfound ->
          ModifiedState = State#state{ cache = orddict:store(Key,MergedWithCache,State#state.cache)},
          worker_rc:add_key(State#state.worker,Key,CRDT),
          {noreply,ModifiedState};
        {error,_} -> {noreply,State};
        Merged ->
          ModifiedState = State#state{ cache = orddict:store(Key,Merged,State#state.cache)},
          {noreply,ModifiedState}

      end
  end;

handle_command({track_keys,Keys}, _Sender, State) ->
  State#state.synch_pid ! Keys,
  {reply, ok, State};

handle_command({get_value,Key}, _Sender, State) ->
  Result = worker_rc:get_value(State#state.worker,Key),
  {reply, Result, State};

handle_command({request_permissions,Key,RequesterId,SyncType},_Sender,State) ->
  CRDT = worker_rc:transfer_permissions(Key,RequesterId,State#state.worker,State#state.transfer_policy),
  ModifiedState = State#state{ cache = orddict:store(Key,CRDT,State#state.cache)},
  case SyncType of
    async ->
      rpc:async_call(orddict:fetch(RequesterId,State#state.ids_addresses), crdtdb, merge_value, [Key,CRDT]),
      {noreply,ModifiedState};
    sync ->
      {reply,CRDT,ModifiedState}
  end;

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

request_permissions(State,Key,PrefList, Mode) ->
  CanRequestPermissions = permissionsRequestAllowed(Key,State),
  case CanRequestPermissions of
    true ->
      case Mode of
        async ->
          (asynchronous_request_mode())(Key,PrefList,State),
          State1 = State#state{last_permission_request=orddict:store(Key,now(),
            State#state.last_permission_request)},
          {ok, State1};
        sync  ->
          io:format("Exhaustive mode: ~p ~n",[PrefList]),
          case (exhaustive_request_mode())(Key,PrefList,State) of
            forbidden -> forbidden;
            {ok,CRDT} ->
              State1 = State#state{
                last_permission_request=orddict:store(Key,now(), State#state.last_permission_request),
                cache=orddict:store(Key,CRDT,State#state.cache)},
              {ok,State1}
          end
      end;
    false -> {refused,State}
  end.

do_merge(Pid) -> Pid ! doIt.

merge_remote(Keys,State) ->
  receive
    doIt ->
      lists:foreach(fun(Key) ->
        Object = worker_rc:get_crdt(State#state.worker,Key),
        lists:foreach(fun({Id,Address}) ->
          if
            Id /= State#state.worker#worker.id ->
              %io:format("SENDING OBJECT ~p FOR MERGE on doIT to ~p ~n",[Key, Address]),
              rpc:async_call(Address, crdtdb, merge_value, [Key,Object]);
            true -> ok
          end
        end, State#state.sync_addresses)
      end,ordsets:to_list(Keys)),
      merge_remote(Keys,State);
    terminate -> ok;
    NewKeys = [_Head | _Tail]->
      merge_remote(lists:foldl(fun(K,Set) -> ordsets:add_element(K,Set)end,Keys,NewKeys),State);
    Junk -> io:format("Received junk ~p ~n",[Junk])
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
    %Modified, if not found allow
    error -> true
  end.

cache_get_value(Key, State) ->
  case orddict:find(Key,State#state.cache) of
    {ok, CRDT} -> {ok,CRDT};
    error ->
      FreshCache = refresh_cache(Key,State),
      cache_get_value(Key,State#state{cache = FreshCache})
  end.

cache_increment(Key, State) ->
  case orddict:find(Key,State#state.cache) of
    {ok, CRDT} ->
      case nncounter:increment(State#state.worker#worker.id,1,CRDT) of
        {ok, UpdtCounter} ->
          ModifiedState = State#state{cache = orddict:store(Key,UpdtCounter,State#state.cache)},
          {ok,UpdtCounter,ModifiedState}
      end;
    error ->
      FreshCache = refresh_cache(Key,State),
      cache_increment(Key,State#state{cache = FreshCache})
  end.

cache_decrement(Key, State) ->
  case orddict:find(Key,State#state.cache) of
    {ok, CRDT} ->
      case nncounter:decrement(State#state.worker#worker.id,1,CRDT) of
        {ok, UpdtCRDT} ->
          ModifiedState = State#state{cache = orddict:store(Key,UpdtCRDT,State#state.cache)},
          {ok,UpdtCRDT,ModifiedState};
        _ -> {fail,nncounter:value(CRDT),State}
      end;
    error ->
      FreshCache = refresh_cache(Key,State),
      cache_decrement(Key,State#state{cache = FreshCache})
  end.

refresh_cache(Key,State) ->
  case orddict:find(Key,State#state.cache) of
    {ok,CachedObj} ->
      Merged = worker_rc:merge_crdt(State#state.worker, Key, CachedObj),
      orddict:store(Key,Merged,State#state.cache);
    error ->
      Obj = worker_rc:get_crdt(State#state.worker,Key),
      orddict:store(Key,Obj,State#state.cache)
  end.





%% ===================================================================
%% Permissions request protocols
%% ===================================================================

asynchronous_request_mode() -> fun(Key,List,State) ->
  Fun =
    fun(_F,[]) -> io:format("no more regions to try~n"), failed;
      (F,[{Head,_Permissions} | Tail]) ->
        if
          Head =/= (State#state.worker)#worker.id ->
            Target = orddict:fetch(Head,State#state.ids_addresses),
            rpc:async_call(Target, crdtdb, request_permissions, [Key,(State#state.worker)#worker.id,async]);
          true -> F(F,Tail)
        end
    end,
  Fun(Fun,List)
end.

exhaustive_request_mode() -> fun(Key,List,State) ->
  Fun =
    fun(_F,[]) -> io:format("no more regions to try"), forbidden;
       (F,[{Head,_Permissions} | Tail]) ->
      Target = orddict:fetch(Head,State#state.ids_addresses),
      if
        Target =/= (State#state.worker)#worker.id ->
          io:format("Exhaustive request!!~n"),
          CRDT = rpc:call(Target, crdtdb, request_permissions, [Key,(State#state.worker)#worker.id,sync]),
          Merged = worker_rc:merge_crdt(State#state.worker,Key,CRDT),
          case nncounter:localPermissions((State#state.worker)#worker.id, Merged) > 0 of
            true -> io:format("success!!~n"),{ok,Merged};
            false -> F(F,Tail)
          end;
        true -> F(F,Tail)
      end
    end,
  Fun(Fun,List)
end.
