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
         do_merge/1,
         key_batcher/5,
         batching_manager/1,
         write_to_storage/5
        ]).

-ignore_xref([
              start_vnode/1, merge_remote/2
             ]).

-include("constants.hrl").
-include("state.hrl").

%% TODO: Check why in some cases the client does not execute the last decrement

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

%%  Inits the vnode:
%%  Initializes the cache and the operations batcher;
%%  Chooses the policy to manage escrows, currently it cannot be paremetrized.
init([Partition]) ->
    State = #state {
               partition=Partition,
               sync_timer = nil,
               synch_pid = nil,
               worker = nil,
               cache = orddict:new(),
               last_permission_request=orddict:new(),
               transfer_policy = nncounter:half_permissions(),
               ids_addresses = orddict:new(),
               key_mapping = orddict:new(),
               port = app_helper:get_env(riak_core, pb_port,?DEFAULT_PB_PORT),
               batcher = spawn_link(crdtdb_vnode, batching_manager, [orddict:new()])
              },
    {ok, State}.

%%  Resets the database by writting the InitValue to all keys from '0' to NumKeys.
handle_command({reset, NumKeys, InitValue, AddressesIds}, _Sender, State) ->
    {Address,Port} = {?DEFAULT_RIAK_ADDRESS, State#state.port},
    lager:info("Empty Bucket"),
    worker_rc:empty_bucket(?BUCKET,Address,Port),
    lager:info("Reset Counters"),
    worker_rc:reset_bucket(NumKeys,InitValue, ?BUCKET, Address, Port, AddressesIds),
    {reply, ok, State};

%%  Resets the database and uess a uniform random generator to pick the 
%%  initial value of each key. 
handle_command({reset, random, NumKeys, InitValue, AddressesIds}, _Sender, State) ->
    {Address,Port} = {?DEFAULT_RIAK_ADDRESS, State#state.port},
    worker_rc:empty_bucket(?BUCKET,Address,Port),
    worker_rc:reset_bucket(random,NumKeys,InitValue,
                           ?BUCKET, Address, Port, AddressesIds),
    {reply, ok, State};

%%  Opens the connection to Riak and sets the Region identifier for this node.
%%  If the remote synchronizer is on, reboots it to avoid synching to machines
%%  in the previous configuration.
handle_command({start, Region, Addresses},_Sender,State) ->
    DictAddresses = lists:foldl(
                      fun({IdSuffix,Address},Dict) ->
                              %String = erlang:atom_to_list(IdSuffix),
                              %Id= string:sub_string(String, 1,string:rstr(String,"_")-1),
                              orddict:store(IdSuffix,Address,Dict) 
                      end,State#state.ids_addresses,Addresses),
    KeyMapping = lists:foldl(
                   fun({IdSuffix,Address},Dict) ->
                        %String = erlang:atom_to_list(IdSuffix),
                        %Suffix= string:sub_string(String,string:rstr(String,"_")+1),
                        orddict:store(IdSuffix,Address,Dict) 
                      end,State#state.ids_addresses,Addresses),
    Worker = worker_rc:init(?DEFAULT_RIAK_ADDRESS,State#state.port,?BUCKET,Region),
    NewState = State#state{
                 worker = Worker,
                 ids_addresses = DictAddresses,
                 key_mapping = KeyMapping,
                 sync_addresses = Addresses},
    case State#state.synch_pid /= nil of
        true -> State#state.synch_pid ! terminate,
                timer:cancel(State#state.sync_timer);
        false -> ok
    end,
    % Starts the remote synchronizer
    Pid = spawn_link(crdtdb_vnode, merge_remote ,[ordsets:new(), NewState]),
    {ok,Ref} = timer:apply_interval(?SYNC_INTERVAL,crdtdb_vnode,do_merge,[Pid]),
    Pid ! doIt,
    {reply, ok, NewState#state{sync_timer=Ref, synch_pid = Pid}};

%  Increment operation. 
%  First updates cached value and then send it to the batcher
%  to be written when the current write finishes
handle_command({increment,Key,ReplyPid}, _Sender, State) ->
    {ok,CRDT,ModifiedState} = cache_increment(Key,State),
    State#state.batcher ! {batch,Key,CRDT,ReplyPid},
    {noreply, ModifiedState};

%%  Decrement is a bit more complex. 
%%  First the node tries to decrement the counter from cache; If it succeeds
%%  send the update to the batcher. If the escrow for that key is below the
%%  threshold and the node can ask permissions for that key, then check if 
%%  new updates have arrived to the local storage.
%%  If the operation does not succeed on cache, th3n update the cached value
%%  with the value from storage and retry the operation.
%%
%%  TODO: Make this operations recursive instead of the double case
handle_command({decrement,Key,ReplyPid}, _Sender, State) ->
    {ok,CachedCRDT} = cache_get_value(Key,State),
    CanRequestPermissions = permissionsRequestAllowed(Key,State),
    Result = worker_rc:check_permissions(State#state.worker,CachedCRDT),
    case Result of
        finished ->
            ReplyPid ! {ReplyPid,{finished,0}},
            {noreply,State};
        ok ->
            {ok,UpdtCRDT,LastState} = cache_decrement(Key,State),
            State#state.batcher ! {batch,Key,UpdtCRDT,ReplyPid},
            {noreply,LastState};
        {request, _Preflist} when not CanRequestPermissions->
            {ok,UpdtCRDT,LastState} = cache_decrement(Key,State),
            State#state.batcher ! {batch,Key,UpdtCRDT,ReplyPid},
            {noreply,LastState};
        _ ->
            %If cannot satisfy from cache, refresh it
            FreshCacheState = State#state{cache=refresh_cache(Key,State)},
            FreshCRDT = orddict:fetch(Key,FreshCacheState#state.cache),
            case worker_rc:check_permissions(State#state.worker,FreshCRDT) of
                finished ->
                    ReplyPid ! {ReplyPid,{finished,0}},
                    {noreply,FreshCacheState};
                ok ->
                    {ok, UpdtCRDT, LastState} = 
                    cache_decrement(Key,FreshCacheState),
                    State#state.batcher ! {batch,Key,UpdtCRDT,ReplyPid},
                    {noreply, LastState};
                {forbidden_not_available,Val} ->
                    ReplyPid ! {ReplyPid,{forbidden,Val}},
                    {noreply,FreshCacheState};
                {forbidden,PrefList} when CanRequestPermissions ->
                    case request_permissions(FreshCacheState,Key,PrefList,sync) of
                        forbidden ->
                            %Get the most recent value
                            Cache = refresh_cache(Key,FreshCacheState),
                            ModifiedState = State#state{cache = Cache},
                            {ok,CachedCRDTUpdt} = 
                            cache_get_value(Key,ModifiedState),
                            ReplyPid ! 
                            {ReplyPid,{forbidden, nncounter:value(CachedCRDTUpdt)}},
                            {noreply,ModifiedState};
                        {ok,StateUpdated} ->
                            {ok,UpdtCRDT,LastState} = 
                            cache_decrement(Key,StateUpdated),
                            State#state.batcher ! {batch,Key,UpdtCRDT,ReplyPid},
                            {noreply,LastState}
                    end;
                {forbidden,_PrefList} ->
                    Val = nncounter:value(FreshCRDT),
                    ReplyPid ! {ReplyPid,{forbidden,Val}},
                    {noreply,FreshCacheState};
                {request,PrefList} when CanRequestPermissions ->
                    {_,StateUpdated} = 
                    request_permissions(FreshCacheState,Key,PrefList, async),
                    {ok,UpdtCRDT,LastState} = 
                    cache_decrement(Key,StateUpdated),
                    State#state.batcher ! {batch,Key,UpdtCRDT,ReplyPid},
                    {noreply,LastState};
                {request,_PrefList} ->
                    {ok,UpdtCRDT,LastState} = cache_decrement(Key,FreshCacheState),
                    State#state.batcher ! {batch,Key,UpdtCRDT,ReplyPid},
                    {noreply,LastState}
            end
    end;
%%  Handles a merge request from a remote node for a specific key
handle_command({merge_value,BinKey,CRDT}, _Sender, State) ->
    Result = case (State#state.synch_pid) of
                 nil ->
                     io:format("WARNING SYNCHRONIZER NOT ON ~n"),
                     {noreply,State};
                 Pid ->
                     % Send the key to the key tracker.
                     % Check if the tracker knows the key?? -- that would be nice
                     Pid ! [BinKey],
                     {ok,CachedCRDT} = cache_get_value(BinKey,State),
                     % Merges the received CRDT with the existing one
                     MergedWithCache = nncounter:merge(CRDT,CachedCRDT),
                     case worker_rc:merge_crdt(State#state.worker,BinKey,MergedWithCache) of
                         %The notfound does not make much sense because we already merged 
                         %with the cached value which means the value exists.
                         notfound ->
                             ModifiedState = 
                             State#state{ 
                               cache = orddict:store(BinKey,MergedWithCache,State#state.cache)},
                             worker_rc:add_key(State#state.worker,BinKey,CRDT),
                             {noreply,ModifiedState};
                         {error,_} -> {noreply,State};
                         Merged ->
                             %Updates the cache
                             ModifiedState =
                             State#state{ cache = orddict:store(BinKey,Merged,State#state.cache)},
                             {noreply,ModifiedState}

                     end
             end,
    %Key = erlang:binary_to_list(BinKey),
    %K = string:sub_string(Key,1,string:str(Key,"_")-1),
    %Keys = orddict:fold(fun(Region,_,KeysAcc) ->
    %                            KeyArgs= [K, erlang:atom_to_list(Region)],
    %                            [erlang:list_to_binary(string:join(KeyArgs,"_")) | KeysAcc]
    %                    end,[],State#state.key_mapping),
    %merge_multiple(CRDT,Keys,State),
    Result;
    
%%  Tells the vnode to track Keys
handle_command({track_keys,Keys}, _Sender, State) ->
    State#state.synch_pid ! Keys,
    {reply, ok, State};

handle_command({get_value,Key}, _Sender, State) ->
    Result = worker_rc:get_value(State#state.worker,Key),
    {reply, Result, State};


%%  Handles permissions request, replies async or sync
handle_command({request_permissions,Key,RequesterId,SyncType},_Sender,State) ->
    case worker_rc:transfer_permissions(
           Key,RequesterId,State#state.worker,State#state.transfer_policy) of
        {transferred,CRDT} ->
            %KeyAsList = erlang:binary_to_list(Key),
            %K = string:sub_string(KeyAsList,1,string:str(KeyAsList,"_")-1),
            %Keys = orddict:fold(fun(Region,_,KeysAcc) ->
            %                            KeyArgs= [K, erlang:atom_to_list(Region)],
            %                            [erlang:list_to_binary(string:join(KeyArgs,"_")) | KeysAcc]
            %                    end,[],State#state.key_mapping),
            %merge_multiple(CRDT,Keys,State),
            CRDT;
        {not_allowed,CRDT} -> CRDT;
        {Error,CRDT} -> 
            io:format("ERROR ERROR ERROR on transfer ~p~n",[Error]),
            CRDT
    end,
    ModifiedState = State#state{cache = orddict:store(Key,CRDT,State#state.cache)},

    case SyncType of
        async ->
            rpc:async_call(orddict:fetch(
                             RequesterId,State#state.ids_addresses), crdtdb, 
                           merge_value, [Key,CRDT]),
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

%merge_multiple(CRDTArg, Keys, State) ->
%    Fun = fun(F,Key,CRDT, RetryCount) ->
%                  case worker_rc:merge_crdt(State#state.worker,Key,CRDT) of
%                      notfound ->
%                          io:format("Ignoring error and proceed~n");
%                      {error,_} when RetryCount < 100 -> 
%                          io:format("Error (Strong consistency??) 
%                                                                      retry ~p Count: ~p~n",[Key, RetryCount]),
%                          F(F, Key, CRDT, RetryCount+1);
%                      {error,_} -> 
%                          io:format("Couldn't update key ~p~n",[Key]);
%                      _Merged -> ok
%                  end
%          end,
%    lists:foreach(fun(Key) -> Fun(Fun,Key,CRDTArg,State) end,Keys).

%% Requests permissions to other owners of the same key 
%% (can incur in intra-dc latencies on the receiver, 
%% because we contact the endpoint of the configurations and not the 
%% exact node that is resposible for the key)
%% Exhaustive mode is called when the vnode has no permissions and the value of
%% the counter is still positive. May return if no more permissions are 
%% available.
%%
request_permissions(State,Key,PrefList, Mode) ->
    CanRequestPermissions = permissionsRequestAllowed(Key,State),
    case CanRequestPermissions of
        true ->
            case Mode of
                async ->
                    (asynchronous_request_mode())(Key,PrefList,State),
                    State1 = State#state{
                               last_permission_request=
                               orddict:store(Key,now(), 
                                             State#state.last_permission_request)},
                    {ok, State1};
                sync  ->
                    io:format("Exhaustive mode: ~p ~n",[PrefList]),
                    case (exhaustive_request_mode())(Key,PrefList,State) of
                        forbidden -> forbidden;
                        {ok,CRDT} ->
                            State1 = State#state{
                                       last_permission_request =
                                       orddict:store(Key,now(),
                                                     State#state.last_permission_request),
                                       cache =
                                       orddict:store(Key,CRDT,State#state.cache)},
                            {ok,State1}
                    end
            end;
        false -> {refused,State}
    end.

do_merge(Pid) -> Pid ! doIt.

%% Process that handles the geo-replication. 
%% The process can receive a sync request or a List of new keys to track
%% % TODO: Only merge modified keys
merge_remote(Keys,State) ->
    receive
        doIt ->
            lists:foreach(
              fun(Key) ->
                      Object = worker_rc:get_crdt(State#state.worker,Key),
                      lists:foreach(
                        fun({Id,Address}) ->
                                if
                                    Id /= State#state.worker#worker.id ->
                                        rpc:async_call(Address, crdtdb, 
                                                       merge_value, [Key,Object]);
                                    true -> ok
                                end
                        end, State#state.sync_addresses)
              end,
              ordsets:to_list(Keys)),
            merge_remote(Keys,State);

        terminate -> ok;

        NewKeys = [_Head | _Tail]->
            merge_remote(lists:foldl(
                           fun(K,Set) ->
                                   IsElement = ordsets:is_element(K,Set),
                                   if
                                       IsElement -> Set;
                                       true ->
                                           %% A batcher for each key, not very 
                                           %% efficient but we assume the number 
                                           %% of keys is small
                                           Pid = spawn_link(crdtdb_vnode,key_batcher,
                                                            [not_batching,no_counter,[],K,State]),
                                           State#state.batcher ! {new_key, K, Pid},
                                           ordsets:add_element(K,Set)
                                   end
                           end,Keys,NewKeys),State);
        Junk -> io:format("Received junk ~p ~n",[Junk])
    end.

%%  Checks if this node is allowed to request permissions for a given key
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

%%  Cache related operations

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
                {ok, UpdtCRDT} ->
                    %write always
                    %worker_rc:add_key(State#state.worker,Key,UpdtCRDT),
                    ModifiedState = State#state{
                                      cache = orddict:store(Key,UpdtCRDT,State#state.cache)},
                    {ok,UpdtCRDT,ModifiedState}
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
                    %write always
                    %worker_rc:add_key(State#state.worker,Key,UpdtCRDT),
                    ModifiedState = State#state{
                                      cache = orddict:store(Key,UpdtCRDT,State#state.cache)},
                    {ok,UpdtCRDT,ModifiedState};
                _ -> {fail,nncounter:value(CRDT),State}
            end;
        error ->
            FreshCache = refresh_cache(Key,State),
            cache_decrement(Key,State#state{cache = FreshCache})
    end.

refresh_cache(Key,State) ->
    CRDT = worker_rc:get_crdt(State#state.worker,Key),
    orddict:store(Key,CRDT,State#state.cache).






%% ===================================================================
%% Permissions request protocols
%% ===================================================================

asynchronous_request_mode() -> 
    fun(Key,List,State) ->
            Fun =
            fun(_F,[]) -> failed;
               (F,[{Head,_Permissions} | Tail]) ->
                    if
                        Head =/= (State#state.worker)#worker.id ->
                            Target = orddict:fetch(Head,State#state.ids_addresses),
                            rpc:async_call(Target, crdtdb, request_permissions,
                                           [Key,(State#state.worker)#worker.id,async]);
                        true -> F(F,Tail)
                    end
            end,
            Fun(Fun,List)
    end.

exhaustive_request_mode() -> 
    fun(Key,List,State) ->
            Fun =
            fun(_F,[]) -> forbidden;
               (F,[{Head,_Permissions} | Tail]) ->
                    Target = orddict:fetch(Head,State#state.ids_addresses),
                    if
                        Target =/= (State#state.worker)#worker.id ->
                            CRDT = 
                            rpc:call(Target, crdtdb, request_permissions,
                                     [Key,(State#state.worker)#worker.id,sync]),
                            Merged = worker_rc:merge_crdt(State#state.worker,Key,CRDT),
                            case nncounter:localPermissions((State#state.worker)#worker.id, Merged) > 0 of
                                true -> {ok,Merged};
                                false -> F(F,Tail)
                            end;
                        true -> F(F,Tail)
                    end
            end,
            Fun(Fun,List)
    end.

%% ===================================================================
%% Operation batcher
%% ===================================================================

batching_manager(KeyPids) ->
    receive
        {new_key, Key, Pid} ->
            batching_manager(orddict:store(Key,Pid,KeyPids));
        {batch, Key, CRDT, ReplyPid} ->
            case orddict:find(Key, KeyPids) of
                {ok, Pid} ->
                    Pid ! {op, {ReplyPid, CRDT}},
                    batching_manager(KeyPids);
                error ->
                    batching_manager(KeyPids)
            end
    end.

key_batcher(not_batching,no_counter,[],Key,State) ->
    receive
        {op,{ReplyPid,CRDT}} ->
            spawn_link(crdtdb_vnode,write_to_storage,
                       [self(),[ReplyPid],Key,CRDT, State]),
            key_batcher(batching,no_counter,[],Key,State)
    end;

key_batcher(batching,MergeCRDT,Waiting,Key,State) ->
    receive
        {op,{ReplyPid,CRDT}} ->
            case MergeCRDT of
                no_counter -> key_batcher(batching, CRDT,[ReplyPid],Key,State);
                _ -> key_batcher(batching, nncounter:merge(CRDT,MergeCRDT),
                                 [ReplyPid | Waiting],Key,State)
            end;
        write_finished ->
            case MergeCRDT of
                no_counter ->
                    key_batcher(not_batching,no_counter,[],Key,State);
                _ ->
                    spawn_link(crdtdb_vnode,write_to_storage,
                               [self(),Waiting,Key, MergeCRDT, State]),
                    key_batcher(batching,no_counter,[],Key,State)
            end
    end.

write_to_storage(ReplyPid,Waiting,Key,CRDT,State) ->
    worker_rc:add_key(State#state.worker,Key,CRDT),
    notify_waiting(Waiting,CRDT,State#state.worker#worker.id),
    ReplyPid ! write_finished.

notify_waiting(Waiting, CRDT, LocalId) ->
    lists:foreach(
      fun(RequesterPid) ->
              Val=nncounter:value(CRDT),
              Per=nncounter:localPermissions(LocalId,CRDT),
              RequesterPid ! {RequesterPid,{ok,Val,Per}}
      end, Waiting).

