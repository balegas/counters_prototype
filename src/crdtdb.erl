-module(crdtdb).
-include("crdtdb.hrl").
-include("constants.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ reset/3, decrement/1, get_value/1, merge_value/2, request_permissions/2]).

-ignore_xref([ reset/0, decrement/1, get_value/1, merge_value/2, request_permissions/2]).

%% Public API

%%Resets the Bucket and start the periodic synchronizer for each key
%%Address in the form {id,crdtdb@Address,Port}
reset(NumKeys,InitValue,Addresses) ->
  %Any node can process reset
  RandomIdx = riak_core_util:chash_key({<<"reset">>, term_to_binary(now())}),
  [{RandomIndexNode, _Type}] = riak_core_apl:get_primary_apl(RandomIdx, 1, crdtdb),
  riak_core_vnode_master:sync_spawn_command(RandomIndexNode, {reset,NumKeys,InitValue,Addresses}, crdtdb_vnode_master),

  NodeKeys = lists:foldl(fun(Key,Dict)->
    BinaryKey = integer_to_binary(Key),
    KeyIdx = riak_core_util:chash_key({?BUCKET,BinaryKey}),
    [{{_,KeyNode}, _Type}] = riak_core_apl:get_primary_apl(KeyIdx, 1, crdtdb),
    orddict:append(KeyNode,BinaryKey,Dict) end,
    orddict:new(),lists:seq(0,NumKeys)),


  orddict:fold(fun(_, Values=[Key|_],_)->
    EKeyIdx = riak_core_util:chash_key({?BUCKET,Key}),
    [{EKeyNode, _}] = riak_core_apl:get_primary_apl(EKeyIdx, 1, crdtdb),
    riak_core_vnode_master:sync_spawn_command(EKeyNode, {synchronize,Values,Addresses}, crdtdb_vnode_master) end,
    nil,NodeKeys).


%%Decrements a given Key if the value is positive
decrement(Key) ->
  DocIdx = riak_core_util:chash_key({?BUCKET, Key}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
  io:format("~p ~p",[PrefList, node()]),
  [{IndexNode, _Type}] = PrefList,
  riak_core_vnode_master:sync_spawn_command(IndexNode, decrement, crdtdb_vnode_master).

%%Retrieves the value of the given Key
get_value(Key) ->
  DocIdx = riak_core_util:chash_key({?BUCKET, Key}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
  io:format("~p ~p",[PrefList, node()]),
  [{IndexNode, _Type}] = PrefList,
  riak_core_vnode_master:sync_spawn_command(IndexNode, get_value, crdtdb_vnode_master).

merge_value(Key,CRDT) ->
  DocIdx = riak_core_util:chash_key({?BUCKET, Key}),
  [{IndexNode, _Type}] = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
   riak_core_vnode_master:sync_spawn_command(IndexNode, {merge_value,Key,CRDT}, crdtdb_vnode_master).

request_permissions(Key, RequesterId) ->
  DocIdx = riak_core_util:chash_key({?BUCKET, Key}),
  [{IndexNode, _Type}] = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
  riak_core_vnode_master:sync_spawn_command(IndexNode, {request_permissions,Key,RequesterId}, crdtdb_vnode_master).