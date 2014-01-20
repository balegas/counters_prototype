-module(crdtdb).
-include("crdtdb.hrl").
-include("constants.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([start/1, reset/3, reset/4, decrement/1, increment/1, get_value/1, merge_value/2, request_permissions/2]).

-ignore_xref([reset/3, reset/4, decrement/1, increment/1, get_value/1, merge_value/2, request_permissions/2]).

%% Public API

%%Resets the Bucket and start the periodic synchronizer for each key
%%Address in the form {id,crdtdb@Address}

reset(NumKeys,InitValue,Addresses, Random) ->
  reset_bucket(NumKeys,InitValue,Addresses,Random).

reset(NumKeys,InitValue,Addresses) ->
  reset_bucket(NumKeys,InitValue,Addresses,noRandom).

reset_bucket(NumKeys,InitValue,Addresses,Random) ->
  start(Addresses),
  RandomIdx = riak_core_util:chash_key({<<"start">>, term_to_binary(now())}),
  [{RandomIndexNode, _Type}] = riak_core_apl:get_primary_apl(RandomIdx, 1, crdtdb),
  if
    Random == noRandom ->
      riak_core_vnode_master:sync_spawn_command(RandomIndexNode, {reset,NumKeys,InitValue,Addresses}, crdtdb_vnode_master);
    true ->
      riak_core_vnode_master:sync_spawn_command(RandomIndexNode, {reset,Random,NumKeys,InitValue,Addresses}, crdtdb_vnode_master)
  end,

  NodeKeys = lists:foldl(fun(Key,Dict)->
    BinaryKey = integer_to_binary(Key),
    KeyIdx = riak_core_util:chash_key({?BUCKET,BinaryKey}),
    [{VirtualNode, _Type}] = riak_core_apl:get_primary_apl(KeyIdx, 1, crdtdb),
    orddict:append(VirtualNode,BinaryKey,Dict) end,
    orddict:new(),lists:seq(0,NumKeys)),

  orddict:fold(fun(_, Keys=[Key|_],_)->
    EKeyIdx = riak_core_util:chash_key({?BUCKET,Key}),
    [{EKeyNode, _}] = riak_core_apl:get_primary_apl(EKeyIdx, 1, crdtdb),
    riak_core_vnode_master:sync_spawn_command(EKeyNode, {track_keys,Keys}, crdtdb_vnode_master) end,
    nil,NodeKeys).


start(Addresses) ->
  AllNodes = riak_core_apl:active_owners(crdtdb),
  lists:foreach(fun({NodeAddress,_})->
    riak_core_vnode_master:sync_spawn_command(NodeAddress, {start,Addresses}, crdtdb_vnode_master)
  end,AllNodes).

%%Decrements a given Key if the value is positive
decrement(Key) ->
  DocIdx = riak_core_util:chash_key({?BUCKET, Key}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
  [{IndexNode, _Type}] = PrefList,
  riak_core_vnode_master:sync_spawn_command(IndexNode, {decrement,Key}, crdtdb_vnode_master).

increment(Key) ->
  DocIdx = riak_core_util:chash_key({?BUCKET, Key}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
  [{IndexNode, _Type}] = PrefList,
  riak_core_vnode_master:sync_spawn_command(IndexNode, {increment,Key}, crdtdb_vnode_master).

%%Retrieves the value of the given Key
get_value(Key) ->
  DocIdx = riak_core_util:chash_key({?BUCKET, Key}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
  [{IndexNode, _Type}] = PrefList,
  riak_core_vnode_master:sync_spawn_command(IndexNode, {get_value,Key}, crdtdb_vnode_master).

merge_value(Key,CRDT) ->
  DocIdx = riak_core_util:chash_key({?BUCKET, Key}),
  [{IndexNode, _Type}] = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
   riak_core_vnode_master:sync_spawn_command(IndexNode, {merge_value,Key,CRDT}, crdtdb_vnode_master).

request_permissions(Key, RequesterId) ->
  DocIdx = riak_core_util:chash_key({?BUCKET, Key}),
  [{IndexNode, _Type}] = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
  riak_core_vnode_master:sync_spawn_command(IndexNode, {request_permissions,Key,RequesterId}, crdtdb_vnode_master).