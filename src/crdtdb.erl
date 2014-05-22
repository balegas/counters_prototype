-module(crdtdb).
-include("crdtdb.hrl").
-include("constants.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([start/2, reset/4, reset/5, decrement/1, increment/1, get_value/1, merge_value/2, request_permissions/3]).

-ignore_xref([reset/3, reset/4, decrement/1, increment/1, get_value/1, merge_value/2, request_permissions/2]).

%% Public API

%%Resets the Bucket and start the periodic synchronizer for each key
%%Address in the form {id,crdtdb@Address}

reset(Region,NumKeys,InitValue,Addresses, Random) ->
  reset_bucket(Region,NumKeys,InitValue,Addresses,Random).

reset(Region,NumKeys,InitValue,Addresses) ->
  reset_bucket(Region,NumKeys,InitValue,Addresses,noRandom).

reset_bucket(Region,NumKeys,InitValue,Addresses,Random) ->
  start(Region,Addresses),
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


start(Region,Addresses) ->
  AllNodes = riak_core_apl:active_owners(crdtdb),
  lists:foreach(fun({NodeAddress,_})->
    riak_core_vnode_master:sync_spawn_command(NodeAddress, {start,Region,Addresses}, crdtdb_vnode_master)
  end,AllNodes).
	

%%Decrements a given Key if the value is positive
decrement(Key) ->
DocIdx = riak_core_util:chash_key({?BUCKET, Key}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
  [{IndexNode, _Type}] = PrefList,
  MyPid = self(),
  riak_core_vnode_master:command(IndexNode, {decrement,Key,MyPid}, crdtdb_vnode_master),
  receive
    {MyPid, Reply} ->
      Reply
  end
%Reply here
.

increment(Key) ->
  DocIdx = riak_core_util:chash_key({?BUCKET, Key}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
  [{IndexNode, _Type}] = PrefList,
  MyPid = self(),
  riak_core_vnode_master:command(IndexNode, {increment,Key,MyPid}, crdtdb_vnode_master),
  receive
    {MyPid, Reply} ->
      Reply
  end.

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


request_permissions(Key, RequesterId,SyncType) ->
  DocIdx = riak_core_util:chash_key({?BUCKET, Key}),
  [{IndexNode, _Type}] = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
  riak_core_vnode_master:sync_spawn_command(IndexNode, {request_permissions,Key,RequesterId,SyncType}, crdtdb_vnode_master).
