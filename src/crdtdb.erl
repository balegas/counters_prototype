-module(crdtdb).
-include("crdtdb.hrl").
-include("constants.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0, reset/0, decrement/0
        ]).

-ignore_xref([
              ping/0,reset/0, decrement/0
             ]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, crdtdb_vnode_master).
reset() ->
  DocIdx = riak_core_util:chash_key({<<"reset">>, term_to_binary(now())}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
  [{IndexNode, _Type}] = PrefList,
  riak_core_vnode_master:sync_spawn_command(IndexNode, reset, crdtdb_vnode_master).
decrement() ->
  DocIdx = riak_core_util:chash_key({?BUCKET, ?KEY}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, crdtdb),
  io:format("~p ~p",[PrefList, node()]),
  [{IndexNode, _Type} | _] = PrefList,
  riak_core_vnode_master:sync_spawn_command(IndexNode, decrement, crdtdb_vnode_master).
