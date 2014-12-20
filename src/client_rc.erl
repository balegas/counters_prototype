%%%-------------------------------------------------------------------
%%% @author balegas
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Dez 2013 19:10
%%%-------------------------------------------------------------------
-module(client_rc).
-author("balegas").
-include("constants.hrl").

%% API
-export([start/3, reset/6, reset/5, broadcast/3]).

reset(Region, Address,NKeys,InitValue,AllAddresses) ->
    rpc:call(Address, crdtdb, reset, [Region,NKeys,InitValue,AllAddresses]).

reset(Random, Region,Address,NKeys,InitValue,AllAddresses) ->
    rpc:call(Address, crdtdb, reset, [Region,NKeys,InitValue,AllAddresses,Random]).

start(Address,RegionId,AllAddresses) ->
    rpc:call(Address, crdtdb, start, [RegionId,AllAddresses]).

broadcast(Address,NKeys,AllAddresses) ->
    rpc:call(Address, crdtdb, broadcast, [NKeys,AllAddresses]).