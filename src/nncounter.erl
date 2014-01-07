%%%-------------------------------------------------------------------
%%% @author balegas
%%% @copyright (C) 2013, <COMPANY>
%%% @doc
%%% A State based implementation of the Non-negative counter CRDT.
%%% This counter is able to maintain a non-negative value by
%%% explicitly exchanging permissions to execute decrement operations. 
%%% Each replica, a 'term', maintains a number of tokens that can
%%% spend to execute decrement operations.
%%% Increment operations generate new tokens, and these can be 
%%% transferred between replicas.
%%% All operations on this CRDT are monotonic and do not keep 
%%% extra tombstones.
%%% @end
%%% Created : 24. Out 2013 11:50
%%%-------------------------------------------------------------------

-module(nncounter).
-author("balegas").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-opaque id() :: term.
-type non_neg_counter() :: {orddict:orddict(),orddict:orddict()}.

-export_type([id/0]).

%% API
-export([new/2, localPermissions/2, value/1, increment/3, decrement/3, transfer/4, merge/2,to_binary/1,from_binary/1]).

%% @doc Create a new, empty 'non_neg_counter()'
-spec new() -> non_neg_counter().
new() ->
  orddict:new().

%% @doc Creates a new 'non_neg_counter()' with initial value 'V' and 'V' permissions assigned to replica 'Id'.
-spec new(id(),integer()) -> non_neg_counter().
new(Id,V) -> {orddict:update_counter({Id,Id},V,new()), orddict:update_counter(Id,0,new())}.

%% @doc Returns the available permissions for 'non_neg_counter' 'Id'.
-spec localPermissions(id,non_neg_counter()) -> integer.
localPermissions(Id,{P,D}) -> 
	Received = lists:foldl(fun({_,V},Acc) -> Acc + V end, 0, orddict:filter(
		fun({_,To},_) when To == Id -> true; 
		(_,_) -> false end, P)),
	Granted  = lists:foldl(fun({_,V},Acc) -> Acc + V end, 0, orddict:filter(
		fun({From,To},_) when From == Id andalso To /= Id -> true;
		(_,_) -> false end, P)),
	case orddict:find(Id,D) of
		{ok, Decrements} -> Received - Granted - Decrements;
		error -> Received - Granted
	end.
	
%% @doc Returns the available permissions for 'non_neg_counter' 'Id'.
-spec value(non_neg_counter()) -> integer.
value({P,D}) -> 
	TotalIncrements = orddict:fold(fun({K,K},V,Acc) -> V + Acc; (_,_,Acc) -> Acc end, 0, P),
	TotalDecrements = orddict:fold(fun(_,V,Acc) -> V + Acc end, 0, D),
	TotalIncrements - TotalDecrements.

%% @doc Increments 'V' units to 'non_neg_counter' 'Id'.
-spec increment(id, non_neg_integer, non_neg_counter()) -> non_neg_counter().
increment(Id,V,{P,D}) -> 
	{ok,{orddict:update_counter({Id,Id},V,P),D}}.

%% @doc Decrements 'V' units from 'non_neg_counter' 'Id', if it has enough local permissions.
-spec decrement(id, non_neg_integer, non_neg_counter()) -> {ok,non_neg_counter()} | forbidden.
decrement(Id,V,{P,D}) -> 
	case localPermissions(Id,{P,D}) of
		Available when Available >= V -> {ok, {P,orddict:update_counter(Id,V,D)}};
		_ -> forbidden
	end.
%% @doc Transfers 'V' permission units from 'non_neg_counter' 'From' to 'non_neg_counter' 'To', 
%% only if 'From' has enough local permissions.
-spec transfer(id, id, non_neg_integer, non_neg_counter()) -> {ok,non_neg_counter()} | forbidden.
transfer(From,To,V,{P,D}) -> 
	case localPermissions(From,{P,D}) of
		Available when Available >= V -> {ok, {orddict:update_counter({From,To},V,P),D}};
		_ -> forbidden
	end.
	
%% @doc merges the state of two 'non_neg_counter'.
-spec merge(non_neg_counter(), non_neg_counter()) -> non_neg_counter().
merge({AP,AD}, {BP,BD}) ->
    {orddict:merge(fun(_, V1, V2) -> max(V1,V2) end,AP, BP),
	 orddict:merge(fun(_, V1, V2) -> max(V1,V2) end, AD, BD)}.
	 
-spec to_binary(non_neg_counter()) -> binary().
to_binary(C) -> term_to_binary(C).
	
-spec from_binary(binary()) -> non_neg_counter().
from_binary(<<B/binary>>) -> binary_to_term(B).



%% ===================================================================
%% EUnit tests
%% ===================================================================

-ifdef(TEST).
new_test() ->
	?assertEqual([], new()).

new2_test() ->
	?assertEqual({[{{r1,r1},10}],[{r1,0}]}, new(r1,10)).

localPermisisons_test() ->
	Counter = new(r1,10),
	?assertEqual(10, localPermissions(r1,Counter)),
	?assertEqual(0, localPermissions(r2,Counter)).

increment1_test() ->
	Counter = new(r1,10),
	?assertEqual(10, localPermissions(r1,Counter)),
	{_,Counter1} = increment(r1,5,Counter),
	?assertEqual(15, localPermissions(r1,Counter1)),
	?assertEqual(15, value(Counter1)).

increment2_test() ->
	Counter = new(r1,10),
	?assertEqual(10, localPermissions(r1,Counter)),
	{_,Counter1} = increment(r2,5,Counter),
	?assertEqual(5, localPermissions(r2,Counter1)),
	?assertEqual(10, localPermissions(r1,Counter1)),
	?assertEqual(15, value(Counter1)).

decrement_test() ->
	{_,Counter} = decrement(r1,5,new(r1,10)),
	?assertEqual(5, value(Counter)).
	
decrement2_test() ->
	{_,Counter} = decrement(r1,10,new(r1,10)),
	?assertEqual(0, value(Counter)),
	?assertEqual(forbidden, decrement(r1,1,Counter)).

decrement_increment_test() ->
	{_,Counter} = decrement(r1,10,new(r1,10)),
	?assertEqual(0, value(Counter)),
	{_,Counter1} = increment(r2,10,Counter),
	?assertEqual(10, value(Counter1)).

decrement_increment2_test() ->
	{_,Counter} = decrement(r1,10,new(r1,10)),
	?assertEqual(0, value(Counter)),
	{_,Counter1} = increment(r2,10,Counter),
	?assertEqual(forbidden, decrement(r1,1,Counter1)),
	?assertEqual(10, value(Counter1)),
	{_,Counter2} = decrement(r2,5,Counter1),
	?assertEqual(5, value(Counter2)).
	
transfer_test() ->
	Counter = new(r1,10),
	?assertEqual(10, localPermissions(r1,Counter)),
	{ok,Counter1} = transfer(r1,r2,5,Counter),
	?assertEqual(5, localPermissions(r1,Counter1)),
	?assertEqual(forbidden, transfer(r1,r2,6,Counter1)),
	?assertEqual(5, localPermissions(r2,Counter1)),
	?assertEqual(10, value(Counter1)).

binary_test() ->
	Counter = new(r1,10),
	B = to_binary(Counter),
	from_binary(B) == Counter.
	
merge_test() ->
		Counter1 = new(r1,10),
		CounterBinary = to_binary(Counter1),
		Counter2 = from_binary(CounterBinary),
		{ok,Counter3} = increment(r2,10,Counter1),
		{ok,Counter4} = decrement(r1,5,Counter2),
		Counter12 = merge(Counter3,Counter4),
		Counter21 = merge(Counter4,Counter3),
		?assertEqual(Counter12,Counter21),
		?assertEqual(15,value(Counter21)).
-endif.

