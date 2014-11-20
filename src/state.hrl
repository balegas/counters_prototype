%%%-------------------------------------------------------------------
%%% @author balegas
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2014 22:29
%%%-------------------------------------------------------------------
-author("balegas").
-record(state, {batcher,partition, worker, ids_addresses, key_mapping, transfer_policy, sync_timer, synch_pid, sync_addresses,last_permission_request, port, cache}).
