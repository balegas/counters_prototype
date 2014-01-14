%%%-------------------------------------------------------------------
%%% @author balegas
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2014 22:29
%%%-------------------------------------------------------------------
-author("balegas").
-record(state, {partition, worker, ids_addresses, request_mode, transfer_policy, sync_timer, synch_pid, sync_addresses,last_permission_request}).
