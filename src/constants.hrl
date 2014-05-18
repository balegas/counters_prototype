-define(DEFAULT_RIAK_ADDRESS, "localhost").
-define(DEFAULT_PB_PORT, 8087).

-define(BUCKET, {<<"default">>,<<"ITEMS">>}).

-define(DEFAULT_KEY, <<"0">>).

%% Delays the execution of updates in milliseconds
-define(MAX_INTERVAL, 100).

-define(TIME_UNIT, 1000).

-define(DEFAULT_TIMEOUT, (30 * ?TIME_UNIT)).

%%Replication may have to be changed between experiments, this should be a aprameter
-define(REPLICATION_FACTOR,3).


%% 10 second interval - not using millisecond precision?
-define(PLOT_INTERVAL, (10000*?TIME_UNIT)).

-define(SYNC_INTERVAL, 60 * ?TIME_UNIT). % One minute

-define(PERMISSIONS_THRESHOLD, 1000).

-define(PERMISSIONS_DELAY, (5*math:pow(10,6))).




