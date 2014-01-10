-define(DEFAULT_RIAK_ADDRESS, "localhost").
-define(DEFAULT_PB_PORT, 8087).

-define(BUCKET, {<<"default">>,<<"ITEMS">>}).

%% Delays the execution of updates in milliseconds
-define(MIN_INTERVAL, 50).

-define(TIME_UNIT, 1000).

-define(DEFAULT_TIMEOUT, 30 * ?TIME_UNIT).

%%Replication may have to be changed between experiments, this should be a aprameter
-define(REPLICATION_FACTOR,1).


%% 10 second interval - not using millisecond precision?
-define(PLOT_INTERVAL, 10000 * ?TIME_UNIT).

-define(SYNC_INTERVAL, 60 * ?TIME_UNIT). % One minute

-define(PERMISSIONS_THRESHOLD, 20).




