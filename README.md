crdtdb: A Riak Core Application
======================================

#Instructions
Compile Riak using devrel

Make a devrel release of the crdtdb repo, it will assign the ports to match Riak's devrel

### Start a Riak node
Create a last writer wins bucket
	curl -X PUT -H 'Content-Type: application/json' -d '{"props":{"last_write_wins":true, "n_val":3}}' "http://localhost:10018/buckets/ITEMS/props"

### Start a crdtdb node
crdtdb/dev/dev1/bin/crdtdb start

### Initialize the nodes
erl -name 'client@127.0.0.1' -setcookie crdtdb

rpc:call('crdtdb1@127.0.0.1', crdtdb, start, [id0, [{id0,'crdtdb1@127.0.0.1'}]]).

*id0 is the DC identifier
* [{id0,'crdtdb1@127.0.0.1'}] is a list of server nodes

### Clear the database
erl -name 'client@127.0.0.1' -setcookie crdtdb

rpc:call('crdtdb1@127.0.0.1', crdtdb, reset, [id0,0,5,[{id0,'crdtdb1@127.0.0.1'}]]).

* id0 is the local 1
* 0 is the max key id
* 5 is the initial value
* [{id0,'crdtdb1@127.0.0.1'}] is a list of server nodes
 
### Try some commands
erl -name 'client@127.0.0.1' -setcookie crdtdb

rpc:call('crdtdb1@127.0.0.1', crdtdb, decrement, [<<"0">>]).


