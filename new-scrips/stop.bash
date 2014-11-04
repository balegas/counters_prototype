#!/bin/bash

source $1

hosts=""
for h in ${SERVERS[@]}; do
	hosts=$hosts" -H "$USERNAME"@"$h" "
done

	
cmd="pssh "$hosts
cmd=$cmd" sudo riak stop ; sudo "$CRDTDB_DIR" crdtdb stop ; sudo killall beam.smp; sudo rm -fr /var/lib/riak/ring/* ; sudo rm -fr /var/lib/riak/anti_entropy/; sudo rm -fr /var/lib/riak/anti_entropy/; sudo  rm -fr /var/lib/riak/anti_entropy/; sudo rm -fr /var/lib/riak/kv_vnode/; sudo rm -fr /var/lib/riak/riak_repl/; sudo rm -fr "$CRDTDB_DIR"/data/*"

$cmd

# ./stop.bash US-EAST && ./stop.bash US-WEST && ./stop.bash EU-WEST
