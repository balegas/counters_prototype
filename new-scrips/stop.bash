#!/bin/bash

source $1

pssh="parallel-ssh"
hosts=""
for h in ${SERVERS[@]}; do
	hosts=$hosts" -H "$USERNAME"@"$h" "
done

	
cmd=$pssh" "$hosts
cmd=$cmd" sudo riak stop ; sudo "$CRDTDB_DIR" crdtdb stop ; sudo killall beam.smp; sudo rm -fr /var/lib/riak/*; sudo rm -fr "$CRDTDB_DIR"/data/*"

$cmd

# ./new-scrips/stop.bash ./new-scrips/US-WEST ; ./new-scrips/stop.bash ./new-scrips/US-EAST ; ./new-scrips/stop.bash ./new-scrips/EU-WEST