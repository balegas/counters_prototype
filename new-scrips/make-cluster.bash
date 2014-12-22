#!/bin/bash

CRDTDB=true
REPL=false

source $1

if $REPL; then
	for k in $(seq 0 $((${#SERVERS[@]}-1))); 
	do
		cmd="sudo mv /etc/riak/advanced.config.bak /etc/riak/advanced.config ; sudo sed 's/127.0.0.1/${IP[k]}/' -i.bak /etc/riak/advanced.config"
		echo $cmd
		ssh $USERNAME@${SERVERS[k]} $cmd
	done
fi

for k in $(seq 0 $((${#SERVERS[@]}-1))); 
do
	cmd="sudo rm -r /var/log/riak/* ; sudo mv /etc/riak/riak.conf.bak /etc/riak/riak.conf ; sudo sed 's/riak@127.0.0.1/riak@${SERVERS[k]}/;s/## \(strong_consistency\)/\1/;s/127.0.0.1:8087/${IP[k]}:8087/' -i.bak /etc/riak/riak.conf ; sudo riak start"
	# && sudo sed -e 's/127.0.0.1/${IP[k]}/' -i.bak2 /etc/riak/riak.conf"
	#" 
	echo $cmd
	ssh $USERNAME@${SERVERS[k]} $cmd
	if $REPL; then
		cmd="sudo riak-repl nat-map add ${SERVERS[k]} ${IP[k]}"
		echo $cmd
		ssh $USERNAME@${SERVERS[k]} $cmd
	fi
done

for k in $(seq 1 $((${#SERVERS[@]}-1)))
do
	cmd="sudo riak-admin cluster join riak@${SERVERS[0]}"
	ssh $USERNAME@${SERVERS[k]} $cmd
	echo $cmd		
done

cmd="sudo riak-admin cluster plan && sudo riak-admin cluster commit"
ssh $USERNAME@${SERVERS[0]} $cmd

cmd="sudo riak-admin bucket-type create causal '{\"props\":{\"datatype\":\"counter\", \"n_val\":3}}' && sudo riak-admin bucket-type activate causal"
cmd=$cmd" && sudo riak-admin bucket-type create strong '{\"props\":{\"consistent\":true, \"n_val\":3}}' && sudo riak-admin bucket-type activate strong"
cmd=$cmd" && sudo riak-admin bucket-type create last_write_wins '{\"props\":{\"last_write_wins\":true, \"n_val\":3}}' && sudo riak-admin bucket-type activate last_write_wins"
cmd=$cmd" && sudo riak-admin bucket-type create nncounter '{\"props\":{\"consistent\":true, \"n_val\":3}}' && sudo riak-admin bucket-type activate nncounter"
ssh $USERNAME@${SERVERS[0]} $cmd

if $CRDTDB; then
	for k in $(seq 0 $((${#SERVERS[@]}-1)))
	do
		cmd="sudo rm -r "$CRDTDB_DIR"log/"
		echo $cmd
		ssh ubuntu@${SERVERS[k]} $cmd
		cmd="sudo mv $CRDTDB_DIR/etc/vm.args.bak $CRDTDB_DIR/etc/vm.args"
		echo $cmd
		ssh ubuntu@${SERVERS[k]} $cmd
		cmd="sudo sed 's/crdtdb@127.0.0.1/crdtdb@${SERVERS[k]}/' -i.bak $CRDTDB_DIR/etc/vm.args"
		echo $cmd
		ssh ubuntu@${SERVERS[k]} $cmd
	done
	for k in $(seq 0 $((${#SERVERS[@]}-1)))
	do
		cmd="ulimit -n 4096; sudo "$CRDTDB_DIR"bin/crdtdb start"
		echo $cmd
		ssh ubuntu@${SERVERS[k]} $cmd
	done
	sleep 5
	for k in $(seq 1 $((${#SERVERS[@]}-1)))
	do
		cmd="sudo "$CRDTDB_DIR"bin/crdtdb-admin cluster join crdtdb@${SERVERS[0]}"
		echo $cmd
		ssh ubuntu@${SERVERS[k]} $cmd
	done
	sleep 5
	cmd="sudo "$CRDTDB_DIR"bin/crdtdb-admin cluster plan && sudo "$CRDTDB_DIR"bin/crdtdb-admin cluster commit"
	echo $cmd
	ssh ubuntu@${SERVERS[0]} $cmd
fi

# ./new-scrips/make-cluster.bash ./new-scrips/US-WEST ; ./new-scrips/make-cluster.bash ./new-scrips/US-EAST ; ./new-scrips/make-cluster.bash ./new-scrips/EU-WEST