#!/bin/bash

source $1

for k in $(seq 0 $((${#SERVERS[@]}-1))); do
		cmd="sudo mv /etc/riak/riak.conf.bak /etc/riak/riak.conf && sudo sed 's/riak@127.0.0.1/riak@${SERVERS[k]}/' -i.bak /etc/riak/riak.conf && sudo riak start"
		# && sudo sed -e 's/127.0.0.1/${IP[k]}/' -i.bak2 /etc/riak/riak.conf"
		#" 
		echo $cmd
		ssh -t $USERNAME@${SERVERS[k]} $cmd &
	done
	
sleep 10

for k in $(seq 1 $((${#SERVERS[@]}-1)))
	do
		:
		cmd="sudo riak-admin cluster join riak@${SERVERS[0]}" 
		ssh -t $USERNAME@${SERVERS[k]} $cmd
		echo $cmd		
done

cmd="sudo riak-admin cluster plan && sudo riak-admin cluster commit"
ssh -t $USERNAME@${SERVERS[0]} $cmd

for k in $(seq 0 $((${#SERVERS[@]}-1))); do
		cmd="curl -X PUT -H 'Content-Type: application/json' -d '{\"props\":{\"last_write_wins\":true, \"n_val\":3}}' \"http://127.0.0.1:8098/buckets/ITEMS/props\"" 
		#Curl command is different for Riak experiments...
		#cmd="curl -X PUT -H 'Content-Type: application/json' -d '{\"props\":{\"allow_mult\":true}}' \"http://${IP[k]}:8098/buckets/ITEMS/props\"" 
		ssh -t $USERNAME@${SERVERS[k]} $cmd &
done


#####################

for k in $(seq 0 $((${#SERVERS[@]}-1)))
	do
		:
		cmd="sudo mv $CRDTDB_DIR/etc/vm.args.bak $CRDTDB_DIR/etc/vm.args"
		echo $cmd
		ssh ubuntu@${SERVERS[k]} $cmd
		cmd="sudo sed 's/crdtdb@127.0.0.1/crdtdb@${SERVERS[k]}/' -i.bak $CRDTDB_DIR/etc/vm.args" 
		echo $cmd
		ssh ubuntu@${SERVERS[k]} $cmd
	done

for k in $(seq 0 $((${#SERVERS[@]}-1)))
	do
		:
		cmd="ulimit -n 4096; sudo "$CRDTDB_DIR"bin/crdtdb start"
		echo $cmd
		ssh ubuntu@${SERVERS[k]} $cmd
done

sleep 5

for k in $(seq 1 $((${#SERVERS[@]}-1)))
	do
		:
		cmd="sudo "$CRDTDB_DIR"bin/crdtdb-admin cluster join crdtdb@${SERVERS[0]}" 
		echo $cmd
		ssh ubuntu@${SERVERS[k]} $cmd
done

sleep 5

cmd="sudo "$CRDTDB_DIR"bin/crdtdb-admin cluster plan && sudo "$CRDTDB_DIR"bin/crdtdb-admin cluster commit"
echo $cmd
ssh ubuntu@${SERVERS[0]} $cmd

# ./make-cluster.bash US-EAST && ./make-cluster.bash US-WEST && ./make-cluster.bash EU-WEST