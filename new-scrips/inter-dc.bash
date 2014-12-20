SCRIPT_PATH="`dirname \"$0\"`"

source $SCRIPT_PATH/US-EAST
USEAST=${SERVERS[0]}

source $SCRIPT_PATH/US-WEST
USWEST=${SERVERS[0]}

source $SCRIPT_PATH/EU-WEST
EUWEST=${SERVERS[0]}

ssh $USERNAME@$USEAST "sudo riak-repl clustername USEAST"
ssh $USERNAME@$USWEST "sudo riak-repl clustername USWEST"
ssh $USERNAME@$EUWEST "sudo riak-repl clustername EUWEST"

source $SCRIPT_PATH/US-EAST
USEAST=${SERVERS[0]}

for k in $(seq 0 $((${#SERVERS[@]}-1))); 
do
	cmd="sudo riak-repl nat-map add ${SERVERS[k]} ${IP[k]}"
	echo $cmd
	ssh $USERNAME@${SERVERS[k]} $cmd
done

source $SCRIPT_PATH/US-WEST
USWEST=${SERVERS[0]}

for k in $(seq 0 $((${#SERVERS[@]}-1))); 
do
	cmd="sudo riak-repl nat-map add ${SERVERS[k]} ${IP[k]}"
	echo $cmd
	ssh $USERNAME@${SERVERS[k]} $cmd
done

source $SCRIPT_PATH/EU-WEST
EUWEST=${SERVERS[0]}

for k in $(seq 0 $((${#SERVERS[@]}-1))); 
do
	cmd="sudo riak-repl nat-map add ${SERVERS[k]} ${IP[k]}"
	echo $cmd
	ssh $USERNAME@${SERVERS[k]} $cmd
done

ssh $USERNAME@$USEAST "sudo riak-repl connect $USWEST:9080 && sudo riak-repl connect $EUWEST:9080"
ssh $USERNAME@$USWEST "sudo riak-repl connect $USEAST:9080 && sudo riak-repl connect $EUWEST:9080"
ssh $USERNAME@$EUWEST "sudo riak-repl connect $USEAST:9080 && sudo riak-repl connect $USWEST:9080"

ssh $USERNAME@$USEAST "sudo riak-repl realtime enable USWEST && sudo riak-repl realtime enable EUWEST"
ssh $USERNAME@$USWEST "sudo riak-repl realtime enable USEAST && sudo riak-repl realtime enable EUWEST"
ssh $USERNAME@$EUWEST "sudo riak-repl realtime enable USEAST && sudo riak-repl realtime enable USWEST"

ssh $USERNAME@$USEAST "sudo riak-repl realtime start USWEST && sudo riak-repl realtime start EUWEST"
ssh $USERNAME@$USWEST "sudo riak-repl realtime start USEAST && sudo riak-repl realtime start EUWEST"
ssh $USERNAME@$EUWEST "sudo riak-repl realtime start USEAST && sudo riak-repl realtime start USWEST"