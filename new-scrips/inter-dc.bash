source US-EAST
USEAST=${SERVERS[0]}

source US-WEST
USWEST=${SERVERS[0]}

source EU-WEST
EUWEST=${SERVERS[0]}

ssh -t $USERNAME@$USEAST "sudo riak-repl clustername US-EAST && sudo riak-repl connect $USWEST:9080 && sudo riak-repl connect $EUWEST:9080 && sudo riak-repl realtime enable US-WEST && sudo riak-repl realtime enable EU-WEST && sudo riak-repl realtime start EU-WEST && sudo riak-repl realtime start US-WEST"

ssh -t $USERNAME@$USWEST "sudo riak-repl clustername US-WEST && sudo riak-repl connect $USEAST:9080 && sudo riak-repl connect $EUWEST:9080 && sudo riak-repl realtime enable US-EAST && sudo riak-repl realtime enable EU-WEST && sudo riak-repl realtime start US-EAST && sudo riak-repl realtime start EU-WEST"

ssh -t $USERNAME@$EUWEST "sudo riak-repl clustername EU-WEST && sudo riak-repl connect $USEAST:9080 && sudo riak-repl connect $USWEST:9080 && sudo riak-repl realtime enable US-EAST && sudo riak-repl realtime enable US-WEST && sudo riak-repl realtime start US-EAST && sudo riak-repl realtime start US-WEST"

#./inter-dc.bash US-EAST && ./inter-dc.bash US-WEST && ./inter-dc.bash EU-WEST