#!/bin/bash

USERNAME="ubuntu"
USER_ROOT="/home/$USERNAME/counters_prototype/"
RIAK_ROOT="/home/$USERNAME/riak/"

SRC="/Users/balegas/workspace/erlang/counters_prototype/src/"

SCRIPTS_ROOT=$USER_ROOT"scripts/"
OUTPUT_DIR=$USER_ROOT"results-rc-time-dec-1key-10c/"

REGION_NAME=(
	"EU-WEST_1"
	"EU-WEST_2"
	)
	
SERVERS=(
	"ec2-54-171-255-223.eu-west-1.compute.amazonaws.com"
	"ec2-54-194-142-127.eu-west-1.compute.amazonaws.com"
#	"ec2-54-76-138-80.eu-west-1.compute.amazonaws.com"
	
	)

NODE_NAME=(
	"crdtdb@"${SERVERS[0]}
	"crdtdb@"${SERVERS[1]}
	)

NODES_WITH_REGION=(
	${REGION_NAME[0]}":"${NODE_NAME[0]}
	${REGION_NAME[1]}":"${NODE_NAME[1]}
	)
	
CLIENT_NODES=(
	"ec2-54-194-128-186.eu-west-1.compute.amazonaws.com"
	"ec2-54-171-248-121.eu-west-1.compute.amazonaws.com"
#	"ec2-54-171-248-121.eu-west-1.compute.amazonaws.com"
	)

CLIENTS=(
	${CLIENT_NODES[0]}":"${NODE_NAME[0]}
	${CLIENT_NODES[1]}":"${NODE_NAME[1]}

	)

BUCKET_TYPE="default"
BUCKET="ITEMS"
INITIAL_VALUE="9999999999"
N_KEYS="1"
N_VAL="3"
HTTP_PORT="8098"
TIME=180
GENERATOR="uniform_generator"
DEC_PROB="1.0"


declare -a CLIENTS_REGION=(5 10 15 20 30 40 50)

#<RiakAddress> <RiakPort> <BucketName> 
create_last_write_wins_bucket(){
	curl -X PUT -H 'Content-Type: application/json' -d '{"props":{"last_write_wins":true, "n_val":'$N_VAL'}}' "http://"$1":"$2"/buckets/"$3"/props"
}

#<Clients> #<Command>
ssh_command() {
	hosts=($1)
	for h in ${hosts[@]}; do
		OIFS=$IFS
		IFS=':'			
		tokens=($h)
		client=${tokens[0]}
		echo "client  " $client
		ssh -t $USERNAME@$client $2
		IFS=$OIFS
	done
}

#<Clients>
kill_all() {
#	cmd="rm -fr crdtdb/results/*"
	cmd="killall beam.smp"
	ssh_command "$1" "$cmd"
	echo "All clients have stopped"
}

#<Clients>
wait_finish() {
	hosts=($1)
	dontStop=true
	dontStop=true
	while $dontStop; do
		sleep 10
		dontStop=false
		counter=0

		
		for h in ${hosts[@]}; do
			
			OIFS=$IFS
			IFS=':'			
			tokens=($h)
			client=${tokens[0]}
			IFS=$OIFS
			
			echo "Verifying if $client has finished"
			Res="$(ssh $USERNAME@$client pgrep 'beam' | wc -l)"
			#Res="$(ssh $USERNAME@$host ps -C beam --no-headers | wc -l)"
			echo $Res "beam processes are running"
			
			if [ $Res != "0" ]; then
				dontStop=true
			fi
		done



	done
	echo "All clients have stopped"
}



#Process options
while getopts "v:c:kKidt:" optname
  do
    case "$optname" in
      "v")
		  INITIAL_VALUE=$OPTARG
		  ;;
      "c")
	  	case $OPTARG in
		  'strong')
		  	BUCKET_TYPE="STRONG"
			create_strong_bucket ${SERVERS[0]} $BUCKET
		  ;;
		  'eventual') 
		  	BUCKET_TYPE="default" 
			create_last_write_wins_bucket ${SERVERS[0]} $BUCKET
			;;
	  	esac
        ;;
      "i")
			echo "Copy source to remote nodes"
			hosts=""
			for h in ${SERVERS[@]}; do
				hosts=$hosts" -H "$USERNAME"@"$h" "
			done
			cmd="prsync "$hosts" -r "
			cmd=$cmd" "$SRC" "$USER_ROOT"/src"
			$cmd
			
			hosts=""
			for h in ${CLIENT_NODES[@]}; do
				hosts=$hosts" -H "$USERNAME"@"$h" "
			done
			cmd="prsync "$hosts" -r "
			cmd=$cmd" "$SRC" "$USER_ROOT"/src"
			$cmd
			
			exit
        ;;
	  "d")
		
	  	;;
      "t")
  		  THREADS=$OPTARG
  		  ;;
	  "k")
		  kill_all "`echo ${CLIENTS[@]}`"
  		  exit
		  ;;
      "?")
        echo "Unknown option $OPTARG"
        ;;
      ":")
        echo "No argument value for option $OPTARG"
        ;;
      *)
      # Should not occur
        echo "Unknown error while processing options"
        ;;
    esac
  done
  
echo "Bucket: "$BUCKET_TYPE" "$BUCKET
echo "Initial value: "$INITIAL_VALUE

#create_last_write_wins_bucket ${SERVERS[0]} $HTTP_PORT $BUCKET


for j in "${CLIENTS_REGION[@]}"
   do
      :
	  total_clients="1"
  	  filename="experiment_R3_C"$total_clients"_K"$N_KEYS"_V"$INITIAL_VALUE
	  servers=${SERVERS[@]}
	  nodes_with_regions=${NODES_WITH_REGION[@]}
	  
	  echo "SET ADDRESSES"
	  ri=0;
	  for h in ${servers[@]}; do
	 	cmd=$SCRIPTS_ROOT"init-script ${NODE_NAME[$((ri))]} ${REGION_NAME[$((ri))]} $USER_ROOT `echo ${nodes_with_regions[@]}`"
  	  	ri=`expr $ri + 1`
	  	echo "INIT "$h "CMD" $cmd
		echo $cmd
	  	ssh $USERNAME@$h $cmd
	  done

	  echo "RESET"
	  #Command for Riak-Core
	  ri=0;
	  for h in ${servers[@]}; do
	  	 cmd=$SCRIPTS_ROOT"reset-script-rc ${NODE_NAME[$((ri))]} ${REGION_NAME[$((ri))]} $N_KEYS $INITIAL_VALUE $USER_ROOT `echo ${nodes_with_regions[@]}`"
		 ri=`expr $ri + 1`
     	 echo "RESET "$h "CMD" $cmd
 	  	 ssh $USERNAME@$h $cmd
	  done
	  
	  sleep 3

 	  clients=(${CLIENTS[@]})
	  for k in $(seq 0 $((${#clients[@]}-1)))
	  	do
			:
			
			OIFS=$IFS
			IFS=':'			
			tokens=(${clients[$k]})
			client=${tokens[0]}
			server=${tokens[1]}
			
			local_filename=$filename"_"$k
			
			#Command for strong consistency with key linearizability and Riak-Core
			RESULTS_REGION="$OUTPUT_DIR""${REGION_NAME[k]}'_'$total_clients'_'clients'/'3_regions'/'"
			cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-time-script-rc $server $j $N_KEYS $TIME $GENERATOR $DEC_PROB $USER_ROOT $RESULTS_REGION ${REGION_NAME[k]} > $RESULTS_REGION""$local_filename"
			echo "CLIENT "$client":"  $cmd
			ssh -f $USERNAME@$client $cmd
			
			IFS=$OIFS
			
		done
		sleep 5
		wait_finish "`echo ${clients[@]}`"
		sleep 3
	done

#shift $(( ${OPTIND} - 1 )); echo "${*}"
echo "Finish"