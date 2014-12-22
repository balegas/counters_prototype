#!/bin/bash

SCRIPT_PATH="`dirname \"$0\"`"
source "$SCRIPT_PATH/config"

#<RiakAddress> <RiakPort> <BucketName>
create_weakly_consistent_bucket(){
	cmd="curl http://localhost:$2/buckets/$3/props -XPUT -d '{\"props\":{\"allow_mult\":true, \"n_val\":3, \"dvv_enabled\": true}}' -H \"Content-Type: application/json\""
	echo $cmd
	ssh $USERNAME@$1 $cmd
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
	elapsed=$(($TIME - $POLL_DELAY))
	loop=true

	sleep $elapsed

	while $loop; do
#		if [ $elapsed -ge $TIMEOUT ]; then
#			exit
#		fi

		sleep $POLL_DELAY
		elapsed=$(($elapsed + $POLL_DELAY))

		loop=false
		echo $elapsed "seconds:"
		for h in ${hosts[@]}; do
			OIFS=$IFS
			IFS=':'			
			tokens=($h)
			client=${tokens[0]}
			IFS=$OIFS
			
			Res="$(ssh $USERNAME@$client pgrep 'beam' | wc -l)"
			echo "    $client - $Res beam processes are running"
			
			if [ $Res != "0" ]; then
				loop=true
			fi
		done

	done
	echo "All clients have stopped"
}

#Process options
while getopts "v:t:" optname
  do
    case "$optname" in
      "v")
		  INITIAL_VALUE=$OPTARG
        ;;
      "t")
  		  THREADS=$OPTARG
  		  ;;
      "?")
        echo "Unknown option $OPTARG"
        exit
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

for i in "${THINK_TIMES[@]}"
do
	THINK_TIME=$i
	for j in "${CLIENTS_REGION[@]}"
	do
		total_clients=`expr $j \* 3`
	  	filename="experiment_R3_C"$total_clients"_K"$N_KEYS"_V"$INITIAL_VALUE

	  	servers=(${SERVERS[@]})
	  	bucket=$(date +%s)
		  
	  	echo "Bucket: "$CONSISTENCY" "$bucket
		echo "Initial value: "$INITIAL_VALUE
	  
		if [ $CONSISTENCY == "weak" ]; then
			for k in $(seq 0 $((${#servers[@]}-1)))
			do
				create_weakly_consistent_bucket ${SERVERS[k]} $HTTP_PORT $bucket
			done
		fi
	  
		for k in $(seq 0 $((${#servers[@]}-1)))
		do
		  	cmd=$SCRIPTS_ROOT"reset-script-counter $CONSISTENCY $INITIAL_VALUE $bucket $CONSISTENCY $N_KEYS ${SERVERS[k]} ${RIAK_PB_PORT[k]} $USER_ROOT `echo ${REGION_NAME[@]}`"
		  	echo $cmd
		  	ssh $USERNAME@${SERVERS[k]} $cmd
	  	done

	  	echo "Reset Complete"
		sleep 120
	  
	  clients=(${CONNECTIONS[@]})
	  for k in $(seq 0 $((${#clients[@]}-1)))
	  do
			
			OIFS=$IFS
			IFS=':'			
			tokens=(${clients[$k]})
			client=${tokens[0]}
			server=${tokens[1]}
			
			local_filename=$filename"_"$k
			
			RESULTS_REGION="$OUTPUT_DIR/$bucket/""${REGION_NAME[k]}'_'$total_clients'_'clients'_'$THINK_TIME'_'thinktime'/'"
			cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-time-script-wc $server ${RIAK_PB_PORT[k]} $CONSISTENCY $bucket $CONSISTENCY $j $N_KEYS $TIME $GENERATOR $DEC_PROB $USER_ROOT $RESULTS_REGION ${REGION_NAME[k]} $THINK_TIME > $RESULTS_REGION""$local_filename"
			echo $cmd
			ssh -f $USERNAME@$client $cmd
			
			IFS=$OIFS
		done
			
		now=$(date +"%d-%m-%y %T")
		wait_finish "`echo ${clients[@]}`"
		succs=""
		lats=""
		for k in $(seq 0 $((${#clients[@]}-1)))
		do
			OIFS=$IFS
			IFS=':'	
			tokens=(${clients[$k]})
			client=${tokens[0]}
				
			RESULTS_REGION="$OUTPUT_DIR/$bucket/""${REGION_NAME[k]}'_'$total_clients'_'clients'_'$THINK_TIME'_'thinktime'/'"
			local_filename=$filename"_"$k
			succ=$(ssh -f $USERNAME@$client grep "Success:" $RESULTS_REGION$local_filename | sed 's/Success:\([0-9]\+\).*$/\1/')
			lat=$(ssh -f $USERNAME@$client grep "Latency:" $RESULTS_REGION$local_filename | sed 's/Latency:\([0-9]\+\).*$/\1/')
			succs=$succs" "$succ
			lats=$lats" "$lat

			IFS=$OIFS
		done
		res=$now" "$OUTPUT_DIR$bucket" "$j" Threads:$succs |$lats"
		echo $res
		echo $res >> log.txt
		sleep 120
	done
done

#shift $(( ${OPTIND} - 1 )); echo "${*}"
echo "Finish"
