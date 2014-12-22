#!/bin/bash

SCRIPT_PATH="`dirname \"$0\"`"
source "$SCRIPT_PATH/config"

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

 shift $((OPTIND-1))

for i in "${THINK_TIMES[@]}"
do
	THINK_TIME=$i
	for j in "${CLIENTS_REGION[@]}"
	do
	  uid=$(date +%s)
	  total_clients=`expr $j \* 3`
		  filename="experiment_R3_C"$total_clients"_K"$N_KEYS"_V"$INITIAL_VALUE
	  all_servers=${ALL_SERVERS[@]}
	  servers=${SERVERS[@]}
	  nodes_with_regions=${NODES_WITH_REGION[@]}
	  
	  echo "SET ADDRESSES"
	  ri=0;
	  for h in ${all_servers[@]}; do
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
		 ri=`expr $ri + 3`
	 	 echo "RESET "$h "CMD" $cmd
		  	 ssh $USERNAME@$h $cmd
		done

	  echo "BROADCAST"
	  #Command for Riak-Core
	  ri=0;
	  for h in ${servers[@]}; do
	  	 cmd=$SCRIPTS_ROOT"broadcast-script-rc ${NODE_NAME[$((ri))]} ${REGION_NAME[$((ri))]} $N_KEYS $USER_ROOT `echo ${nodes_with_regions[@]}`"
		 ri=`expr $ri + 3`
	 	 echo "BROADCAST "$h "CMD" $cmd
		  	 ssh $USERNAME@$h $cmd
		done

	sleep 120

	clients=(${CONNECTIONS_RC[@]})
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
		RESULTS_REGION="$OUTPUT_DIR/$uid/""${REGION_NAME[k]}'_'$total_clients'_'clients'_'$THINK_TIME'_'thinktime'/'"
		cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-time-script-rc $server $j $N_KEYS $TIME $GENERATOR $DEC_PROB $USER_ROOT $RESULTS_REGION ${REGION_NAME[k]} $THINK_TIME > $RESULTS_REGION""$local_filename"
		echo "CLIENT "$client":"  $cmd
		ssh -f $USERNAME@$client $cmd

		IFS=$OIFS	
	done

	now=$(date +"%d-%m-%y %T")
	wait_finish "`echo ${clients[@]}`"
	succs=""
	lats=""
	for k in $(seq 0 $((${#clients[@]}-1)))
	do
	:
		OIFS=$IFS
		IFS=':'	
		tokens=(${clients[$k]})
		client=${tokens[0]}
		
		RESULTS_REGION="$OUTPUT_DIR/$uid/""${REGION_NAME[k]}'_'$total_clients'_'clients'_'$THINK_TIME'_'thinktime'/'"
		local_filename=$filename"_"$k
		succ=$(ssh -f $USERNAME@$client grep "Success:" $RESULTS_REGION$local_filename | sed 's/Success:\([0-9]\+\).*$/\1/')
		lat=$(ssh -f $USERNAME@$client grep "Latency:" $RESULTS_REGION$local_filename | sed 's/Latency:\([0-9]\+\).*$/\1/')
		succs=$succs" "$succ
		lats=$lats" "$lat
		
		IFS=$OIFS
	done
	res=$now" "$OUTPUT_DIR$uid"/ "$j" Threads:$succs |$lats"
	echo $res
	echo $res >> log.txt
	sleep 120
	done
done

echo "Finish"
