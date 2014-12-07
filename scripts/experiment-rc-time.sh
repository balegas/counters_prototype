#!/bin/bash

#USERNAME="delphinus"
#USER_ROOT="/home/$USERNAME/SyncFree/balegas_github/counters_prototype/"

USERNAME="ubuntu"
USER_ROOT="/home/$USERNAME/counters_prototype/"

LOCAL_ROOT="/home/delphinus/SyncFree/balegas_github/counters_prototype/"

SCRIPTS_ROOT=$USER_ROOT"scripts/"
OUTPUT_DIR=$USER_ROOT"results-rc-time-dec-3s-3p-3c/"

REGION_NAME=(
	"EU-WEST_1"
	"EU-WEST_2"
	"EU-WEST_3"
	)
	
SERVERS=(
#	"127.0.0.1"

	"ec2-54-171-255-223.eu-west-1.compute.amazonaws.com"
	"ec2-54-194-142-127.eu-west-1.compute.amazonaws.com"
	"ec2-54-76-138-80.eu-west-1.compute.amazonaws.com"
	)

NODE_NAME=(
#	"crdtdb1@"${SERVERS[0]}

	"crdtdb@"${SERVERS[0]}
	"crdtdb@"${SERVERS[1]}
	"crdtdb@"${SERVERS[2]}
	)

NODES_WITH_REGION=(
	${REGION_NAME[0]}":"${NODE_NAME[0]}
	${REGION_NAME[1]}":"${NODE_NAME[1]}
	${REGION_NAME[2]}":"${NODE_NAME[2]}
	)
	
CLIENT_NODES=(
#	"127.0.0.1"

	"ec2-54-194-128-186.eu-west-1.compute.amazonaws.com"
	"ec2-54-171-248-121.eu-west-1.compute.amazonaws.com"
	"ec2-54-171-248-121.eu-west-1.compute.amazonaws.com"
	)

CLIENTS=(
	${CLIENT_NODES[0]}":"${NODE_NAME[0]}
	${CLIENT_NODES[1]}":"${NODE_NAME[1]}
	${CLIENT_NODES[2]}":"${NODE_NAME[2]}
	)

BUCKET_TYPE="default"
BUCKET="ITEMS"
INITIAL_VALUE="9999999999"
N_KEYS="1"
N_VAL="3"
TIME=120
TIMEOUT=$(($TIME + 30))
POLL_DELAY=10
GENERATOR="uniform_generator"
DEC_PROB="1.0"

declare -a CLIENTS_REGION=(5 25 50 100)

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
		if [ $elapsed -ge $TIMEOUT ]; then
			echo $2" ### FAILED ###" >> log.txt
			kill_all "`echo ${CLIENTS[@]}`"
			sleep 10
			bash "$LOCAL_ROOT/new-scrips/stop.bash" $3
			sleep 30
			bash "$LOCAL_ROOT/new-scrips/make-cluster.bash" $3
			exit
		fi

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
while getopts "v:c:kKidt:" optname
  do
    case "$optname" in
      "v")
		  INITIAL_VALUE=$OPTARG
        ;;
      "i")
			echo "Copy source to remote nodes"
			hosts=""
			for h in ${SERVERS[@]}; do
				hosts=$hosts" -H "$USERNAME"@"$h" "
			done
			prsync="parallel-rsync "$hosts" -r "
			cmd=$prsync" "$LOCAL_ROOT"src "$USER_ROOT
			$cmd
			cmd=$prsync" "$LOCAL_ROOT"scripts "$USER_ROOT
			$cmd
			
			hosts=""
			for h in ${CLIENT_NODES[@]}; do
				hosts=$hosts" -H "$USERNAME"@"$h" "
			done
			prsync="parallel-rsync "$hosts" -r "
			cmd=$prsync" "$LOCAL_ROOT"src "$USER_ROOT
			$cmd
			cmd=$prsync" "$LOCAL_ROOT"scripts "$USER_ROOT
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
  
echo $1
echo "Bucket: "$BUCKET_TYPE" "$BUCKET
echo "Initial value: "$INITIAL_VALUE

for j in "${CLIENTS_REGION[@]}"
   do
      :
	  total_clients=`expr $j \* 3`
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
	  
	  sleep 20

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

	now=$(date +"%d-%m-%y %T")
	res=$now" "$OUTPUT_DIR" "$j" Threads:"
	wait_finish "`echo ${clients[@]}`" "$res" $1
	for k in $(seq 0 $((${#clients[@]}-1)))
		do
		:
		
			OIFS=$IFS
			IFS=':'	
			tokens=(${clients[$k]})
			client=${tokens[0]}
			
			RESULTS_REGION="$OUTPUT_DIR""${REGION_NAME[k]}'_'$total_clients'_'clients'/'3_regions'/'"
			local_filename=$filename"_"$k
			succs=$(ssh -f $USERNAME@$client grep "Success:" $RESULTS_REGION$local_filename | sed 's/Success:\([0-9]\+\).*$/\1/')
			res=$res" "$succs
			
			IFS=$OIFS
		done
	echo $res
	echo $res >> log.txt
	sleep 180
   done
   
echo "Finish"