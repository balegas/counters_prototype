#!/bin/bash

USERNAME="ubuntu"
USER_ROOT="/home/$USERNAME/crdtdb-git/"
RIAK_ROOT="/home/$USERNAME/riak/"

SCRIPTS_ROOT=$USER_ROOT"scripts/"
OUTPUT_DIR=$USER_ROOT"results-rc-time-contention-durable/"

REGION_NAME=(
	"US-EAST"
	"US-WEST"
	"EU-WEST"
	"US-EAST"
	"US-WEST"
	"EU-WEST"
	"US-EAST"
	"US-WEST"
	"EU-WEST"
	)

NODE_NAME=(
	"crdtdb@ec2-54-208-239-199.compute-1.amazonaws.com"
	"crdtdb@ec2-54-183-53-168.us-west-1.compute.amazonaws.com"
	"crdtdb@ec2-54-76-62-175.eu-west-1.compute.amazonaws.com"
	)
					
SERVERS=(
	"ec2-54-208-239-199.compute-1.amazonaws.com"
	"ec2-54-183-53-168.us-west-1.compute.amazonaws.com"
	"ec2-54-76-62-175.eu-west-1.compute.amazonaws.com"
	)
					
NODES_WITH_REGION=(
	"US-EAST:crdtdb@ec2-54-208-239-199.compute-1.amazonaws.com"
	"US-WEST:crdtdb@ec2-54-183-53-168.us-west-1.compute.amazonaws.com"
	"EU-WEST:crdtdb@ec2-54-76-62-175.eu-west-1.compute.amazonaws.com"
	)
					
CLIENTS=(
	"ec2-54-208-241-197.compute-1.amazonaws.com:crdtdb@ec2-54-208-239-199.compute-1.amazonaws.com"
	"ec2-54-183-64-143.us-west-1.compute.amazonaws.com:crdtdb@ec2-54-183-53-27.us-west-1.compute.amazonaws.com"
	"ec2-54-72-55-184.eu-west-1.compute.amazonaws.com:crdtdb@ec2-54-76-62-175.eu-west-1.compute.amazonaws.com"
	"ec2-54-208-241-212.compute-1.amazonaws.com:crdtdb@ec2-54-208-239-235.compute-1.amazonaws.com"
	"ec2-54-183-47-17.us-west-1.compute.amazonaws.com:crdtdb@ec2-54-183-53-168.us-west-1.compute.amazonaws.com"
	"ec2-54-76-41-215.eu-west-1.compute.amazonaws.com:crdtdb@ec2-54-76-62-11.eu-west-1.compute.amazonaws.com"
	"ec2-54-86-236-255.compute-1.amazonaws.com:crdtdb@ec2-54-208-102-195.compute-1.amazonaws.com"
	"ec2-54-183-64-144.us-west-1.compute.amazonaws.com:crdtdb@ec2-54-183-54-249.us-west-1.compute.amazonaws.com"
	"ec2-54-76-61-204.eu-west-1.compute.amazonaws.com:crdtdb@ec2-54-76-62-174.eu-west-1.compute.amazonaws.com"
	)

BUCKET_TYPE="default"
BUCKET="ITEMS"
INITIAL_VALUE="9999999999"
N_KEYS="1"
N_VAL="3"
HTTP_PORT="8098"
TIME=120
GENERATOR="uniform_generator"
DEC_PROB="0.8"


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
while getopts "v:c:kKrdt:" optname
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
      "r")
        echo "Get results from clients."
		copy_files "`echo ${CLIENTS[@]}`"
		exit
        ;;
	  "d")
		create_cluster "localhost"
	  	;;
      "t")
  		  THREADS=$OPTARG
  		  ;;
	  "k")
		  kill_all "`echo ${CLIENTS[@]}`"
  		  exit
		  ;;
	  "K")
		  restart_all_servers "`echo ${ALL_SERVERS[@]}`"
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