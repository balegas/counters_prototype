#!/bin/bash

USERNAME="ubuntu"
USER_ROOT="/home/$USERNAME/crdtdb-git/"
RIAK_ROOT="/home/$USERNAME/riak/"

SCRIPTS_ROOT=$USER_ROOT"scripts/"
OUTPUT_DIR=$USER_ROOT"results-sc/"

declare -a REGION_NAME=(
						'US-EAST'
						'US-WEST'
						'EU-WEST'
						)


declare -a NODE_NAME=(
					"crdtdb@ec2-54-80-25-204.compute-1.amazonaws.com"
					)
					
declare -a SERVERS=(
					"ec2-54-80-25-204.compute-1.amazonaws.com"
					)
					
declare -a NODES_WITH_REGION=(
					"NO_REGION:crdtdb@ec2-54-80-25-204.compute-1.amazonaws.com"
					)
					

declare -a CLIENTS=(
					"ec2-23-20-7-106.compute-1.amazonaws.com"
					"ec2-54-183-11-146.us-west-1.compute.amazonaws.com"
					"ec2-54-72-230-126.eu-west-1.compute.amazonaws.com"
					)
					
declare -a ALL_SERVERS=(
					"ec2-54-227-40-196.compute-1.amazonaws.com"
					) 

BUCKET_TYPE="default"
BUCKET="ITEMS"
INITIAL_VALUE="1000"
N_KEYS="1"
N_VAL="3"
HTTP_PORT="8098"



declare -a REGIONS=(3)
declare -a CLIENTS_REGION=(10 20)

#<RiakAddress> <BucketName> 
create_last_write_wins_bucket(){
	curl -X PUT -H 'Content-Type: application/json' -d '{"props":{"last_write_wins":true, "n_val":'$N_VAL'}}' "http://"$1":"$HTTP_PORT"/buckets/"$2"/props"
}


#<RiakAddress> <BucketName> 
create_strong_bucket(){
	cmd=$RIAK_ROOT"/bin/riak-admin bucket-type create "$2" '{\"props\": {\"consistent\":true, \"n_val\":$N_VAL}}'; "$RIAK_ROOT"/bin/riak-admin bucket-type activate $2"
	echo "Execute: "$cmd
	ssh -f $USERNAME"@"$1 $cmd
}


#<Clients>
kill_all() {
#	cmd="rm -fr crdtdb/results/*"
	cmd="killall beam.smp"
	ssh_command "$1" "$cmd"
	echo "All clients have stopped"
}

restart_all_servers() {
	cmd="killall beam.smp ; rm -fr crdtdb/dev/dev1/log/* ; ulimit -n 4096 && riak/bin/riak start && crdtdb/dev/dev1/bin/crdtdb start"
	#cmd="killall beam.smp"
	#cmd="sudo reboot"
	#cmd="ulimit -n 4096 && riak/bin/riak start && crdtdb/dev/dev1/bin/crdtdb start"
	ssh_command "$1" "$cmd"
	echo "All servers have restarted"
}
	  	  

#<Clients>
copy_files() {
	echo $1
	hosts=($1)
	for h in ${hosts[@]}; do
		mkdir -p results
		rsync -avz -e ssh $USERNAME@$h:$OUTPUT_DIR results
	done
}


#<Clients> #<Command>
ssh_command() {
	echo "Addresses: "$1
	echo "Command: "$2
	hosts=($1)
	for h in ${hosts[@]}; do
		ssh -t $USERNAME@$h $2
	done
}

#<Clients>
wait_finish() {
	hosts=($1)
	dontStop=true
	dontStop=true
	while $dontStop; do
		dontStop=false
		counter=0
		for h in ${hosts[@]}; do
			sleep 10
			echo "Verifying if $h has finished"
			Res="$(ssh $USERNAME@$h pgrep 'beam' | wc -l)"
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

create_last_write_wins_bucket ${SERVERS[0]} $HTTP_PORT $BUCKET

for i in "${REGIONS[@]}"
do
   :
   for j in "${CLIENTS_REGION[@]}"
   do
      :
  	  filename="experiment_R"$i"_C"$j"_K"$N_KEYS"_V"$INITIAL_VALUE
	  servers=${SERVERS[@]:0:$(($i))}
	  nodes_with_regions=${NODES_WITH_REGION[@]:0:$(($i))}
	  
	  echo "SET ADDRESSES"
 	  cmd=$SCRIPTS_ROOT"init-script ${NODE_NAME[0]} NO_REGION $USER_ROOT ${NODES_WITH_REGION[0]}"
	  ssh $USERNAME@${SERVERS[0]} $cmd
	  cmd=$SCRIPTS_ROOT"reset-script-rc ${NODE_NAME[0]} NO_REGION $N_KEYS $INITIAL_VALUE $USER_ROOT ${NODES_WITH_REGION[0]}"
  	  ssh $USERNAME@${SERVERS[0]} $cmd
	  
	  sleep 10
	  
 	  clients=(${CLIENTS[@]:0:$i})
	  servers=(${SERVERS[@]:0:$i})
	  for k in $(seq 0 $((${#clients[@]}-1)))
	  	do
			:
			#Command for strong consistency with key linearizability and Riak-Core
			RESULTS_REGION="$OUTPUT_DIR""${REGION_NAME[k]}'_'$j'_'clients'/'$i'_'regions'/'"
			cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-script-rc ${NODE_NAME[0]} $j $USER_ROOT $RESULTS_REGION > $RESULTS_REGION""$filename"" ${REGION_NAME[k]}"
			echo $cmd
			ssh -f $USERNAME@${clients[k]} $cmd
			
		done
		sleep 5
		wait_finish "`echo ${clients[@]}`"
		sleep 60
	done
done

#shift $(( ${OPTIND} - 1 )); echo "${*}"
echo "Finish"