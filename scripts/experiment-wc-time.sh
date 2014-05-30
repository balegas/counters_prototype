#!/bin/bash

USERNAME="ubuntu"
USER_ROOT="/home/$USERNAME/crdtdb-git/"
RIAK_ROOT="/home/$USERNAME/riak/"

SCRIPTS_ROOT=$USER_ROOT"scripts/"
OUTPUT_DIR=$USER_ROOT"results-wc-time-contention/"




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
	
	
SERVERS=(
	"ec2-54-208-213-100.compute-1.amazonaws.com"
	"ec2-54-183-66-125.us-west-1.compute.amazonaws.com"
	"ec2-54-76-92-180.eu-west-1.compute.amazonaws.com"
	)
					
CLIENTS=(
	"ec2-54-84-34-26.compute-1.amazonaws.com:ec2-54-208-213-100.compute-1.amazonaws.com"
	"ec2-54-183-37-207.us-west-1.compute.amazonaws.com:ec2-54-183-66-125.us-west-1.compute.amazonaws.com"
	"ec2-54-76-52-148.eu-west-1.compute.amazonaws.com:ec2-54-76-92-180.eu-west-1.compute.amazonaws.com"
	"ec2-54-209-43-239.compute-1.amazonaws.com:ec2-54-208-177-248.compute-1.amazonaws.com"
	"ec2-54-183-64-159.us-west-1.compute.amazonaws.com:ec2-54-183-66-124.us-west-1.compute.amazonaws.com"
	"ec2-54-76-106-82.eu-west-1.compute.amazonaws.com:ec2-54-76-97-248.eu-west-1.compute.amazonaws.com"
	"ec2-54-208-253-138.compute-1.amazonaws.com:ec2-54-208-186-220.compute-1.amazonaws.com"
	"ec2-54-183-63-140.us-west-1.compute.amazonaws.com:ec2-54-183-66-123.us-west-1.compute.amazonaws.com"
	"ec2-54-76-106-85.eu-west-1.compute.amazonaws.com:ec2-54-76-97-142.eu-west-1.compute.amazonaws.com"
	)

	
					
RIAK_PB_PORT=(
	"8087"
	"8087"
	"8087"
	"8087"
	"8087"
	"8087"
	"8087"
	"8087"
	"8087"
	)

ALL_SERVERS=(
	"127.0.0.1"
	"127.0.0.1"
	) 
	
BUCKET_TYPE="default"
BUCKET="ITEMS"
DEFAULT_KEY="0"
INITIAL_VALUE="900000000"
N_KEYS="1"
N_VAL="3"
TIME=120
GENERATOR="uniform_generator"
DEC_PROB="0.8"
HTTP_PORT="8098"



declare -a CLIENTS_REGION=(100)

#<RiakAddress> <RiakPort> <BucketName> 
create_last_write_wins_bucket(){
	curl -i http://$1:$2/buckets/$3/props  -X PUT -d '{"props":{"allow_mult":true}}' -H "Content-Type: application/json"
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


for j in "${CLIENTS_REGION[@]}"
   do
      :

	  total_clients=`expr $j \* 3`
  	  filename="experiment_R3_C"$total_clients"_K"$N_KEYS"_V"$INITIAL_VALUE

  	  servers=(${SERVERS[@]})
	  bucket=$(date +%s)
	  
	  for k in $(seq 0 $((${#servers[@]}-1)))
	  	do
			:
			create_last_write_wins_bucket ${SERVERS[k]} $HTTP_PORT $bucket
		done
	  cmd=$SCRIPTS_ROOT"reset-script-counter $INITIAL_VALUE $bucket $BUCKET_TYPE $DEFAULT_KEY ${SERVERS[0]} ${RIAK_PB_PORT[0]} $USER_ROOT"
	  echo $cmd
	  ssh $USERNAME@${SERVERS[0]} $cmd
	  
	  sleep 15
	  
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
			
			RESULTS_REGION="$OUTPUT_DIR""${REGION_NAME[k]}'_'$total_clients'_'clients'/'3_regions'/'"
			cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-time-script-wc $server ${RIAK_PB_PORT[k]} $bucket $BUCKET_TYPE $j $N_KEYS $TIME $GENERATOR $DEC_PROB $USER_ROOT $RESULTS_REGION ${REGION_NAME[k]} > $RESULTS_REGION""$local_filename"
			echo $cmd
			ssh -f $USERNAME@$client $cmd
			
			IFS=$OIFS
		done
		sleep 5
		wait_finish "`echo ${clients[@]}`"
		sleep 30
	done

#shift $(( ${OPTIND} - 1 )); echo "${*}"
echo "Finish"