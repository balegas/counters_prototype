#!/bin/bash

#TODO #Regions is hard-coded
#functions depend on the structure of the clients...

USERNAME="ubuntu"
USER_ROOT="/home/$USERNAME/crdtdb-git/"
RIAK_ROOT="/home/$USERNAME/riak/"

SCRIPTS_ROOT=$USER_ROOT"scripts/"
OUTPUT_DIR=$USER_ROOT"results-wc-empty-one-counter/"


REGION_NAME=(
	"US-EAST"
	"US-WEST"
	"EU-WEST"
	)

SERVERS=(
	"ec2-23-20-12-80.compute-1.amazonaws.com"
	"ec2-54-183-40-147.us-west-1.compute.amazonaws.com"
	"ec2-54-76-22-29.eu-west-1.compute.amazonaws.com"
	)
					
CLIENTS=(
	"ec2-54-204-76-239.compute-1.amazonaws.com:ec2-23-20-12-80.compute-1.amazonaws.com"
	"ec2-54-183-36-135.us-west-1.compute.amazonaws.com:ec2-54-183-40-147.us-west-1.compute.amazonaws.com"
	"ec2-54-76-25-7.eu-west-1.compute.amazonaws.com:ec2-54-76-22-29.eu-west-1.compute.amazonaws.com"
#	"ec2-54-242-53-237.compute-1.amazonaws.com:ec2-50-16-108-189.compute-1.amazonaws.com"
#	"ec2-54-183-38-176.us-west-1.compute.amazonaws.com:ec2-54-183-40-239.us-west-1.compute.amazonaws.com"
#	"ec2-54-76-27-99.eu-west-1.compute.amazonaws.com:ec2-54-76-32-199.eu-west-1.compute.amazonaws.com"
#	"ec2-54-205-199-82.compute-1.amazonaws.com:ec2-54-198-38-55.compute-1.amazonaws.com"
#	"ec2-54-183-40-182.us-west-1.compute.amazonaws.com:ec2-54-183-37-56.us-west-1.compute.amazonaws.com"
#	"ec2-54-76-3-151.eu-west-1.compute.amazonaws.com:ec2-54-72-232-53.eu-west-1.compute.amazonaws.com"	
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

BUCKET_TYPE="default"
BUCKET="ITEMS"
DEFAULT_KEY="0"
INITIAL_VALUE="1000"
N_KEYS="1"
N_VAL="3"
HTTP_PORT="8098"

# TT 300, Threshold 300

CLIENTS_REGION=(1 10 20)

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
	  total_clients=`expr $j \* 1`
  	  filename="experiment_R3_C"$total_clients"_K"$N_KEYS"_V"$INITIAL_VALUE
	  servers=${SERVERS[@]}
	  bucket=$(date +%s)
	  
	  create_last_write_wins_bucket ${SERVERS[0]} $HTTP_PORT $bucket
	  cmd=$SCRIPTS_ROOT"reset-script-counter $INITIAL_VALUE $bucket $BUCKET_TYPE $DEFAULT_KEY ${SERVERS[0]} ${RIAK_PB_PORT[0]} $USER_ROOT"
	  echo $cmd
	  ssh $USERNAME@${SERVERS[0]} $cmd
	  
	  sleep 10
	  
 	  clients=(${CLIENTS[@]})
	  servers=(${SERVERS[@]})
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
			cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-script-counter $server ${RIAK_PB_PORT[k]} $j $USER_ROOT $bucket $BUCKET_TYPE ${REGION_NAME[k]} $RESULTS_REGION > $RESULTS_REGION""$local_filename"
			echo $cmd
			ssh -f $USERNAME@$client $cmd
			
			IFS=$OIFS
		done
		sleep 5
		wait_finish "`echo ${clients[@]}`"
		sleep 60
	done

#shift $(( ${OPTIND} - 1 )); echo "${*}"
echo "Finish"