#!/bin/bash

USERNAME="ec2-user"
USER_ROOT="/home/ec2-user/crdtdb/"
RIAK_ROOT="/home/ec2-user/riak/"

USERNAME="balegas"
USER_ROOT="/Users/balegas/workspace/erlang/crdtdb/"
RIAK_ROOT="/Users/balegas/workspace/riak/"

#USERNAME="balegas"
#USER_ROOT="/Users/balegas/workspace/erlang/crdtdb/"
#RIAK_ROOT="/Users/balegas/workspace/riak/"

SCRIPTS_ROOT=$USER_ROOT"scripts/"
OUTPUT_DIR=$USER_ROOT"results-sc-time/"


declare -a REGION_NAME=('EU' 'US')

declare -a NODE_NAME=("crdtdb1@127.0.0.1" "crdtdb2@127.0.0.1")
					
declare -a SERVERS=("127.0.0.1"	"127.0.0.1")
					
declare -a NODES_WITH_REGION=("NO_REGION:crdtdb1@127.0.0.1")
					
declare -a CLIENTS=("127.0.0.1"	"127.0.0.1")
					
declare -a ALL_SERVERS=("127.0.0.1") 

BUCKET_TYPE="default"
BUCKET="ITEMS"
INITIAL_VALUE="10000"
N_KEYS="100"
N_VAL="3"
TIME=120
GENERATOR="uniform_generator"
DEC_PROB="0.8"
HTTP_PORT="10018"



declare -a REGIONS=(2)
declare -a CLIENTS_REGION=(160)

#<RiakAddress> <RiakPort> <BucketName> 
create_last_write_wins_bucket(){
	curl -X PUT -H 'Content-Type: application/json' -d '{"props":{"last_write_wins":true, "n_val":'$N_VAL'}}' "http://"$1":"$2"/buckets/"$3"/props"
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
		sleep 10
		dontStop=false
		counter=0
		for h in ${hosts[@]}; do
			echo "Verifying if $h has finished"
			Res="$(ssh $USERNAME@$h pgrep 'beam' | wc -l)"
			#Res="$(ssh $USERNAME@$host ps -C beam --no-headers | wc -l)"
			echo $Res "beam processes are running"
			
			if [ $Res != "4" ]; then
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
	  
 	  cmd=$SCRIPTS_ROOT"init-script ${NODE_NAME[0]} NO_REGION $USER_ROOT ${NODES_WITH_REGION[0]}"
	  ssh $USERNAME@${SERVERS[0]} $cmd
	  cmd=$SCRIPTS_ROOT"reset-script-rc ${NODE_NAME[0]} NO_REGION $N_KEYS $INITIAL_VALUE $USER_ROOT ${NODES_WITH_REGION[0]}"
  	  ssh $USERNAME@${SERVERS[0]} $cmd
	  
	  sleep 10
	  
 	  clients=(${CLIENTS[@]:0:$i})
	  for k in $(seq 0 $((${#clients[@]}-1)))
	  	do
			:
			#Command for strong consistency with key linearizability and Riak-Core
			RESULTS_REGION="$OUTPUT_DIR""${REGION_NAME[k]}'_'$j'_'clients'/'$i'_'regions'/'"
			cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-time-script-rc ${NODE_NAME[0]} $j $N_KEYS $TIME $GENERATOR $DEC_PROB $USER_ROOT $RESULTS_REGION ${REGION_NAME[k]} > $RESULTS_REGION""$filename"
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