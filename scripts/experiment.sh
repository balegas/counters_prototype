#!/bin/bash

#-c <ConsistencyLevel> -t <numClientsByRegion> -v <initialValue> <primaryAddress>

#ConsistencyLevel - Selects the appropriate bucket (Strong/Eventual)
#Address is written in the form "ID:ADDRESS"


# rm -fr riak/data/ && riak/bin/riak start
# riak/bin/riak-admin cluster join riak@ec2-54-216-124-119.eu-west-1.compute.amazonaws.com

USERNAME="ec2-user"
USER_ROOT="/home/ec2-user/crdtdb/"
RIAK_ROOT="/home/ec2-user/riak/"

#USERNAME="balegas"
#USER_ROOT="/Users/balegas/workspace/erlang/crdtdb/"
#RIAK_ROOT="/Users/balegas/workspace/riak/"

SCRIPTS_ROOT=$USER_ROOT"scripts/"
OUTPUT_DIR=$USER_ROOT"results/"


declare -a REGION_NAME=('eu')
declare -a CLIENTS=('ec2-54-227-223-214.compute-1.amazonaws.com')
declare -a SERVERS=('id0:ec2-54-220-2-228.eu-west-1.compute.amazonaws.com')


BUCKET_TYPE="STRONG"
BUCKET="ITEMS"
INITIAL_VALUE="10000"
N_VAL="3"
HTTP_PORT="8098"
RIAK_PB_PORT="8087"


#declare -a CLIENTS=('ec2-54-203-38-92.us-west-2.compute.amazonaws.com')
#declare -a SERVERS=('id0:ec2-54-216-124-119.eu-west-1.compute.amazonaws.com')
#curl -X PUT -H "Content-Type: application/json" -d '{"props":{"last_write_wins":true, "n_val":1}}' http://localhost:8098/buckets/ITEMS/props
#bin/riak-admin bucket-type create STRONG '{"props": {"consistent":true, "n_val":1}}'
#bin/riak-admin bucket-type activate STRONG

declare -a REGIONS=(1 2 3 4 5)
declare -a CLIENTS_REGION=(1 10 100)

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

#<RiakLeader> <OtherServers>
create_cluster(){
	cmd="dev/dev1/bin/riak stop ; sleep 5 ; dev/dev1/bin/riak start"
	ssh $USERNAME"@"$1 $cmd
	cmd="dev/dev2/bin/riak stop ; sleep 5 ;  dev/dev2/bin/riak start ; sleep 5 ; dev/dev2/bin/riak-admin cluster join dev1@127.0.0.1"
	ssh $USERNAME"@"$1 $cmd
	cmd="dev/dev3/bin/riak stop ; sleep 5 ; dev/dev3/bin/riak start ; sleep 5 ; dev/dev3/bin/riak-admin cluster join dev1@127.0.0.1"
	ssh $USERNAME"@"$1 $cmd
	cmd="dev/dev4/bin/riak stop ; sleep 5 ; dev/dev4/bin/riak start ; sleep 5 ; dev/dev4/bin/riak-admin cluster join dev1@127.0.0.1"
	ssh $USERNAME"@"$1 $cmd
	cmd="dev/dev5/bin/riak stop ; sleep 5 ; dev/dev5/bin/riak start ; sleep 5 ; dev/dev5/bin/riak-admin cluster join dev1@127.0.0.1"
	ssh $USERNAME"@"$1 $cmd
	cmd="dev/dev1/bin/riak-admin cluster plan"
	ssh $USERNAME"@"$1 $cmd
	cmd="dev/dev1/bin/riak-admin cluster commit"
	ssh -f $USERNAME"@"$1 $cmd
}


#<Clients>
wait_finish() {
	hosts=($1)
	dontStop=true
#	safelimit=$(($1*2))
	dontStop=true
	while $dontStop; do
		dontStop=false
		counter=0
		for h in ${hosts[@]}; do
			echo "Verifying if $h has finished"
			Res="$(ssh $USERNAME@$h pgrep 'beam' | wc -l)"
			#Res="$(ssh $USERNAME@$host ps -C beam --no-headers | wc -l)"
			echo $Res "beam processes are running"
			
																					##ATTENTION!
			if [ $Res != "0" ]; then
				dontStop=true
			fi

#			counter=`expr $counter + 1`
#			if [ $counter -eq $safelimit ]; then
#				break
#			fi
		done
	done
	echo "All clients have stopped"
}



#Process options
while getopts "v:c:rdt:" optname
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
		copy_from_remote $CLIENTS $USERNAME $OUTPUT_DIR $USER_ROOT
		exit
        ;;
	  "d")
		create_cluster "localhost"
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
  
echo "Bucket: "$BUCKET_TYPE" "$BUCKET
echo "Initial value: "$INITIAL_VALUE


for i in "${REGIONS[@]}"
do
   :
   for j in "${CLIENTS_REGION[@]}"
   do
      :
	  filename="experiment_R"$i"_C"$j
	  other=${SERVERS[@]:1:$(($i-1))}
	  cmd=$SCRIPTS_ROOT"reset-script ${SERVERS[0]} $RIAK_PB_PORT $INITIAL_VALUE $USER_ROOT $BUCKET $BUCKET_TYPE"
	  echo $cmd
	  ssh $USERNAME"@"${CLIENTS[0]} $cmd
	  clients=(${CLIENTS[@]:0:$i})
	  servers=(${SERVERS[@]:0:$i})
	  for k in $(seq 0 $((${#clients[@]}-1)))
	  	do
			:
			RESULTS_REGION="$OUTPUT_DIR""${REGION_NAME[k]}/"
			cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-script ${SERVERS[k]} $RIAK_PB_PORT $j $USER_ROOT $BUCKET $BUCKET_TYPE $RESULTS_REGION > $RESULTS_REGION""$filename"
			echo $cmd
			ssh -f $USERNAME@${clients[k]} $cmd
		done
		sleep 2
		wait_finish "${clients[@]}"
		sleep 5
	done
done

sleep 10

#shift $(( ${OPTIND} - 1 )); echo "${*}"
echo "Finish"