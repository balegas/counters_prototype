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


declare -a REGION_NAME=('us-east' 'us-west' 'ap' 'eu' 'sa')


#Command for strong consistency 
#declare -a CLIENTS=('ec2-54-226-3-200.compute-1.amazonaws.com' 'ec2-54-203-170-24.us-west-2.compute.amazonaws.com' 'ec2-122-248-211-193.ap-southeast-1.compute.amazonaws.com' 'ec2-54-195-66-94.eu-west-1.compute.amazonaws.com' 'ec2-177-71-160-146.sa-east-1.compute.amazonaws.com')
#declare -a SERVERS=('id0:ec2-54-195-47-229.eu-west-1.compute.amazonaws.com' 'id0:ec2-54-220-245-165.eu-west-1.compute.amazonaws.com' 'id0:ec2-54-228-85-40.eu-west-1.compute.amazonaws.com' 'id0:ec2-54-195-47-229.eu-west-1.compute.amazonaws.com' 'id0:ec2-54-220-245-165.eu-west-1.compute.amazonaws.com')

declare -a CLIENTS=('ec2-54-224-144-118.compute-1.amazonaws.com' 'ec2-54-184-126-117.us-west-2.compute.amazonaws.com' 'ec2-54-255-7-26.ap-southeast-1.compute.amazonaws.com' 'ec2-54-216-77-45.eu-west-1.compute.amazonaws.com' 'ec2-177-71-160-146.sa-east-1.compute.amazonaws.com')
declare -a SERVERS=('crdtdb1@ec2-54-247-156-208.eu-west-1.compute.amazonaws.com:crdtdb1@ec2-54-247-156-208.eu-west-1.compute.amazonaws.com' 'crdtdb1@ec2-54-247-156-208.eu-west-1.compute.amazonaws.com:crdtdb1@ec2-54-247-156-208.eu-west-1.compute.amazonaws.com' 'crdtdb1@ec2-54-247-156-208.eu-west-1.compute.amazonaws.com:crdtdb1@ec2-54-247-156-208.eu-west-1.compute.amazonaws.com' 'crdtdb1@ec2-54-247-156-208.eu-west-1.compute.amazonaws.com:crdtdb1@ec2-54-247-156-208.eu-west-1.compute.amazonaws.com' 'crdtdb1@ec2-54-247-156-208.eu-west-1.compute.amazonaws.com:crdtdb1@ec2-54-247-156-208.eu-west-1.compute.amazonaws.com')



BUCKET_TYPE="STRONG"
BUCKET="ITEMS"
INITIAL_VALUE="10000"
N_VAL="3"
HTTP_PORT="8098"
RIAK_PB_PORT="8087"


#declare -a CLIENTS=('ec2-54-203-38-92.us-west-2.compute.amazonaws.com')
#declare -a SERVERS=('id0:ec2-54-216-124-119.eu-west-1.compute.amazonaws.com')
#curl -X PUT -H "Content-Type: application/json" -d '{"props":{"last_write_wins":true, "n_val":3}}' http://localhost:8098/buckets/ITEMS/props
#bin/riak-admin bucket-type create STRONG '{"props": {"consistent":true, "n_val":1}}'
#bin/riak-admin bucket-type activate STRONG

declare -a REGIONS=(5 4 3 2 1)
declare -a CLIENTS_REGION=(50)

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
kill_all() {
	cmd="killall beam.smp"
	ssh_command "$1" "$cmd"
	echo "All clients have stopped"
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
		ssh $USERNAME@$h $2
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
while getopts "v:c:krdt:" optname
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
	  #Command for strong consistency
	  #cmd=$SCRIPTS_ROOT"reset-script ${SERVERS[0]} $RIAK_PB_PORT $INITIAL_VALUE $USER_ROOT $BUCKET $BUCKET_TYPE"

  	  #Command for strong consistency with key linearizability
	  echo "SET ADDRESSES"
	  	  
	  cmd=$SCRIPTS_ROOT"reset-script-rc $USER_ROOT ${SERVERS[0]} `echo ${other[@]}`"
	  echo $cmd
	  ssh $USERNAME"@"${CLIENTS[0]} $cmd

	  echo "RESET DATA"

	  cmd=$SCRIPTS_ROOT"reset-script-rc 1 $INITIAL_VALUE $USER_ROOT ${SERVERS[0]}"
	  echo $cmd
	  ssh $USERNAME"@"${CLIENTS[0]} $cmd
	  
	    
	  clients=(${CLIENTS[@]:0:$i})
	  servers=(${SERVERS[@]:0:$i})
	  for k in $(seq 0 $((${#clients[@]}-1)))
	  	do
			:
			RESULTS_REGION="$OUTPUT_DIR""${REGION_NAME[k]}'_'$j'_'clients'/'$i'_'regions'/'"
			#Command for strong consistency
			#cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-script ${SERVERS[k]} $RIAK_PB_PORT $j $USER_ROOT $BUCKET $BUCKET_TYPE $RESULTS_REGION > $RESULTS_REGION""$filename"
			#Command for strong consistency with key linearizability
			cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-script-rc ${SERVERS[k]} $j $USER_ROOT $RESULTS_REGION > $RESULTS_REGION""$filename"
			echo $cmd
			ssh -f $USERNAME@${clients[k]} $cmd
		done
		sleep 2
		wait_finish "`echo ${clients[@]}`"
		sleep 30
	done
done

#shift $(( ${OPTIND} - 1 )); echo "${*}"
echo "Finish"