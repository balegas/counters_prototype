#!/bin/bash


USERNAME="ec2-user"
USER_ROOT="/home/ec2-user/crdtdb/"
RIAK_ROOT="/home/ec2-user/riak/"

#USERNAME="balegas"
#USER_ROOT="/Users/balegas/workspace/erlang/crdtdb/"
#RIAK_ROOT="/Users/balegas/workspace/riak/"

SCRIPTS_ROOT=$USER_ROOT"scripts/"
OUTPUT_DIR=$USER_ROOT"results/"




declare -a REGION_NAME=('no-region' 'no-region' 'no-region' 'no-region' 'no-region')

declare -a CLIENTS=(
					"ec2-54-243-16-161.compute-1.amazonaws.com"
					"ec2-54-184-111-97.us-west-2.compute.amazonaws.com"
					"ec2-46-137-151-80.eu-west-1.compute.amazonaws.com"
					"ec2-175-41-154-66.ap-southeast-1.compute.amazonaws.com"
					"ec2-54-232-153-222.sa-east-1.compute.amazonaws.com"

					)

declare -a SERVERS=(
					"no-region:ec2-54-205-246-100.compute-1.amazonaws.com"
					"no-region:ec2-54-244-147-126.us-west-2.compute.amazonaws.com"
					"no-region:ec2-54-195-0-215.eu-west-1.compute.amazonaws.com"
					"no-region:ec2-54-255-28-12.ap-southeast-1.compute.amazonaws.com"
					"no-region:ec2-54-207-143-83.sa-east-1.compute.amazonaws.com"
					)

declare -a ALL_SERVERS=(
					"ec2-54-205-246-100.compute-1.amazonaws.com"
					"ec2-54-244-147-126.us-west-2.compute.amazonaws.com"
					"ec2-54-195-0-215.eu-west-1.compute.amazonaws.com"
					"ec2-54-255-28-12.ap-southeast-1.compute.amazonaws.com"
					"ec2-54-207-143-83.sa-east-1.compute.amazonaws.com"
					) 

declare -a SERVERS2=(
					"ec2-54-205-246-100.compute-1.amazonaws.com"
					"ec2-54-244-147-126.us-west-2.compute.amazonaws.com"
					"ec2-54-195-0-215.eu-west-1.compute.amazonaws.com"
					"ec2-54-255-28-12.ap-southeast-1.compute.amazonaws.com"
					"ec2-54-207-143-83.sa-east-1.compute.amazonaws.com"
					)



BUCKET_TYPE="default"
BUCKET="ITEMS"
INITIAL_VALUE="200000"
N_KEYS="0"
N_VAL="3"
HTTP_PORT="8098"
RIAK_PB_PORT="8087"


declare -a REGIONS=(5)
declare -a CLIENTS_REGION=(2000)

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


for i in "${REGIONS[@]}"
do
   :
   for j in "${CLIENTS_REGION[@]}"
   do
      :
	  filename="experiment_R"$i"_C"$j
	  servers=${SERVERS[@]:0:$(($i))}
	  servers2=${SERVERS2[@]:0:$(($i))}
	  
	  #Command for strong consistency
	  #cmd=$SCRIPTS_ROOT"reset-script ${SERVERS[0]} $RIAK_PB_PORT $INITIAL_VALUE $USER_ROOT $BUCKET $BUCKET_TYPE"
	  #ssh $USERNAME@${SERVERS2[0]} $cmd

	  #Command for Riak-Core
	  #echo "REBOOT CLUSTER"
	  
	  #for h in ${SERVERS2[@]}; do
	  #	 cmd=$USER_ROOT"dev/dev1/bin/crdtdb stop ; rm -fr "$USER_ROOT"dev/dev1/log/* && rm -fr "$USER_ROOT"dev/dev1/data/* && "$USER_ROOT"dev/dev1/bin/crdtdb start"
	#	 echo $cmd
 	  #	 ssh $USERNAME@$h $cmd
	 # done
	  
	  #echo "SET ADDRESSES"
	  #ri=0;
	  #for h in ${servers2[@]}; do
	#	  cmd=$SCRIPTS_ROOT"init-script $h ${REGION_NAME[$((ri))]} $USER_ROOT `echo ${servers[@]}`"
	#  	  ri=`expr $ri + 1`
	#	  echo "INIT "$h "CMD" $cmd
	#	  ssh $USERNAME@$h $cmd
	# done

	  echo "RESET"
	  #Command for Riak-Core
	  ri=0;
	  for h in ${servers2[@]}; do
	  	 #cmd=$SCRIPTS_ROOT"reset-script-rc $h ${REGION_NAME[$((ri))]} $N_KEYS $INITIAL_VALUE $USER_ROOT `echo ${servers[@]}`"
		 cmd=$SCRIPTS_ROOT"reset-script-counter ${SERVERS[0]} $RIAK_PB_PORT $INITIAL_VALUE $USER_ROOT $BUCKET $BUCKET_TYPE"
	  	 ri=`expr $ri + 1`
     	 echo "INIT "$h "CMD" $cmd
 	  	 ssh $USERNAME@$h $cmd
	  done
	  
	  sleep 10
	  
	  #Command for weak consistency
	  #cmd=$SCRIPTS_ROOT"reset-script-counter ${SERVERS[0]} $RIAK_PB_PORT $INITIAL_VALUE $USER_ROOT $BUCKET $BUCKET_TYPE"
	  #echo "INIT "${SERVERS2[0]} "CMD" $cmd
	  #ssh $USERNAME@${SERVERS2[0]} $cmd
	  #sleep 120
	  
	  #Command for strong consistency with key linearizability
	  #cmd=$SCRIPTS_ROOT"init-script ${SERVERS2[0]} $USER_ROOT '`echo ${SERVERS[0]}`'"
  	  #ssh $USERNAME@${SERVERS2[0]} $cmd
	  
	  #cmd=$SCRIPTS_ROOT"init-script ${SERVERS2[0]} ${REGION_NAME[$((ri))]} $USER_ROOT `echo ${SERVERS[0]}`"
	  #echo "INIT "${SERVERS2[0]} "CMD" $cmd
	  #ssh $USERNAME@${SERVERS2[0]} $cmd
	  
	  
	  #cmd=$SCRIPTS_ROOT"reset-script-rc ${SERVERS2[0]} ${REGION_NAME[$((ri))]} 1 $INITIAL_VALUE $USER_ROOT `echo ${SERVERS[0]}`"
	  #echo "INIT "${SERVERS2[0]} "CMD" $cmd
	  #ssh $USERNAME@${SERVERS2[0]} $cmd
	  
	  
 	  clients=(${CLIENTS[@]:0:$i})
	  clients2=(${CLIENTS2[@]:0:$i})
	  servers=(${SERVERS[@]:0:$i})
  	  servers2=(${SERVERS3[@]:0:$i})
	  for k in $(seq 0 $((${#clients[@]}-1)))
	  	do
			:
			#RESULTS_REGION="$OUTPUT_DIR""${REGION_NAME[k]}'_'$j'_'clients'_'1'/'$i'_'regions'/'"
			#echo "Starting "${clients[k]}" "$RESULTS_REGION
			#cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-time-script-rc ${servers[k]} $j $N_KEYS '180' 'uniform_generator' '0.8' $USER_ROOT $RESULTS_REGION > $RESULTS_REGION""$filename"
			#ssh -f $USERNAME@${clients[k]} $cmd
			
			#Command for strong consistency with key linearizability and Riak-Core
			#RESULTS_REGION="$OUTPUT_DIR""${REGION_NAME[k]}'_'$j'_'clients'/'$i'_'regions'/'"
			#cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-script-rc ${SERVERS[k]} $j $USER_ROOT $RESULTS_REGION > $RESULTS_REGION""$filename"
			#echo $cmd
			#ssh -f $USERNAME@${clients[k]} $cmd

			#Command for weak consistency
			RESULTS_REGION="$OUTPUT_DIR""${REGION_NAME[k]}'_'$j'_'clients'/'$i'_'regions'/'"
			cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-script-counter ${SERVERS[k]} $RIAK_PB_PORT $j $USER_ROOT $BUCKET $BUCKET_TYPE $RESULTS_REGION> $RESULTS_REGION""$filename"
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