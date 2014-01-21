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



#ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start

#rm -fr dev/dev1/log/*

#ulimit -n 4096 && riak/bin/riak stop && rm -fr riak/data/* && riak/bin/riak start && curl -X PUT -H "Content-Type: application/json" -d '{"props":{"last_write_wins":true, "n_val":3}}' http://localhost:8098/buckets/ITEMS/props

#ulimit -n 4096 && riak/bin/riak stop && rm -fr riak/data/* && riak/bin/riak start && riak/bin/riak-admin cluster join riak@ec2-54-203-182-45.us-west-2.compute.amazonaws.com

#crdtdb/dev/dev1/bin/crdtdb stop && rm -fr crdtdb/dev/dev1/log/* && crdtdb/dev/dev1/bin/crdtdb start

#tail -f crdtdb/dev/dev1/log/erlang.log.1
#rm -fr riak/data/* && riak/bin/riak start && rm -fr crdtdb/dev/dev1/log/* && crdtdb/dev/dev1/bin/crdtdb start

#vi riak/etc/riak.conf 
#ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start && riak/bin/riak-admin  cluster join riak@ec2-54-207-158-184.sa-east-1.compute.amazonaws.com

#sudo yum install -y gcc gcc-c++ glibc-devel make git pam-devel ; wget http://www.erlang.org/download/otp_src_R16B02.tar.gz ; tar -xvf otp_src_R16B02.tar.gz;  cd otp_src_R16B02; ./configure ; make; sudo make install
#sudo yum install -y gcc gcc-c++ glibc-devel make git pam-devel; git clone https://github.com/basho/riak; cd riak; make rel; cd ..; mv riak riak-rep; mv riak-rep/rel/riak ..




#/home/ec2-user/crdtdb/scripts/reset-script-rc ec2-54-255-3-147.ap-southeast-1.compute.amazonaws.com 1 50000 /home/ec2-user/crdtdb/ crdtdb1@ec2-54-224-122-132.compute-1.amazonaws.com:crdtdb1@ec2-54-224-122-132.compute-1.amazonaws.com crdtdb1@ec2-54-203-182-45.us-west-2.compute.amazonaws.com:crdtdb1@ec2-54-203-182-45.us-west-2.compute.amazonaws.com crdtdb1@ec2-54-255-3-147.ap-southeast-1.compute.amazonaws.com:crdtdb1@ec2-54-255-3-147.ap-southeast-1.compute.amazonaws.com crdtdb1@ec2-54-195-16-157.eu-west-1.compute.amazonaws.com:crdtdb1@ec2-54-195-16-157.eu-west-1.compute.amazonaws.com




#Command for strong consistency 
#declare -a CLIENTS=('ec2-54-226-3-200.compute-1.amazonaws.com' 'ec2-54-203-170-24.us-west-2.compute.amazonaws.com' 'ec2-122-248-211-193.ap-southeast-1.compute.amazonaws.com' 'ec2-54-195-66-94.eu-west-1.compute.amazonaws.com' 'ec2-177-71-160-146.sa-east-1.compute.amazonaws.com')
#declare -a SERVERS=('id0:ec2-54-195-47-229.eu-west-1.compute.amazonaws.com' 'id0:ec2-54-220-245-165.eu-west-1.compute.amazonaws.com' 'id0:ec2-54-228-85-40.eu-west-1.compute.amazonaws.com' 'id0:ec2-54-195-47-229.eu-west-1.compute.amazonaws.com' 'id0:ec2-54-220-245-165.eu-west-1.compute.amazonaws.com')

declare -a REGION_NAME=('us-east' 'us-west' 'ap' 'eu' 'sa')
declare -a CLIENTS=("ec2-50-19-51-167.compute-1.amazonaws.com")
declare -a SERVERS=('us-east:crdtdb1@ec2-54-211-19-35.compute-1.amazonaws.com')
declare -a SERVERS2=('ec2-54-211-19-35.compute-1.amazonaws.com')



BUCKET_TYPE="STRONG"
BUCKET="ITEMS"
INITIAL_VALUE="100"
N_KEYS="10"
N_VAL="3"
HTTP_PORT="8098"
RIAK_PB_PORT="8087"


#declare -a CLIENTS=('ec2-54-203-38-92.us-west-2.compute.amazonaws.com')
#declare -a SERVERS=('id0:ec2-54-216-124-119.eu-west-1.compute.amazonaws.com')
#curl -X PUT -H "Content-Type: application/json" -d '{"props":{"last_write_wins":true, "n_val":3}}' http://localhost:8098/buckets/ITEMS/props
#bin/riak-admin bucket-type create STRONG '{"props": {"consistent":true, "n_val":1}}'
#bin/riak-admin bucket-type activate STRONG

declare -a REGIONS=(1)
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
	  servers=${SERVERS[@]:0:$(($i))}
	  servers2=${SERVERS2[@]:0:$(($i))}
	  
	  #Command for strong consistency
	  #cmd=$SCRIPTS_ROOT"reset-script ${SERVERS[0]} $RIAK_PB_PORT $INITIAL_VALUE $USER_ROOT $BUCKET $BUCKET_TYPE"
	  #ssh $USERNAME@${SERVERS2[0]} $cmd

	  #Command for Riak-Core
	  echo "REBOOT CLUSTER"
	  
	  #for h in ${SERVERS2[@]}; do
	  #	 cmd=$USER_ROOT"dev/dev1/bin/crdtdb stop ; rm -fr "$USER_ROOT"dev/dev1/log/* && "$USER_ROOT"dev/dev1/bin/crdtdb start"
	  #	 echo $cmd
 	  #	 ssh $USERNAME@$h $cmd
	  #done
	  
	  echo "SET ADDRESSES"
	  
	  for h in ${servers2[@]}; do
	  	 cmd=$SCRIPTS_ROOT"init-script $h $USER_ROOT `echo ${servers[@]}`"
      	 #echo "INIT "$h "CMD" $cmd
 	  	 ssh $USERNAME@$h $cmd
	  done

	  echo "RESET DATA"
	  
	  #Command for Riak-Core
	  
	  for h in ${servers2[@]}; do
	 	 cmd=$SCRIPTS_ROOT"reset-script-rc random $h $N_KEYS $INITIAL_VALUE $USER_ROOT `echo ${servers[@]}`"
     	 echo "INIT "$h "CMD" $cmd
 	  	 ssh $USERNAME@$h $cmd
	 done

   	  #Command for strong consistency with key linearizability
	  #cmd=$SCRIPTS_ROOT"init-script ${SERVERS2[0]} $USER_ROOT '`echo ${SERVERS[0]}`'"
  	  #ssh $USERNAME@${SERVERS2[0]} $cmd
	  #cmd=$SCRIPTS_ROOT"reset-script-rc ${SERVERS2[0]} 1 $INITIAL_VALUE $USER_ROOT '`echo ${SERVERS[0]}`'"
	  #echo "INIT "${SERVERS2[0]} "CMD" $cmd
	  #ssh $USERNAME@${SERVERS2[0]} $cmd
	  
	  
	  sleep 5
	  
	  
	  
 	  clients=(${CLIENTS[@]:0:$i})
	  servers=(${SERVERS[@]:0:$i})
	  for k in $(seq 0 $((${#clients[@]}-1)))
	  	do
			:
			RESULTS_REGION="$OUTPUT_DIR""${REGION_NAME[k]}'_'$j'_'clients'/'$i'_'regions'/'"
			echo "Starting "${clients[k]}" "$RESULTS_REGION
			#Command for strong consistency
			cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-time-script-rc ${SERVERS[k]} $j $N_KEYS '60' 'uniform_generator' '0.8' $USER_ROOT $RESULTS_REGION > $RESULTS_REGION""$filename"
			#Command for strong consistency with key linearizability and Riak-Core
			#cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-script-rc ${SERVERS[k]} $j $USER_ROOT $RESULTS_REGION > $RESULTS_REGION""$filename"
			#echo $cmd
			ssh -f $USERNAME@${clients[k]} $cmd
		done
		sleep 5
		wait_finish "`echo ${clients[@]}`"
		sleep 300
	done
done

#shift $(( ${OPTIND} - 1 )); echo "${*}"
echo "Finish"