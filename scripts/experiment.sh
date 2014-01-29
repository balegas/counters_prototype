#!/bin/bash

#-c <ConsistencyLevel> -t <numClientsByRegion> -v <initialValue> <primaryAddress>

#ConsistencyLevel - Selects the appropriate bucket (Strong/Eventual)
#Address is written in the form "ID:ADDRESS"


#ssh ec2-user@ec2-184-73-119-138.compute-1.amazonaws.com "riak/bin/riak stop ; ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start"
#ssh ec2-user@ec2-54-211-89-174.compute-1.amazonaws.com "riak/bin/riak stop ; ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start ; riak/bin/riak-admin cluster join riak@ec2-184-73-119-138.compute-1.amazonaws.com"
#ssh ec2-user@ec2-54-197-42-1.compute-1.amazonaws.com "riak/bin/riak stop ; ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start ; riak/bin/riak-admin cluster join riak@ec2-184-73-119-138.compute-1.amazonaws.com"
#ssh ec2-user@ec2-184-73-119-138.compute-1.amazonaws.com "riak/bin/riak-admin cluster plan && riak/bin/riak-admin cluster commit"

#ssh ec2-user@ec2-54-211-89-174.compute-1.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb stop ; rm -fr crdtdb/dev/dev1/log/* ; rm -fr crdtdb/dev/dev1/data/* &&  crdtdb/dev/dev1/bin/crdtdb start"
#ssh ec2-user@ec2-54-197-42-1.compute-1.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb stop ; rm -fr crdtdb/dev/dev1/log/* ; rm -fr crdtdb/dev/dev1/data/* && crdtdb/dev/dev1/bin/crdtdb start"
#ssh ec2-user@ec2-184-73-119-138.compute-1.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb stop ; rm -fr crdtdb/dev/dev1/log/* ; rm -fr crdtdb/dev/dev1/data/* && crdtdb/dev/dev1/bin/crdtdb start"

#ssh ec2-user@ec2-54-211-89-174.compute-1.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb-admin cluster join crdtdb1@ec2-184-73-119-138.compute-1.amazonaws.com"
#ssh ec2-user@ec2-54-197-42-1.compute-1.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb-admin cluster join crdtdb1@ec2-184-73-119-138.compute-1.amazonaws.com" 
#ssh ec2-user@ec2-184-73-119-138.compute-1.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb-admin cluster plan && crdtdb/dev/dev1/bin/crdtdb-admin cluster commit ; curl -X PUT -H "Content-Type: application/json" -d '{"props":{"last_write_wins":true, "n_val":3}}' http://localhost:8098/buckets/ITEMS/props"

#==========

#ssh ec2-user@ec2-54-203-150-222.us-west-2.compute.amazonaws.com "riak/bin/riak stop ; ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start"
#ssh ec2-user@ec2-54-203-165-150.us-west-2.compute.amazonaws.com "riak/bin/riak stop ; ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start && riak/bin/riak-admin cluster join riak@ec2-54-203-150-222.us-west-2.compute.amazonaws.com"
#ssh ec2-user@ec2-54-203-183-196.us-west-2.compute.amazonaws.com "riak/bin/riak stop ; ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start ; riak/bin/riak-admin cluster join riak@ec2-54-203-150-222.us-west-2.compute.amazonaws.com"
#ssh ec2-user@ec2-54-203-150-222.us-west-2.compute.amazonaws.com "riak/bin/riak-admin cluster plan && riak/bin/riak-admin cluster commit"

#ssh ec2-user@ec2-54-203-150-222.us-west-2.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb stop ; rm -fr crdtdb/dev/dev1/log/* ; rm -fr crdtdb/dev/dev1/data/* &&  crdtdb/dev/dev1/bin/crdtdb start"
#ssh ec2-user@ec2-54-203-165-150.us-west-2.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb stop ; rm -fr crdtdb/dev/dev1/log/* ; rm -fr crdtdb/dev/dev1/data/* &&  crdtdb/dev/dev1/bin/crdtdb start"
#ssh ec2-user@ec2-54-203-183-196.us-west-2.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb stop ; rm -fr crdtdb/dev/dev1/log/* ; rm -fr crdtdb/dev/dev1/data/* &&  crdtdb/dev/dev1/bin/crdtdb start"

#ssh ec2-user@ec2-54-203-165-150.us-west-2.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb-admin cluster join crdtdb1@ec2-54-203-150-222.us-west-2.compute.amazonaws.com"
#ssh ec2-user@ec2-54-203-183-196.us-west-2.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb-admin cluster join crdtdb1@ec2-54-203-150-222.us-west-2.compute.amazonaws.com" 
#ssh ec2-user@ec2-54-203-150-222.us-west-2.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb-admin cluster plan && crdtdb/dev/dev1/bin/crdtdb-admin cluster commit ; curl -X PUT -H "Content-Type: application/json" -d '{"props":{"last_write_wins":true, "n_val":3}}' http://localhost:8098/buckets/ITEMS/props"

#==========

#ssh ec2-user@ec2-176-34-71-1.eu-west-1.compute.amazonaws.com "riak/bin/riak stop ; ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start"
#ssh ec2-user@ec2-54-220-137-5.eu-west-1.compute.amazonaws.com "riak/bin/riak stop ; ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start && riak/bin/riak-admin cluster join riak@ec2-176-34-71-1.eu-west-1.compute.amazonaws.com"
#ssh ec2-user@ec2-54-220-23-254.eu-west-1.compute.amazonaws.com "riak/bin/riak stop ; ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start ; riak/bin/riak-admin cluster join riak@ec2-176-34-71-1.eu-west-1.compute.amazonaws.com"
#ssh ec2-user@ec2-176-34-71-1.eu-west-1.compute.amazonaws.com "riak/bin/riak-admin cluster plan && riak/bin/riak-admin cluster commit"

#ssh ec2-user@ec2-176-34-71-1.eu-west-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb stop ; rm -fr crdtdb/dev/dev1/log/* ; rm -fr crdtdb/dev/dev1/data/* &&  crdtdb/dev/dev1/bin/crdtdb start"
#ssh ec2-user@ec2-54-220-137-5.eu-west-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb stop ; rm -fr crdtdb/dev/dev1/log/* ; rm -fr crdtdb/dev/dev1/data/* &&  crdtdb/dev/dev1/bin/crdtdb start"
#ssh ec2-user@ec2-54-220-23-254.eu-west-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb stop ; rm -fr crdtdb/dev/dev1/log/* ; rm -fr crdtdb/dev/dev1/data/* &&  crdtdb/dev/dev1/bin/crdtdb start"

#ssh ec2-user@ec2-54-220-137-5.eu-west-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb-admin cluster join crdtdb1@ec2-176-34-71-1.eu-west-1.compute.amazonaws.com"
#ssh ec2-user@ec2-54-220-23-254.eu-west-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb-admin cluster join crdtdb1@ec2-176-34-71-1.eu-west-1.compute.amazonaws.com" 
#ssh ec2-user@ec2-176-34-71-1.eu-west-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb-admin cluster plan && crdtdb/dev/dev1/bin/crdtdb-admin cluster commit ; curl -X PUT -H "Content-Type: application/json" -d '{"props":{"last_write_wins":true, "n_val":3}}' http://localhost:8098/buckets/ITEMS/props"

#==========

#ssh ec2-user@ec2-54-251-69-218.ap-southeast-1.compute.amazonaws.com "riak/bin/riak stop ; ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start"
#ssh ec2-user@ec2-54-255-6-100.ap-southeast-1.compute.amazonaws.com "riak/bin/riak stop ; ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start && riak/bin/riak-admin cluster join riak@ec2-54-251-69-218.ap-southeast-1.compute.amazonaws.com"
#ssh ec2-user@ec2-46-137-237-84.ap-southeast-1.compute.amazonaws.com "riak/bin/riak stop ; ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start ; riak/bin/riak-admin cluster join riak@ec2-54-251-69-218.ap-southeast-1.compute.amazonaws.com"
#ssh ec2-user@ec2-54-251-69-218.ap-southeast-1.compute.amazonaws.com "riak/bin/riak-admin cluster plan && riak/bin/riak-admin cluster commit "

#ssh ec2-user@ec2-54-251-69-218.ap-southeast-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb stop ; rm -fr crdtdb/dev/dev1/log/* ; rm -fr crdtdb/dev/dev1/data/* &&  crdtdb/dev/dev1/bin/crdtdb start"
#ssh ec2-user@ec2-54-255-6-100.ap-southeast-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb stop ; rm -fr crdtdb/dev/dev1/log/* ; rm -fr crdtdb/dev/dev1/data/* &&  crdtdb/dev/dev1/bin/crdtdb start"
#ssh ec2-user@ec2-46-137-237-84.ap-southeast-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb stop ; rm -fr crdtdb/dev/dev1/log/* ; rm -fr crdtdb/dev/dev1/data/* &&  crdtdb/dev/dev1/bin/crdtdb start"

#ssh ec2-user@ec2-54-255-6-100.ap-southeast-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb-admin cluster join crdtdb1@ec2-54-251-69-218.ap-southeast-1.compute.amazonaws.com"
#ssh ec2-user@ec2-46-137-237-84.ap-southeast-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb-admin cluster join crdtdb1@ec2-54-251-69-218.ap-southeast-1.compute.amazonaws.com" 
#ssh ec2-user@ec2-54-251-69-218.ap-southeast-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb-admin cluster plan && crdtdb/dev/dev1/bin/crdtdb-admin cluster commit ; curl -X PUT -H "Content-Type: application/json" -d '{"props":{"last_write_wins":true, "n_val":3}}' http://localhost:8098/buckets/ITEMS/props"

#==========

#ssh ec2-user@ec2-54-232-38-127.sa-east-1.compute.amazonaws.com "riak/bin/riak stop ; ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start"
#ssh ec2-user@ec2-54-232-143-57.sa-east-1.compute.amazonaws.com "riak/bin/riak stop ; ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start && riak/bin/riak-admin cluster join riak@ec2-54-232-38-127.sa-east-1.compute.amazonaws.com"
#ssh ec2-user@ec2-54-232-33-191.sa-east-1.compute.amazonaws.com "riak/bin/riak stop ; ulimit -n 4096 && rm -fr riak/data/* && riak/bin/riak start ; riak/bin/riak-admin cluster join riak@ec2-54-232-38-127.sa-east-1.compute.amazonaws.com"
#ssh ec2-user@ec2-54-232-38-127.sa-east-1.compute.amazonaws.com "riak/bin/riak-admin cluster plan && riak/bin/riak-admin cluster commit"

#ssh ec2-user@ec2-54-232-38-127.sa-east-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb stop ; rm -fr crdtdb/dev/dev1/log/* ; rm -fr crdtdb/dev/dev1/data/* &&  crdtdb/dev/dev1/bin/crdtdb start"
#ssh ec2-user@ec2-54-232-143-57.sa-east-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb stop ; rm -fr crdtdb/dev/dev1/log/* ; rm -fr crdtdb/dev/dev1/data/* &&  crdtdb/dev/dev1/bin/crdtdb start"
#ssh ec2-user@ec2-54-232-33-191.sa-east-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb stop ; rm -fr crdtdb/dev/dev1/log/* ; rm -fr crdtdb/dev/dev1/data/* &&  crdtdb/dev/dev1/bin/crdtdb start"

#ssh ec2-user@ec2-54-232-143-57.sa-east-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb-admin cluster join crdtdb1@ec2-54-232-38-127.sa-east-1.compute.amazonaws.com"
#ssh ec2-user@ec2-54-232-33-191.sa-east-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb-admin cluster join crdtdb1@ec2-54-232-38-127.sa-east-1.compute.amazonaws.com" 
#ssh ec2-user@ec2-54-232-38-127.sa-east-1.compute.amazonaws.com "crdtdb/dev/dev1/bin/crdtdb-admin cluster plan && crdtdb/dev/dev1/bin/crdtdb-admin cluster commit ; curl -X PUT -H "Content-Type: application/json" -d '{"props":{"last_write_wins":true, "n_val":3}}' http://localhost:8098/buckets/ITEMS/props"

#crdtdb/dev/dev1/bin/crdtdb-admin member-status
#riak/bin/riak-admin member-status
#curl -X PUT -H "Content-Type: application/json" -d '{"props":{"last_write_wins":true, "n_val":3}}' http://localhost:8098/buckets/ITEMS/props
#curl -X PUT -H "Content-Type: application/json" -d '{"props":{"allow_mult":true, "n_val":3}}' http://localhost:8098/buckets/ITEMSW/props


# cd crdtdb && git pull && make devrel && cd .. && vi vm.args && vi riak.conf && cp vm.args crdtdb/dev/dev1/etc/ && cp riak.conf riak/etc/ && cd crdtdb && git pull

# killall beam.smp ; ulimit -n 4096 && riak/bin/riak start && crdtdb/dev/dev1/bin/crdtdb start
#cd crdtdb/ && git reset --hard HEAD && git pull && make && rm -fr results/*

USERNAME="ec2-user"
USER_ROOT="/home/ec2-user/crdtdb/"
RIAK_ROOT="/home/ec2-user/riak/"

#USERNAME="balegas"
#USER_ROOT="/Users/balegas/workspace/erlang/crdtdb/"
#RIAK_ROOT="/Users/balegas/workspace/riak/"

SCRIPTS_ROOT=$USER_ROOT"scripts/"
OUTPUT_DIR=$USER_ROOT"results/"



#RESET CRDTDB
#crdtdb/dev/dev1/bin/crdtdb stop ; rm -fr crdtdb/dev/dev1/log/* ; rm -fr crdtdb/dev/dev1/data/* ;  crdtdb/dev/dev1/bin/crdtdb start
#RESET RIAK
#ulimit -n 4096 ; riak/bin/riak stop ; rm -fr riak/data/* && riak/bin/riak start


#cd crdtdb/ ; git reset --hard HEAD && git pull && make ; rm -fr results/*


#Command for strong consistency 
#declare -a CLIENTS=('ec2-54-226-3-200.compute-1.amazonaws.com' 'ec2-54-203-170-24.us-west-2.compute.amazonaws.com' 'ec2-122-248-211-193.ap-southeast-1.compute.amazonaws.com' 'ec2-54-195-66-94.eu-west-1.compute.amazonaws.com' 'ec2-177-71-160-146.sa-east-1.compute.amazonaws.com')
#declare -a SERVERS=('id0:ec2-54-195-47-229.eu-west-1.compute.amazonaws.com' 'id0:ec2-54-220-245-165.eu-west-1.compute.amazonaws.com' 'id0:ec2-54-228-85-40.eu-west-1.compute.amazonaws.com' 'id0:ec2-54-195-47-229.eu-west-1.compute.amazonaws.com' 'id0:ec2-54-220-245-165.eu-west-1.compute.amazonaws.com')

#cp ../vm.args dev/dev1/etc/


#curl -X PUT -H "Content-Type: application/json" -d '{"props":{"allow_mult":true, "n_val":3}}' http://ec2-54-220-178-10.eu-west-1.compute.amazonaws.com:8098/buckets/ITEMS/props
#curl -X PUT -H "Content-Type: application/json" -d '{"props":{"allow_mult":true, "n_val":3}}' http://ec2-54-204-132-46.compute-1.amazonaws.com:8098/buckets/ITEMS/props
#curl -X PUT -H "Content-Type: application/json" -d '{"props":{"allow_mult":true, "n_val":3}}' http://ec2-54-203-5-4.us-west-2.compute.amazonaws.com:8098/buckets/ITEMS/props
#curl -X PUT -H "Content-Type: application/json" -d '{"props":{"allow_mult":true, "n_val":3}}' http://ec2-46-137-232-112.ap-southeast-1.compute.amazonaws.com:8098/buckets/ITEMS/props
#curl -X PUT -H "Content-Type: application/json" -d '{"props":{"allow_mult":true, "n_val":3}}' http://ec2-54-232-5-76.sa-east-1.compute.amazonaws.com:8098/buckets/ITEMS/props


declare -a REGION_NAME=('eu' 'us-east' 'us-west' 'ap' 'sa')

declare -a CLIENTS=("ec2-54-220-67-136.eu-west-1.compute.amazonaws.com"
					"ec2-54-224-6-180.compute-1.amazonaws.com"
					)

declare -a SERVERS=("eu:ec2-54-220-178-10.eu-west-1.compute.amazonaws.com"
					"us-east:ec2-54-204-132-46.compute-1.amazonaws.com"
					"us-west:ec2-54-203-5-4.us-west-2.compute.amazonaws.com"
					"ap:ec2-46-137-232-112.ap-southeast-1.compute.amazonaws.com"
					"sa:ec2-54-232-5-76.sa-east-1.compute.amazonaws.com"
					)

declare -a ALL_SERVERS=(
					"ec2-54-220-178-10.eu-west-1.compute.amazonaws.com"
					"ec2-54-204-132-46.compute-1.amazonaws.com"
					"ec2-54-203-5-4.us-west-2.compute.amazonaws.com"
					"ec2-46-137-232-112.ap-southeast-1.compute.amazonaws.com"
					"ec2-54-232-5-76.sa-east-1.compute.amazonaws.com"
					) 

declare -a SERVERS2=(
					"ec2-54-220-178-10.eu-west-1.compute.amazonaws.com"
					"ec2-54-204-132-46.compute-1.amazonaws.com"
					"ec2-54-203-5-4.us-west-2.compute.amazonaws.com"
					"ec2-46-137-232-112.ap-southeast-1.compute.amazonaws.com"
					"ec2-54-232-5-76.sa-east-1.compute.amazonaws.com"
					)



BUCKET_TYPE="default"
BUCKET="ITEMS"
INITIAL_VALUE="20000"
N_KEYS="1"
N_VAL="3"
HTTP_PORT="8098"
RIAK_PB_PORT="8087"


#declare -a CLIENTS=('ec2-54-203-38-92.us-west-2.compute.amazonaws.com')
#declare -a SERVERS=('id0:ec2-54-216-124-119.eu-west-1.compute.amazonaws.com')
#curl -X PUT -H "Content-Type: application/json" -d '{"props":{"last_write_wins":true, "n_val":3}}' http://localhost:8098/buckets/ITEMS/props
#bin/riak-admin bucket-type create STRONG '{"props": {"consistent":true, "n_val":1}}'
#bin/riak-admin bucket-type activate STRONG

declare -a REGIONS=(5)
declare -a CLIENTS_REGION=(50 100 200 400)

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
	#cmd="killall beam.smp ; rm -fr crdtdb/dev/dev1/log/* ; ulimit -n 4096 && riak/bin/riak start && crdtdb/dev/dev1/bin/crdtdb start"
	cmd="killall beam.smp"
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
	  #	 cmd=$USER_ROOT"dev/dev1/bin/crdtdb stop ; rm -fr "$USER_ROOT"dev/dev1/log/* && "$USER_ROOT"dev/dev1/bin/crdtdb start"
	  #	 echo $cmd
 	  #	 ssh $USERNAME@$h $cmd
	  #done
	  
	  #echo "SET ADDRESSES"
	  #ri=0;
	  #for h in ${servers2[@]}; do
	  #	  cmd=$SCRIPTS_ROOT"init-script $h ${REGION_NAME[$((ri))]} $USER_ROOT `echo ${servers[@]}`"
	  #	  ri=`expr $ri + 1`
      	 #echo "INIT "$h "CMD" $cmd
 	  	 #ssh $USERNAME@$h $cmd
	  #done

	  
	  #Command for Riak-Core
	  #ri=0;
	  #for h in ${servers2[@]}; do
	  #	 cmd=$SCRIPTS_ROOT"reset-script-rc $h ${REGION_NAME[$((ri))]} $N_KEYS $INITIAL_VALUE $USER_ROOT `echo ${servers[@]}`"
	  #	 ri=`expr $ri + 1`
     	 #echo "INIT "$h "CMD" $cmd
 	  	 #ssh $USERNAME@$h $cmd
	  #done
	  
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
			#RESULTS_REGION="$OUTPUT_DIR""${REGION_NAME[k]}'_'$j'_'clients'/'$i'_'regions'_'S300'_'1000k'_'ZIPF'_'RUN3'/'"
			#RESULTS_REGION="$OUTPUT_DIR""${REGION_NAME[k]}'_'$j'_'clients'_'1'/'$i'_'regions'/'"
			#echo "Starting "${clients[k]}" "$RESULTS_REGION
			#cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-time-script-rc ${servers[k]} $j $N_KEYS '180' 'uniform_generator' '0.8' $USER_ROOT $RESULTS_REGION > $RESULTS_REGION""$filename"
			#ssh -f $USERNAME@${clients[k]} $cmd
			
			#Command for strong consistency with key linearizability and Riak-Core
			#cmd="mkdir -p $RESULTS_REGION && "$SCRIPTS_ROOT"riak-execution-script-rc ${SERVERS[k]} $j $USER_ROOT $RESULTS_REGION > $RESULTS_REGION""$filename"
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