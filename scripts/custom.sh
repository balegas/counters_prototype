SCRIPT_PATH="`dirname \"$0\"`"
source "$SCRIPT_PATH/config"

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
	cmd="sudo killall beam.smp"
	ssh_command "$1" "$cmd"
	echo "All clients have stopped"
}

while getopts "a:c:kimbdn" optname
  do
    case "$optname" in
    	"n")
			servers=(${ALL_SERVERS[@]})
			for k in $(seq 0 $((${#servers[@]}-1)))
			do
			:
				OIFS=$IFS
				IFS=':'	
				tokens=(${servers[$k]})
				server=${tokens[0]}
				$(scp -r /home/delphinus/SyncFree/balegas_github/counters_prototype/rel/crdtdb/etc/vm.args $USERNAME@$server:/home/ubuntu/counters_prototype/rel/crdtdb/etc/vm.args.bak)
				
				IFS=$OIFS
			done
			exit
;;
      "a")
				cd "$OPTARG"
				for j in "${CLIENTS_REGION[@]}"
				do
			 	 	total_clients=`expr $j \* 3`
			  		filename="experiment_R3_C"$total_clients"_K"$N_KEYS"_V"$INITIAL_VALUE
		  			clients=(${CONNECTIONS_RC[@]})
					for k in $(seq 0 $((${#clients[@]}-1)))
					do
					:
						OIFS=$IFS
						IFS=':'
						tokens=(${clients[$k]})
						client=${tokens[0]}
						local_filename=$filename"_"$k
						RESULTS_REGION=c$k/"results-bcsrv/"${REGION_NAME[k]}"_"$total_clients"_clients"
						rm -r $RESULTS_REGION

						IFS=$OIFS	
					done
			  done

			exit
		;;
      "b")
			pssh="parallel-ssh -v"
			hosts=""
#			for h in ${ALL_SERVERS[@]}; do
#				hosts=$hosts" -H "$USERNAME"@"$h" "
#			done	
#			cmd=$pssh" "$hosts" -t 0 "
#			cmd=$cmd"sudo dpkg --purge riak ; sudo dpkg -i riak_2.0.2-1_amd64.deb"
#			$cmd
			hosts=""
			for h in ${CLIENTS[@]}; do
				hosts=$hosts" -H "$USERNAME"@"$h" "
			done
			cmd=$pssh" "$hosts" -t 0 "
			cmd=$cmd"sudo riak stop"
			$cmd
  		  	exit
		;;
		"d")
			for i in "${THINK_TIMES[@]}"
			do
				THINK_TIME=$i
				for j in "${CLIENTS_REGION[@]}"
				do
			 	 	total_clients=`expr $j \* 3`
		  			clients=(${CONNECTIONS_RC[@]})
					for k in $(seq 0 $((${#clients[@]}-1)))
					do
					:
						OIFS=$IFS
						IFS=':'
						tokens=(${clients[k]})
						client=${tokens[0]}
						cmd="FOLDER=\$(find $OUTPUT_DIR -type d -name \""${REGION_NAME[k]}"_"$total_clients"_clients_"$THINK_TIME"_thinktime\")"
						cmd=$cmd" && if [ \$FOLDER ]; then FOLDER=\$(dirname \"\$FOLDER\") ; echo \$FOLDER; rm -r \$FOLDER; fi"
						echo "CLIENT "$client":"  $cmd
						ssh -f $USERNAME@$client $cmd

						IFS=$OIFS
					done
			  done
			done
		;;
      "c")
			cd $OPTARG
			clients=(${CLIENTS[@]})
			for k in $(seq 0 $((${#clients[@]}-1)))
			do
			:
				OIFS=$IFS
				IFS=':'	
				tokens=(${clients[$k]})
				client=${tokens[0]}
				mkdir c$k
				$(scp -r $USERNAME@$client:$OUTPUT_DIR c$k/)
				
				IFS=$OIFS
			done
			exit
		;;
      "i")
			echo "Copy source to remote nodes"
			hosts=""
			for h in ${ALL_SERVERS[@]}; do
				hosts=$hosts" -H "$USERNAME"@"$h" "
			done
			prsync="parallel-rsync "$hosts" -r "
			cmd=$prsync" "$LOCAL_ROOT"src "$USER_ROOT
			$cmd
			cmd=$prsync" "$LOCAL_ROOT"scripts "$USER_ROOT
			$cmd
			
			hosts=""
			for h in ${CLIENTS[@]}; do
				hosts=$hosts" -H "$USERNAME"@"$h" "
			done
			prsync="parallel-rsync "$hosts" -r "
			cmd=$prsync" "$LOCAL_ROOT"src "$USER_ROOT
			$cmd
			cmd=$prsync" "$LOCAL_ROOT"scripts "$USER_ROOT
			$cmd
			
			exit
        ;;
		"m")
			pssh="parallel-ssh"
			hosts=""
			for h in ${ALL_SERVERS[@]}; do
				hosts=$hosts" -H "$USERNAME"@"$h" "
			done	
			cmd=$pssh" "$hosts
			cmd=$cmd" cd $USER_ROOT ; sudo make rel"
			$cmd
			hosts=""
			for h in ${CLIENTS[@]}; do
				hosts=$hosts" -H
				 "$USERNAME"@"$h" "
			done	
			cmd=$pssh" "$hosts
			cmd=$cmd" cd $USER_ROOT ; sudo make"
			$cmd
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

 shift $((OPTIND-1))