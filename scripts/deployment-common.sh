#!/bin/bash


#<hosts_file> <user> <localDir> <remoteDir>
p_rsync_dir(){
	#parallel-rsync -v -l $2 -r -h $1 $3 $4
	prsync -v -l $2 -r -h $1 $3 $4
}

#<hosts_file> <user> <source> <destination>
copy_from_remote(){
	for host in $(cat "$1"); do
		#Temporart HACK 
		hostArr=($(echo $host | tr ":" "\n"))
		host=${hostArr[1]}
		echo "get from "$host
		scp -r $2@$host:$3 $4 &
	done

}

#<hosts> <user> <source> <destination>
copy_to_remote_host(){
		scp -r $3 $2@$1:$4
}

#WARNING: AGGRESSIVE KILL COMMAND! CAN KILL OTHER EXECUTING PROCESSES

#<hosts_file> <user>
kill_hosts(){
	killCommand="pkill -9 -u $2"
	for host in $(cat "$1"); do
		ssh -f $2@$host $killCommand &
	done
}

#<hosts_file> <user> <cmd>
_parallel_ssh(){
	#parallel-ssh -l $2 -h $1 $3
	pssh -l $2 -h $1 $3  
}

#<hosts_file> <user> <dir>
init_directory(){
		cmd="[ -d $4 ] && rm -fr $4/* || mkdir -p $4"
		_parallel_ssh $1 $2 $cmd 
}

#<hosts_file>
implode_hosts(){
	OUT=""
	for host in $(cat "$1"); do
		echo $host" "
	done
}


