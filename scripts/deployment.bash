#!/bin/bash

declare -a NODE_NAME=(
						"ec2-54-208-239-199.compute-1.amazonaws.com"
						"ec2-54-208-239-235.compute-1.amazonaws.com"
						"ec2-54-208-102-195.compute-1.amazonaws.com"
					) 
declare -a PRIVATE_IP=(
						"127.0.0.1"
						"127.0.0.1"
						"127.0.0.1"
					) 


#sudo iptables -X
#sudo iptables -t nat -F
#sudo iptables -t nat -X
#sudo iptables -t mangle -F
#sudo iptables -t mangle -X
#sudo iptables -P INPUT ACCEPT
#sudo iptables -P FORWARD ACCEPT
#sudo iptables -P OUTPUT ACCEPT
#sudo ufw disable

for k in $(seq 0 $((${#NODE_NAME[@]}-1)))
	do
		:
		cmd="sudo riak stop ; sudo killall beam.smp"
		ssh -t ubuntu@${NODE_NAME[k]} $cmd
		cmd="sudo rm -fr /var/lib/riak/ring/* ; sudo rm -fr /var/lib/riak/anti_entropy/; sudo rm -fr /var/lib/riak/anti_entropy/; sudo  rm -fr /var/lib/riak/anti_entropy/; sudo rm -fr /var/lib/riak/kv_vnode/; sudo rm -fr /var/lib/riak/riak_repl/; sudo rm -fr crdtdb/data/*"
		ssh -t ubuntu@${NODE_NAME[k]} $cmd
	done

#sudo mv /etc/riak/vm.args.back /etc/riak/vm.args
#sudo mv /etc/riak/app.config.back /etc/riak/app.config

for k in $(seq 0 $((${#NODE_NAME[@]}-1)))
	do
		:
		cmd="sudo mv /etc/riak/vm.args.back /etc/riak/vm.args && sudo sed 's/riak@127.0.0.1/riak@${NODE_NAME[k]}/' -i.back /etc/riak/vm.args" 
		echo $cmd
		ssh -t ubuntu@${NODE_NAME[k]} $cmd
		cmd="sudo mv /etc/riak/app.config.back /etc/riak/app.config && sudo sed -e 's/127.0.0.1/${PRIVATE_IP[k]}/' -i.back /etc/riak/app.config"
		echo $cmd
		ssh -t ubuntu@${NODE_NAME[k]} $cmd
	done

for k in $(seq 0 $((${#NODE_NAME[@]}-1)))
	do
		:
		cmd="ulimit -n 4096; sudo riak start"
		ssh -t ubuntu@${NODE_NAME[k]} $cmd
	done

sleep 5

for k in $(seq 1 $((${#NODE_NAME[@]}-1)))
	do
		:
		cmd="sudo riak-admin cluster join riak@${NODE_NAME[0]}" 
		ssh -t ubuntu@${NODE_NAME[k]} $cmd
		echo $cmd		
done

sleep 5

cmd="sudo riak-admin cluster plan && sudo riak-admin cluster commit"
ssh -t ubuntu@${NODE_NAME[0]} $cmd

#--------------------------

for k in $(seq 0 $((${#NODE_NAME[@]}-1)))
	do
		:
		cmd="~/crdtdb/bin/crdtdb stop ; cp ~/crdtdb/etc/vm.args.back ~/crdtdb/etc/vm.args"
		echo $cmd
		ssh ubuntu@${NODE_NAME[k]} $cmd
		cmd="sed 's/crdtdb@127.0.0.1/crdtdb@${NODE_NAME[k]}/' -i.back ~/crdtdb/etc/vm.args" 
		echo $cmd
		ssh ubuntu@${NODE_NAME[k]} $cmd
	done

for k in $(seq 0 $((${#NODE_NAME[@]}-1)))
	do
		:
		cmd="ulimit -n 4096; ~/crdtdb/bin/crdtdb start"
		echo $cmd
		ssh ubuntu@${NODE_NAME[k]} $cmd
done

sleep 5

for k in $(seq 1 $((${#NODE_NAME[@]}-1)))
	do
		:
		cmd="~/crdtdb/bin/crdtdb-admin cluster join crdtdb@${NODE_NAME[0]}" 
		echo $cmd
		ssh ubuntu@${NODE_NAME[k]} $cmd
done

sleep 5

cmd="~/crdtdb/bin/crdtdb-admin cluster plan && ~/crdtdb/bin/crdtdb-admin cluster commit"
echo $cmd
ssh ubuntu@${NODE_NAME[0]} $cmd


#sudo riak-repl clustername US-EAST
#sudo riak-repl connect ec2-54-183-40-147.us-west-1.compute.amazonaws.com:9080
#sudo riak-repl connect ec2-54-76-22-29.eu-west-1.compute.amazonaws.com:9080
#sudo riak-repl realtime enable US-WEST
#sudo riak-repl realtime enable EU-WEST
#sudo riak-repl realtime start EU-WEST
#sudo riak-repl realtime start US-WEST


#sudo riak-repl clustername US-WEST
#sudo riak-repl connect ec2-23-20-12-80.compute-1.amazonaws.com:9080
#sudo riak-repl connect ec2-54-76-22-29.eu-west-1.compute.amazonaws.com:9080
#sudo riak-repl realtime enable US-EAST
#sudo riak-repl realtime enable EU-WEST
#sudo riak-repl realtime start US-EAST
#sudo riak-repl realtime start EU-WEST

#sudo riak-repl clustername EU-WEST
#sudo riak-repl connect ec2-23-20-12-80.compute-1.amazonaws.com:9080
#sudo riak-repl connect ec2-54-183-40-147.us-west-1.compute.amazonaws.com:9080
#sudo riak-repl realtime enable US-EAST
#sudo riak-repl realtime enable US-WEST
#sudo riak-repl realtime start US-EAST
#sudo riak-repl realtime start US-WEST

#rsync -e 'ssh' -avl   ubuntu@ec2-54-208-241-197.compute-1.amazonaws.com:crdtdb-git/results* .
#rsync -e 'ssh' -avl   ubuntu@ec2-54-183-64-143.us-west-1.compute.amazonaws.com:crdtdb-git/results* .
#rsync -e 'ssh' -avl   ubuntu@ec2-54-72-55-184.eu-west-1.compute.amazonaws.com:crdtdb-git/results* .
#rsync -e 'ssh' -avl   ubuntu@ec2-54-208-241-212.compute-1.amazonaws.com:crdtdb-git/results* .
#rsync -e 'ssh' -avl   ubuntu@ec2-54-183-47-17.us-west-1.compute.amazonaws.com:crdtdb-git/results* .
#rsync -e 'ssh' -avl   ubuntu@ec2-54-76-41-215.eu-west-1.compute.amazonaws.com:crdtdb-git/results* .
#rsync -e 'ssh' -avl   ubuntu@ec2-54-86-236-255.compute-1.amazonaws.com:crdtdb-git/results* .
#rsync -e 'ssh' -avl   ubuntu@ec2-54-183-64-144.us-west-1.compute.amazonaws.com:crdtdb-git/results* .
#rsync -e 'ssh' -avl   ubuntu@ec2-54-76-61-204.eu-west-1.compute.amazonaws.com:crdtdb-git/results* .

