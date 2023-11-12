remote_command="
sudo apt-get update --yes
sudo apt install gcc --yes
ver=`uname -r`
sudo apt-get install linux-headers-$ver --yes
sudo apt install ubuntu-drivers-common --yes
sudo ubuntu-drivers autoinstall
"

pid_list=""
nodes=0

# Assumption that master node is 192.168.1.1 and GPU is not attached to this node (VM).

for ip in `cat /home/$USER/gpu_ips.txt`
do
	#not required since gpu_ips.txt contains IPs of GPU containing nodes: Remove it.
        if [ "$ip" = "192.168.1.1" ];
        then
                continue
        else
                ssh -tt -o "StrictHostKeyChecking no" $ip "$remote_command" > /home/$USER/NSF-CC-GAF/GAF/LOG_gpu_driver_install.log 2>&1 &
                pid_list="$pid_list $!"
                echo $pid_list
                nodes=$((nodes+1))
        fi
done

pid_list_finished=""
total_nodes=$nodes

while true;
do
        echo "installing.."
        for pid in $pid_list;
        do
                if ps -p $pid > /dev/null
                then
                        continue
                else
                        if [ "$pid_list_finished" = "" ];
                        then
                                pid_list_finished="$pid_list_finished $pid"
                                nodes=$((nodes-1))
                                echo "Nodes Remaining : " "$total_nodes /" "$nodes"
                        else
                                if [[ $pid_list_finished == *"$pid"* ]];
                                then
                                        nodes=$((nodes-1))
                                        echo "Nodes Remaining : " "$total_nodes /" "$nodes"
                                fi
                        fi
                fi
        done

        if [ $nodes -eq 0 ] ;
        then
                break
        fi

        sleep 7
done

