export DEBIAN_FRONTEND=noninteractive

remote_command='
sudo apt-get install dstat --yes
echo "wireshark-common wireshark-common/install-setuid boolean true" | sudo debconf-set-selections
sudo DEBIAN_FRONTEND=noninteractive apt-get -y install tshark
sudo apt install apparmor-utils --yes
sudo aa-complain /usr/sbin/tcpdump
bash /home/'$USER'/interfaces_up.sh
'

pid_list=""
nodes=0

for ip in `cat /home/$USER/ips.txt`
do
        echo $ip
	scp /home/$USER/NSF-CC-GAF/GAF/interfaces_up.sh $ip:$HOME
        ssh -tt -o "StrictHostKeyChecking no" $ip "$remote_command" & > /dev/null
        pid_list="$pid_list $!"
        nodes=$((nodes+1))
done

pid_list_finished=""
total_nodes=$nodes

while true; 
do
        echo "instaling.."
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

