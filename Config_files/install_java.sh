export DEBIAN_FRONTEND=noninteractive

remote_command="
sudo apt-get update --yes
sudo apt-get install openjdk-8-jdk-headless --yes
sudo apt-get install openjdk-8-jre-headless --yes
"

pid_list=""
nodes=0

for ip in `cat /home/$USER/ips.txt`
do
        echo $ip
        ssh -tt -o "StrictHostKeyChecking no" $ip "$remote_command" & > /dev/null
        pid_list="$pid_list $!"
        nodes=$((nodes+1))
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

###################################################################
############## Setting up the Java environment ####################
###################################################################

remote_command_env='
cp ~/.bashrc ~/.bashrc_copy
echo "export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64" >> ~/.bashrc
source ~/.bashrc
'

for ip in `cat /home/$USER/ips.txt`
do
        echo $ip
        ssh -o "StrictHostKeyChecking no" $ip "$remote_command_env" &
done


