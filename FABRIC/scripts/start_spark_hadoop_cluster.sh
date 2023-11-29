DATA_DIR='/mydata'

temp='echo " "'

ssh -o "StrictHostKeyChecking no" node1 "$temp"
ssh -o "StrictHostKeyChecking no" 0.0.0.0 "$temp"

node_number=0
for ip in `cat /home/$USER/ips.txt`
do
        ssh -o "StrictHostKeyChecking no" vm$node_number "$temp"
        node_number=$((node_number+1))
done

if [ "$JAVA_HOME" = "" ];
then
        echo "JAVA_HOME not set, run: source ~/.bashrc OR logout and login of the system"
        exit
else
	echo ">>> Starting hadoop and spark <<< "
	echo " "
	echo ">>> It should be over by 29 Mississippi if only CPUs are available<<<"
	echo ">>> It should be over by 89 Mississippi if GPUs are available<<<"
	echo " "
        $DATA_DIR/hadoop/bin/hadoop namenode -format > LOG-start_spark_hadoop_cluster.log 2>&1
        $DATA_DIR/hadoop/sbin/start-dfs.sh >> LOG-start_spark_hadoop_cluster.log 2>&1
        $DATA_DIR/hadoop/sbin/start-yarn.sh >> LOG-start_spark_hadoop_cluster.log 2>&1
        $DATA_DIR/hadoop/bin/hdfs dfs -mkdir /spark-events >> LOG-start_spark_hadoop_cluster.log 2>&1
        $DATA_DIR/spark/sbin/start-all.sh >> LOG-start_spark_hadoop_cluster.log 2>&1
fi

remote_command="sudo docker pull nvcr.io/nvidia/clara/clara-parabricks:4.0.0-1"

pid_list=""
nodes=0

for ip in `cat /home/$USER/gpu_ips.txt`
do
        echo $ip
        ssh -tt -o "StrictHostKeyChecking no" $ip "$remote_command" > /dev/null 2>&1 &
        pid_list="$pid_list $!"
        nodes=$((nodes+1))
done

pid_list_finished=""
total_nodes=$nodes

while true; 
do
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

        sleep 3
done

echo ""
echo "************************************************************************************************"
echo "                      >>> Probably started hadoop and spark <<< "
echo "Please run command \"yarn node -list\" and \"hdfs dfsadmin -report\" to check the cluster status"
echo "************************************************************************************************" 

