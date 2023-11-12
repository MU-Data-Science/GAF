export DEBIAN_FRONTEND=noninteractive

spark_version="$2"
spark_hadoop_version="$3"
scala_version="$1"
DATA_DIR='/mydata'

echo "downloading spark and scala"

wget -q http://www.scala-lang.org/files/archive/scala-$scala_version.deb -P $DATA_DIR

wget -q https://archive.apache.org/dist/spark/spark-$spark_version/spark-$spark_version-bin-hadoop$spark_hadoop_version.tgz -P $DATA_DIR > /dev/null


echo "download complete"

for ip in `cat /home/$USER/ips.txt`
do
        scp $DATA_DIR/scala-$scala_version.deb $ip:$DATA_DIR > /dev/null 2>&1
        scp $DATA_DIR/spark-$spark_version-bin-hadoop$spark_hadoop_version.tgz $ip:$DATA_DIR > /dev/null 2>&1
done

echo "sending complete"

sudo apt install rand

key=$(openssl rand -base64 64 | tr -dc 'a-zA-Z0-9' | fold -w 128 | head -n 1)

remote_command='
sudo dpkg -i '$DATA_DIR'/scala-'$scala_version'.deb
tar zxf '$DATA_DIR'/spark-'$spark_version'-bin-hadoop'$spark_hadoop_version'.tgz -C '$DATA_DIR' ; mv '$DATA_DIR'/spark-'$spark_version'-bin-hadoop'$spark_hadoop_version' '$DATA_DIR'/spark
'
pid_list=""

for ip in `cat /home/$USER/ips.txt`
do
        ssh -tt -o "StrictHostKeyChecking no" $ip "$remote_command" &
        pid_list="$pid_list $!"
done

while true;
do
        if ps -p $pid_list > /dev/null
        then
                echo "installing spark and scala..."
                sleep 20
        else
                break
        fi
done

echo "done setup"


key=$(openssl rand -base64 64 | tr -dc 'a-zA-Z0-9' | fold -w 128 | head -n 1)
master_ip="vm0"
#master_ip=""
#
#for ip in `cat /home/$USER/ips.txt`
#do
#        master_ip=$ip
#        break
#done

command_n='
echo "#spark.executor.memory        12g
#spark.executor.cores         4
spark.executor.memory        6g
spark.executor.cores         4
spark.shuffle.blockTransferService nio
spark.network.timeout        840000
spark.shuffle.io.maxRetries  10
spark.shuffle.io.retryWait   30
spark.eventLog.dir           hdfs://'$master_ip':9000/spark-events
spark.eventLog.enabled       true
spark.authenticate           true
spark.authenticate.secret    '$key'
spark.ui.enabled             false" > '$DATA_DIR'/spark/conf/spark-defaults.conf
'


command_pyspark='
echo "export PYTHONPATH=$(ZIPS=("$SPARK_HOME"/python/lib/*.zip); IFS=:; echo "${ZIPS[*]}"):$PYTHONPATH" >> /home/$USER/.bashrc
echo "export PYSPARK_PYTHON=python3" >> /home/$USER/.bashrc ; source ~/.bashrc
'

#/home/$USER/genome_tools/hadoop/bin/hdfs dfs -mkdir /spark-events
#hdfs dfs -mkdir /spark-events

for ip in `cat /home/$USER/ips.txt`
do
        command_env='
        echo "export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64
export SPARK_MASTER_HOST='$master_ip'
export SPARK_PUBLIC_DNS='$master_ip'
export SPARK_LOCAL_DIRS='$DATA_DIR'/spark/spark-tmp
export SPARK_WORKER_OPTS=\"-Dspark.worker.cleanup.enabled=true -Dspark.worker.cleanup.interval=7200 -Dspark.worker.cleanup.appDataTtl=1800\"
export SPARK_LOCAL_IP='$ip'" > '$DATA_DIR'/spark/conf/spark-env.sh '

        ssh -o "StrictHostKeyChecking no" $ip "mkdir '$DATA_DIR'/spark/spark-tmp"
        ssh -o "StrictHostKeyChecking no" $ip "$command_n" &
        ssh -o "StrictHostKeyChecking no" $ip "$command_env" &
        ssh -o "StrictHostKeyChecking no" $ip "$command_pyspark" &
        ssh -o "StrictHostKeyChecking no" $ip "cp /home/$USER/workers '$DATA_DIR'/spark/conf/slaves"

done
            
