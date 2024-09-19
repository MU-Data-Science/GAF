### Assuming that the first IP of the cluster will be the master and the rest will be the workers ###

hadoop_ver="$4"
hadoop_sub_ver="$5"
DATA_DIR='/mydata'
master_ip="vm0"
#master_ip=""
#
#for ip in `cat /home/$USER/ips.txt`
#do
#        master_ip=$ip
#        break
#done

remote_command='
sudo chown -R $USER:$USER /mydata
mkdir /home/$USER/hadoop_data
chmod 777 /home/$USER/hadoop_data
'

for ip in `cat /home/$USER/ips.txt`
do
	ssh -o "StrictHostKeyChecking no" $ip "$remote_command"
done

wget https://archive.apache.org/dist/hadoop/core/hadoop-$hadoop_version.$hadoop_sub_version/hadoop-$hadoop_version.$hadoop_sub_version.tar.gz -P $DATA_DIR > /dev/null 2>&1

echo "Hadoop download on master node complete"

for ip in `cat /home/$USER/ips.txt`
do
	scp $DATA_DIR/hadoop-$hadoop_ver.$hadoop_sub_ver.tar.gz $ip:$DATA_DIR > /dev/null 2>&1
done

echo "hadoop sent to all nodes in the cluster"
echo "unzipping hadoop on all nodes"

remote_command_tar='
tar zxf '$DATA_DIR'/hadoop-'$hadoop_ver'.'$hadoop_sub_ver'.tar.gz -C '$DATA_DIR'
mv '$DATA_DIR'/hadoop-'$hadoop_ver'.'$hadoop_sub_ver' '$DATA_DIR'/hadoop
'

# To Do: Track the pid, if it finishes exit.
for ip in `cat /home/$USER/ips.txt`
do
        ssh -o "StrictHostKeyChecking no" $ip "$remote_command_tar" &
done

echo "configuring hadoop..."
#do not want to check whether the transfer of files are done or not by checking the PIDs, so just a ugly workaround wait for few seconds, it is usually done correctly
sleep 15
##############################################################################################
################# Configuration changes for all nodes in the cluster #########################
##############################################################################################

remote_command_core_site='
echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<?xml-stylesheet type=\"text/xsl\" href=\"configuration.xsl\"?>
<configuration>
  <property>
      <name>fs.defaultFS</name>
      <value>hdfs://'$master_ip':9000</value>
  </property>
  <property>
      <name>hadoop.tmp.dir</name>
      <value>file:/mydata/hadoop_data</value>
  </property>
  <property>
    <name>dfs.webhdfs.enabled</name>
    <value>false</value>
  </property>
</configuration>" > '$DATA_DIR'/hadoop/etc/hadoop/core-site.xml
'

remote_command_hadoop_env='
echo "HADOOP_IDENT_STRING=$USER
JAVA_HOME=\"/usr/lib/jvm/java-1.8.0-openjdk-amd64\"" >>'$DATA_DIR'/hadoop/etc/hadoop/hadoop-env.sh
'

remote_command_mapred_site='
echo "<?xml version=\"1.0\"?>
<?xml-stylesheet type=\"text/xsl\" href=\"configuration.xsl\"?>
<configuration>
  <property>
    <name>mapreduce.framework.name</name>
    <value>yarn</value>
  </property>
</configuration>" > '$DATA_DIR'/hadoop/etc/hadoop/mapred-site.xml
'

remote_command_yarn_site='
echo "<?xml version=\"1.0\"?>
<?xml-stylesheet type=\"text/xsl\" href=\"configuration.xsl\"?>
<configuration>
     <property>
          <name>yarn.nodemanager.aux-services</name>
          <value>mapreduce_shuffle</value>
      </property>
      <property>
          <name>yarn.nodemanager.aux-services.mapreduce.shuffle.class</name>
          <value>org.apache.hadoop.mapred.ShuffleHandler</value>
      </property>
      <property>
               <name>yarn.resourcemanager.resource-tracker.address</name>
               <value>'$master_ip':8025</value>
      </property>
      <property>
               <name>yarn.resourcemanager.scheduler.address</name>
               <value>'$master_ip':8030</value>
      </property>
      <property>
               <name>yarn.resourcemanager.address</name>
               <value>'$master_ip':8050</value>
       </property>
       <property>
                <name>yarn.resourcemanager.application-https.policy</name>
                <value>STRICT</value>
      </property>
      <property>
                <name>yarn.log-aggregation-enable</name>
                <value>true</value>
      </property>
      <property>
                <name>yarn.acl.enable</name>
                <value>true</value>
      </property>
      <property>
                <name>yarn.admin.acl</name>
                <value>'$USER'</value>
      </property>
      <property>
               <name>yarn.scheduler.maximum-allocation-mb</name>  <!--Max RAM-per-container-->
               <value>9000</value>
      </property>
            <property>
               <name>yarn.scheduler.minimum-allocation-mb</name>
               <value>2048</value>
      </property>
      <property>
               <name>yarn.nodemanager.resource.memory-mb</name>  <!--Max RAM-per-node-->
               <value>11000</value>
      </property>
      <property>
               <name>yarn.scheduler.maximum-allocation-vcores</name>
               <value>4</value>
      </property>
      <property>
               <name>yarn.scheduler.minimum-allocation-vcores</name>
               <value>2</value>
      </property>
      <property>
               <name>yarn.nodemanager.resource.cpu-vcores</name>
               <value>4</value>
      </property>

</configuration>" > '$DATA_DIR'/hadoop/etc/hadoop/yarn-site.xml
'

remote_command_capacity='
echo"<?xml version=\"1.0\"?>
<configuration>
  <property>
    <name>yarn.scheduler.capacity.maximum-applications</name>
    <value>10000</value>
    <description>
      Maximum number of applications that can be pending and running.
    </description>
  </property>
  <property>
    <name>yarn.scheduler.capacity.maximum-am-resource-percent</name>
    <value>0.3</value>
    <description>
      Maximum percent of resources in the cluster which can be used to run
      application masters i.e. controls number of concurrent running
      applications.
    </description>
  </property>
  <property>
    <name>yarn.scheduler.capacity.resource-calculator</name>
    <value>org.apache.hadoop.yarn.util.resource.DominantResourceCalculator</value>
    <description>
      The ResourceCalculator implementation to be used to compare
      Resources in the scheduler.
      The default i.e. DefaultResourceCalculator only uses Memory while
      DominantResourceCalculator uses dominant-resource to compare
      multi-dimensional resources such as Memory, CPU etc.
    </description>
  </property>
  <property>
    <name>yarn.scheduler.capacity.root.queues</name>
    <value>default</value>
    <description>
      The queues at the this level (root is the root queue).
    </description>
  </property>
  <property>
    <name>yarn.scheduler.capacity.root.default.capacity</name>
    <value>100</value>
    <description>Default queue target capacity.</description>
  </property>
  <property>
    <name>yarn.scheduler.capacity.root.default.user-limit-factor</name>
    <value>1</value>
    <description>
    </description>
  </property>
  <property>
    <name>yarn.scheduler.capacity.root.default.maximum-capacity</name>
    <value>100</value>
    <description>
    </description>
  </property>
  <property>
    <name>yarn.scheduler.capacity.root.default.state</name>
    <value>RUNNING</value>
    <description>
    </description>
  </property>
  <property>
    <name>yarn.scheduler.capacity.root.default.acl_submit_applications</name>
    <value>'$USER'</value>
    <description>
    </description>
  </property>
  <property>
    <name>yarn.scheduler.capacity.root.default.acl_administer_queue</name>
    <value>'$USER'</value>
    <description>
    </description>
  </property>
  <property>
    <name>yarn.scheduler.capacity.root.default.acl_application_max_priority</name>
    <value>*</value>
    <description>
    </description>
  </property>
   <property>
     <name>yarn.scheduler.capacity.root.default.maximum-application-lifetime
     </name>
     <value>-1</value>
     <description>
     </description>
   </property>
   <property>
     <name>yarn.scheduler.capacity.root.default.default-application-lifetime
     </name>
     <value>-1</value>
     <description>
     </description>
   </property>
  <property>
    <name>yarn.scheduler.capacity.node-locality-delay</name>
    <value>40</value>
    <description>
    </description>
  </property>
  <property>
    <name>yarn.scheduler.capacity.rack-locality-additional-delay</name>
    <value>-1</value>
    <description>
    </description>
  </property>
  <property>
    <name>yarn.scheduler.capacity.queue-mappings</name>
    <value></value>
    <description>
    </description>
  </property>
  <property>
    <name>yarn.scheduler.capacity.queue-mappings-override.enable</name>
    <value>false</value>
    <description>
    </description>
  </property>
  <property>
    <name>yarn.scheduler.capacity.per-node-heartbeat.maximum-offswitch-assignments</name>
    <value>1</value>
    <description>
    </description>
  </property>
  <property>
    <name>yarn.scheduler.capacity.application.fail-fast</name>
    <value>false</value>
    <description>
    </description>
  </property>
</configuration>" > $DATA_DIR/hadoop/etc/hadoop/capacity-scheduler.xml
'

copy_worker='
cp /home/$USER/workers '$DATA_DIR'/hadoop/etc/hadoop/workers
cp /home/$USER/workers '$DATA_DIR'/hadoop/etc/hadoop/slaves
'

for ip in `cat /home/$USER/ips.txt`
do
        echo $ip
        ssh -o "StrictHostKeyChecking no" $ip "$remote_command_core_site" &
        ssh -o "StrictHostKeyChecking no" $ip "$remote_command_hadoop_env" &
        ssh -o "StrictHostKeyChecking no" $ip "$remote_command_mapred_site" &
        ssh -o "StrictHostKeyChecking no" $ip "$remote_command_yarn_site" &
        ssh -o "StrictHostKeyChecking no" $ip "$copy_worker" &
done

####################################################################################################
########################### Configuration changes for master node ##################################
####################################################################################################


master_command='
mkdir /home/$USER/namenode_data
chmod 777 /home/$USER/namenode_data
echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<?xml-stylesheet type=\"text/xsl\" href=\"configuration.xsl\"?>
<configuration>
    <property>
      <name>dfs.replication</name>
      <value>2</value>
    </property>
    <property>
      <name>dfs.namenode.name.dir</name>
      <value>/mydata/hadoop_data/namenode_data</value>
    </property>
    <property>
      <name>dfs.permissions</name>
      <value>false</value>
    </property>
</configuration>" > '$DATA_DIR'/hadoop/etc/hadoop/hdfs-site.xml
'

####################################################################################################
########################### Configuration changes for worker node ##################################
####################################################################################################

worker_command='
mkdir /home/$USER/hadoop_data/datanode
echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<?xml-stylesheet type=\"text/xsl\" href=\"configuration.xsl\"?>
<configuration>
    <property>
      <name>dfs.replication</name>
      <value>2</value>
    </property>
    <property>
          <name>dfs.datanode.data.dir</name>
          <value>/mydata/hadoop/datanode</value>
    </property>
    <property>
      <name>dfs.permissions</name>
      <value>false</value>
    </property>
</configuration>" > '$DATA_DIR'/hadoop/etc/hadoop/hdfs-site.xml
'

##################################################################################################

i=0 #Assuming first IP is assigned to the master
for ip in `cat /home/$USER/ips.txt`
do
        if [ $i -eq 0 ];
        then
                ssh -o "StrictHostKeyChecking no" $ip "$master_command" &
                ssh -o "StrictHostKeyChecking no" $ip "echo $master_ip > /home/$USER/masters"
                ssh -o "StrictHostKeyChecking no" $ip "cp /home/$USER/masters /mydata/hadoop/etc/hadoop/masters"
                i=$((i+1))
        else
                ssh -o "StrictHostKeyChecking no" $ip "$worker_command" &
        fi
done

remote_command='
mkdir '$DATA_DIR'/hadoop/logs ; chmod 777 '$DATA_DIR'/hadoop/logs
'

remote_command_env='
echo "export HADOOP_HOME='$DATA_DIR'/hadoop
 export HADOOP_MAPRED_HOME=\$HADOOP_HOME
 export HADOOP_COMMON_HOME=\$HADOOP_HOME
 export HADOOP_HDFS_HOME=\$HADOOP_HOME
 export YARN_HOME=\$HADOOP_HOME
 export HADOOP_COMMON_LIB_NATIVE_DIR=\$HADOOP_HOME/lib/native
 export HADOOP_OPTS=\"-Djava.library.path=\$HADOOP_HOME/lib\"

 
 export SPARK_HOME='$DATA_DIR'/spark

 export HADOOP_CONF_DIR=\$HADOOP_HOME/etc/hadoop

 # New Path
 export PATH=\$PATH:\$HADOOP_HOME/bin:\$HADOOP_HOME/sbin:\$SPARK_HOME/bin" >> /home/$USER/.bashrc ; source /home/$USER/.bashrc
 '

for ip in `cat /home/$USER/ips.txt`
do
        ssh -o "StrictHostKeyChecking no" $ip "$remote_command"
        ssh -o "StrictHostKeyChecking no" $ip "$remote_command_env"
done

