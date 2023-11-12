# AVAH-FABRIC - Accelerating Variant Calling on Human Genomes for FABRIC

This is a subproject of [EVA](https://github.com/MU-Data-Science/EVA).

## Acknowledgments
This work is supported by the National Science Foundation under [Grant No. 2034247](https://nsf.gov/awardsearch/showAward?AWD_ID=2034247) and [Grant No. 2201583](https://www.nsf.gov/awardsearch/showAward?AWD_ID=2201583&HistoricalAwards=false).

## Environments
- Spark 3.0.0, Hadoop 3.2.0, Scala 2.12.8 (for [Adam-Cannoli](https://github.com/bigdatagenomics))
- Spark 2.4.7, Hadoop 2.7.6, Scala 2.11.12 (for [GATK4](https://gatk.broadinstitute.org/hc/en-us))

Hadoop 3+ must use `etc/hadoop/workers` to list the data nodes; check using `hdfs dfsadmin -report`

## Setup and execution of variant analysis using AVAH

The instructions are [here](https://github.com/MU-Data-Science/EVA#running-variant-analysis-on-a-cluster-of-cloudlab-nodes-using-avah).

## To use GPUs when running AVAH

1. Suppose we have 8 nodes in the cluster. 

2. Make sure the cluster software is set up along with Docker/GPU drivers.
    ```
    $ ${HOME}/EVA/cluster_config/cluster-configure.sh 
    Usage: cluster-configure.sh <no. of nodes> <spark3|spark2> [flag]
    
    Options:
      flag: 1 for installing Docker/NVIDIA GPU drivers; 0 otherwise (default: 0)
    ```

    ```
    $ ${HOME}/EVA/cluster_config/cluster-configure.sh 8 spark2 1
    ```

3. Reboot all the CloudLab servers using the CloudLab dashborad. Then restart dfs, yarn, and Spark services on vm0. (Otherwise, iptables don't get updated correctly.)
    ```
    /mydata/hadoop/sbin/start-dfs.sh
    /mydata/hadoop/sbin/start-yarn.sh
    /mydata/spark/sbin/start-all.sh
    ```
4. On `vm0`, you need to modify the `cluster-configure.sh` script in `${HOME/EVA/scripts`.
Do the following to re-rerun the `cluster-configure.sh` script with only the following line:
`scripts=("set-cluster-iptables")`. Then run the below command to protect the cluster nodes from cryptoattacks: 
    ```
    $ cd ${HOME}/EVA/cluster_config/
    $ ./cluster-configure.sh 8 spark2 0
    ```
5. Checkout the `AVAH-FABRIC` repo.
    ```
    $ git clone https://github.com/raopr/AVAH-FABRIC.git
    ```

6. Copy the necessary files.
    ```
    $ python3 ${HOME}/AVAH-FABRIC/scripts/run_remote_command.py copy 8 ${HOME}/AVAH-FABRIC/scripts/run_parabricks.sh /mydata/
    $ python3 ${HOME}/AVAH-FABRIC/scripts/run_remote_command.py copy 8 ${HOME}/AVAH-FABRIC/scripts/check_gpu_usage.sh /mydata/
    ```
    
7. Now start the GPU usage monitoring script with say 2-second intervals.
    ```
    $ python3 ${HOME}/AVAH-FABRIC/scripts/run_remote_command.py gpu_usage 8 start 2
    ```

8. Assuming all the FASTQ files are in HDFS, we can run AVAH with the `-g` option to invoke GPUs. 

    ```
    $ hdfs dfs -rm -r /tmp/logs; hdfs dfs -rm -r /spark-events/*
    $ ${HOME}/AVAH-FABRIC/scripts/run_variant_analysis_at_scale.sh -i /proj/eva-public-PG0/${USER}-sampleIDs-vlarge.txt -d NONE -n 8 -b 2 -p 17 -P H -G -g
    ```
    To run only a subset of RDD partitions on the GPUs (e.g., every other RDD partition):
    ```
    $ hdfs dfs -rm -r /tmp/logs; hdfs dfs -rm -r /spark-events/*
    $ ${HOME}/AVAH-FABRIC/scripts/run_variant_analysis_at_scale.sh -i /proj/eva-public-PG0/${USER}-sampleIDs-vlarge.txt -d NONE -n 8 -b 2 -p 17 -P H -G -g -m 2
    ```
   To run based on first-come, first-served strategy in an RDD partition on the GPUs:
   ```
   $ hdfs dfs -rm -r /tmp/logs; hdfs dfs -rm -r /spark-events/*
   $ ${HOME}/AVAH-FABRIC/scripts/run_variant_analysis_at_scale.sh -i /proj/eva-public-PG0/${USER}-sampleIDs-vlarge.txt -d NONE -n 8 -b 2 -p 17 -P H -G -g -m 2 -F
   ```
   If `-F` is not used, then all the sequences in an eligible RDD partition are forced to run on the GPUs.

9. If you want to stop the GPU usage monitoring script, then do the following:

    ```
    $ python3 ${HOME}/AVAH-FABRIC/scripts/run_remote_command.py gpu_usage 8 stop
    ```
    
    If you want to check the Docker containers that are running on the nodes, then do the following:
    
    ```
    $ python3 ${HOME}/AVAH-FABRIC/scripts/run_remote_command.py docker 8 list
    ```
    
    If you want to kill the containers, then do the following:
    
    ``` 
    $ python3 ${HOME}/AVAH-FABRIC/scripts/run_remote_command.py docker 8 kill
    ```

## Rebuilding the JAR if needed

This is a Scala project. You can use `sbt` to `compile` and `package` the project. The JAR file should be copied manually to `lib/` before executing AVAH.

If you wish to change the `scalaVersion` in build.sbt, run `reload` before rebuilding the JAR.

<!--
1. First create a cluster on CloudLab using `EVA-multi-node-profile`.
See instructions [here](https://github.com/MU-Data-Science/EVA/tree/master/cluster_config).

2. Run the following commands on `vm0`.

```
$ git clone https://github.com/MU-Data-Science/EVA.git
$ cd EVA/cluster_config
$ ./cluster_configure.sh <num_nodes> spark3
```
If the cluster size is large (e.g., 16+ nodes), use the `screen` command first and then run `cluster_configure.sh`.

3. Make sure the reference sequence files (`hs38.*`) are copied to each cluster node on `/mydata`.

4. On `vm0`, do the following:

```
$ git clone https://github.com/raopr/AVAH.git
$ cp AVAH/misc/sample*-vlarge.txt /proj/eva-public-PG0/
```

When YARN runs the job, it will needs these files on all the cluster nodes.

5. Make sure known SNPs and known INDELs folders are on HDFS. Otherwise, use `EVA/scripts/convert_known_snps_indels_to_adam.sh`.

6. Now run the variant analysis.

```
$ ${HOME}/AVAH/scripts/run_variant_analysis_at_scale.sh -i /proj/eva-public-PG0/sampleIDs-vlarge.txt -d /proj/eva-public-PG0/sampleURLs-vlarge.txt -n 16 -b 2 -p 15 -P D
```

6. If you want to run variant analysis again but don't want to re-download the sequences, use `NONE` as shown below:
```
$ ${HOME}/AVAH/scripts/run_variant_analysis.sh -i /proj/eva-public-PG0/sampleIDs-vlarge.txt -d /proj/eva-public-PG0/sampleURLs-vlarge.txt -n 16 -b 2 -p 15 -P D
```




## How to run the JAR directly if needed

```
$SPARK_HOME/bin/spark-submit --master yarn --deploy-mode cluster --num-executors 3 avah_2.12-0.1.jar -i hdfs://vm0:9000/sampleIDs.txt -d hdfs://vm0:9000/sampleURLs.txt
```
OR
```
$SPARK_HOME/bin/spark-submit --master yarn --deploy-mode client --num-executors 3 avah_2.12-0.1.jar -i hdfs://vm0:9000/sampleIDs.txt -d hdfs://vm0:9000/sampleURLs.txt
```
OR
```
$SPARK_HOME/bin/spark-submit --master yarn --deploy-mode client --num-executors 3 --conf spark.yarn.appMasterEnv.CANNOLI_HOME=/mydata/cannoli --conf spark.yarn.appMasterEnv.SPARK_HOME=/mydata/spark --conf spark.executorEnv.CANNOLI_HOME=/mydata/cannoli --conf spark.executorEnv.SPARK_HOME=/mydata/spark avah_2.12-0.1.jar -i hdfs://vm0:9000/sampleIDs.txt
```
-->

## Useful YARN commands

To check YARN jobs:

```
yarn application -list
```

To kill YARN jobs:

```
yarn application -kill <application_ID>
```

To see YARN queues:

```
mapred queue -list
```

To change YARN's scheduler configuration via command line

```
yarn schedulerconf
```

Examples:

```
yarn schedulerconf -global yarn.scheduler.maximum-allocation-mb=16384
```

```
yarn schedulerconf -global yarn.scheduler.maximum-allocation-vcores=32
```

```
yarn schedulerconf -global yarn.scheduler.maximum-allocation-mb=16384,yarn.scheduler.maximum-allocation-vcores=32
```

## To check status

```
yarn queue -status default
```

## To view YARN logs

```
yarn logs -applicationId <application_ID>
```

## To view cluster usage

```
yarn top
```

```
yarn node -all -list
```

```
yarn node -showDetails -list
```

## Monitoring process execution

```
dstat --cpu --mem --load --top-cpu --top-mem -dn --output report.csv 2 10
```

or

```
dstat --cpu --mem --load --top-cpu --top-mem -dn --noupdate --output report.csv 2 10
```

