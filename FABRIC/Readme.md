# Running AVAH on Fabric

## Setting up Fabric Cluster

Steps given below assume you have access to FABRIC, are a part of GAF project and have your SSH keys setup

1. On Jupyter Hub clone `git clone https://github.com/MU-Data-Science/GAF.git --depth 1`
2. To create a 8 or 16 node cluster on Fabric, go to `Jupyter Hub` on FABRIC portal. From the direcotry structure pane on the left, navigate to `GAF` > `FABRIC` > `scripts`  and open `create_cluster.ipynb`

  ![Screenshot 2023-11-29 at 11 26 48 AM](https://github.com/MU-Data-Science/GAF/assets/22073166/e0957486-8381-4197-ba52-42e145a51b23)

3. Follow the steps 1 till 3 in the notebook.</br> 
Step one loades the Fablib library and list sites and their availability of GPUs, NICs and available CPU cores.  
  - Modify the instance worker configuration as needed e.g., `fabric.c24.m128.d500` will instantiate 24 core processor, 128GB ram and 500 gb storage. 
  - Choose a site location with GPUs one less than the combined total of RTX and T4 GPUs, matching the number of nodes. For instance, in an 8-node cluster, allocate 7 GPUs, with the master node wihtout a GPU.
  - Wait for the slice submit to finish successfully
  - Run Step 3 block to setup ssh, worker ips 
  - Extend the slice from `Step 4` or navigate to `Fabric Portal` > `Experiments` > `My Slices` > `your experiment` and you should be able to see `Lease End at` option to extend the lease. 
 - While setting up the cluster, make sure you have `nat64.sh` file in ~/work directory of fabric jupyter notebook, if not exits, copy it from ~/GAF/FABRIC/scripts/nat64.sh.
4. To setup Hadoop, Spark, and other tools, execute the following in the shell/terminal. Suppose your cluster has 16 nodes on vm0.
  ```
      screen -S setup
      cd ${HOME}/GAF/FABRIC/scripts
      ./cluster_config.sh -G
  ```
5. After the installation is complete run `source ~/.bashrc` of logout and login again.
6. cd `~/GAF/FABRIC/scripts` and run `./start_spark_hadoop_cluster.sh` to start spark and haddop cluster
7. To run GATK we need to modify few files. On the master node open `/mydata/hadoop/etc/hadoop/yarn-site.xml` and add following properties:
 ```
 <property>
             <name>yarn.nodemanager.vmem-check-enabled</name>
             <value>false</value>
</property>
<property>
             <name>yarn.nodemanager.pmem-check-enabled</name>
             <value>false</value>
</property>

```
In the same file change <values> for following properties. 
 
 `yarn.scheduler.maximum-allocation-vcores` to `40` <br>
 `yarn.scheduler.minimum-allocation-vcore` to `1` <br>
 `yarn.nodemanager.resource.cpu-vcores` to `20` <br>
 `yarn.nodemanager.maximum-allocation-mb` to `91440` <br>
 `yarn.nodemanager.resource.memory-mb` to `102000` <br> 
 
 Copy the updated `yarn-site.xml` file to all the nodes and upadte the settings. Assuming we have 8 nodes:
 
 ```
 $ python3 ${HOME}/GAF/FABRIC/scripts/run_remote_command.py copy 8 /mydata/hadoop/etc/hadoop/yarn-site.xml /mydata/hadoop/etc/hadoop/
 $ /mydata/hadoop/sbin/stop-yarn.sh
 $ /mydata/hadoop/sbin/start-yarn.sh
 ```

 Also edit `/mydata/spark/conf/spark-defaults.conf` on master node to have the following properties:
 
 ```
 spark.executor.memory        12g
 spark.executor.memoryOverhead 5g
 ```

 Similarly copy the `spark-defaults.conf` on all the nodes and update settings:
 
 ``` 
 $ python3 ${HOME}/GAF/FABRIC/scripts/run_remote_command.py copy 8 /mydata/spark/conf/spark-defaults.conf /mydata/spark/conf/
 $ /mydata/spark/sbin/stop-all.sh
 $ /mydata/spark/sbin/start-all.sh
 ```

8. Copy reference Genome files, genome ID file, other necessary files and fastq files </br>
  - Nine reference files are needed to run the GATK pipeline. To copy these files from the master (vm0) to the worker nodes it is assumed that the files are already in the mydata folder of the master. To copy the files to the workers run the following script inside of the GAF folder:  
  - Ensure following files are present in `/mydata` before running the command below:
  - Link for ref genome: [click here](https://mailmissouri-my.sharepoint.com/:u:/g/personal/raopr_umsystem_edu/EcAAl7ea7kJGiVzbMuI5XicBOq2jEXeLN38rL8NSqii5HQ?e=8njtmq)
  - Or transfer using scp from cloudlab to fabric cluster. Copy all hs38 file from any vm on cloudlab to vm0 (fabric) and and then use the script mentioned in next step to copy hs38* files on all the nodes in the cluster. Roughly the command would look like this : `scp vm6:/proj/eva-public/hs38* vm0:/mydata/`
  - Copy all hs38* files to worker nodes using `copy_ref.sh`.
  - Another way (probably the easiest) is to download the reference genomes zip and copy to all nodes /mydata
 ```
  cd /mydata
  pip install gdown
  gdown https://drive.google.com/file/d/1q-eg2hQPpsnqT7wFtld7Inc83lwyhh-n/view?usp=sharing --fuzzy
  for i in {0..7}; do scp vm0:/mydata/genomeRefFiles.zip vm$i:/mydata/ ;done
  for i in {0..7}; do ssh vm$i "unzip -o /mydata/genomeRefFiles.zip -d /mydata/" & done; wait
  for i in {0..7}; do ssh vm$i "mv /mydata/genomeRefFiles/* /mydata" & done; wait
  These steps will take sometime to complete operation on all nodes
 ```
make sure the following files are present in `/mydata`
 ```
 hs38.dict
 hs38.fa.amb
 hs38.fa.bwt
 hs38.fa.img
 hs38.fa.sa
 hs38.fa
 hs38.fa.ann
 hs38.fa.fai
 hs38.fa.pac
 ```

9. We also need some extra files to be copied soo run the following piece of code <br> <br>
 
 ```
 $ python3 ${HOME}/GAF/FABRIC/scripts/run_remote_command.py copy 8 ${HOME}/GAF/FABRIC/scripts/run_parabricks.sh /mydata/
 $ python3 ${HOME}/GAF/FABRIC/scripts/run_remote_command.py copy 8 ${HOME}/GAF/FABRIC/scripts/check_gpu_usage.sh /mydata/
 ```

:exclamation: Following method assumes you have atleast 8 node cluster to utilise goole drive api for fastq files download :exclamation: <br> <br>
10. Now we need to download FASTQ files using google drive api. We will be needing Google OAuth token to download fastq files. 
  - In a borwser window, Go to OAuth 2.0 Playground  `developers.google.com/oauthplayground/`
  - In the "Select the Scope" box, paste  `https://www.googleapis.com/auth/drive.readonly` and press enter 
  - Click Authorize APIs and then `Exchange authorization code for tokens` button and copy the `Acces token`
    ![Screenshot 2023-11-29 at 11 22 41 AM](https://github.com/MU-Data-Science/GAF/assets/22073166/1f41c846-08f2-432b-b7cf-2c9169561f02)
  - On FABRIC vm0 (master node) `cd ${HOME}/GAF/FABRIC/Google Drive Experimental Codes/`
  - run `fastqDownloads.py` to start downloading fastq files on 8 nodes using `python3 ./fastqDownloads.py` 
  - Running this script will ask to input the auth token. Paste the token copied in previous step and hit enter
  - Script should start downloading fastqFiles on 8 nodes in screen sessions. Downloaded fastq files will be placed under `/mydata/fastqFiles` on vm0 till vm7. 
  - Once all downloads are complete, on each node run `hdfs dfs -put /mydata/fastqFiles/*fastq.gz /`
  - Or if you want to move downloaded fastq files from all the VMs to VM0, in a screen run this command `for i in {0..7}; do scp vm$i:/mydata/fastqFiles/* vm0:/mydata/genomes ;done`. It will copy the downladed fastq files from all the vms to vm0:/mydata/genomes/
  - Another approach is to send the command to each node to move to hdfs `for i in {0..7}; do ssh vm$i "/mydata/hadoop/bin/hdfs dfs -put /mydata/fastqFiles/* /" & done; wait` 

 :exclamation: copying might throw following error `Name node is in safe mode` :exclamation: <br> <br> 

 Restarting dfs should resolve the error 
 ```
 /mydata/hadoop/sbin/stop-dfs.sh
 /mydata/hadoop/sbin/start-dfs.sh
 ```
11. Run `run_dstat.py` and `run_gpu_stat.py` before starting AVAH's execution.
```
$ python3 ${HOME}/GAF/FABRIC/scripts/run_dstat.py start 8
$ python3 ${HOME}/GAF/FABRIC/scripts/run_gpu_stat.py start 8
```

12. Run AVAH 

 ```
 ${HOME}/GAF/FABRIC/scripts/run_variant_analysis_at_scale.sh -i /proj/eva-public-PG0/${USER}-sampleIDs-vlarge.txt -d NONE -n 8 -b 2 -p 17 -P H -G -g
 ```
  - Refer to AVAH-FABRIC for running AVAH pipeline. It has the latest changes. Just clone on vm0 and rest is all good. 
  - For AVAH with CPU only execution (`-G`), use `-p 7`. Otherwise, you may see `.retry` files for some sequences. 
  - There are several ways to run AVAH. Refer to original [AVAH-STAR Repo](https://github.com/MU-Data-Science/GAF/tree/main/AVAH-STAR) for more options 

13. Once the experiment completes,  stop `run_dstat.py` and `run_gpu_stat.py` 
```
$ python3 ${HOME}/GAF/FABRIC/scripts/run_dstat.py stop 8
$ python3 ${HOME}/GAF/FABRIC/scripts/run_gpu_stat.py stop 8
```

14. Collect the AVAH log file from the master node, and dstat and gpu_stat files from all the worker nodes.
```
$ python3 ${HOME}/GAF/FABRIC/scripts/run_dstat.py 8 collect
$ python3 ${HOME}/GAF/FABRIC/scripts/run_gpu_stat.py 8 collect
```


























