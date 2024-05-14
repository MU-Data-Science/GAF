# Running AVAH on Fabric

This guide provides instructions on how to set up a Fabric cluster and run AVAH (Automated Variant Analysis at scale using Hadoop). It assumes that you have access to FABRIC, are a part of the GAF project, and have your SSH keys set up.

## Setting up Fabric Cluster

1. Clone the GAF repository on Jupyter Hub: 
    ```
    git clone https://github.com/MU-Data-Science/GAF.git
    ```

2. To create an 8 or 16 node cluster on Fabric, navigate to `GAF` > `FABRIC` > `scripts` in the directory structure pane on the left of the Jupyter Hub on the FABRIC portal. Open `create_cluster.ipynb`.

3. Follow steps 1 to 3 in the notebook. It is recommended to use step 2(b) over 2(a) to create the instance and submit the slice creation request. However, you can use 2(a) as well. 

4. To set up Hadoop, Spark, and other tools, execute the following in the terminal. Suppose your cluster has 16 nodes on vm0.

    ```bash
    screen -S setup
    cd ${HOME}/GAF/FABRIC/scripts
    ./cluster_config.sh -G
    ```

5. After the installation is complete, run `source ~/.bashrc` or logout and login again.

6. Navigate to `~/GAF/FABRIC/scripts` and run `./start_spark_hadoop_cluster.sh` to start the Spark and Hadoop cluster.

7. To run GATK, we need to modify a few files. On the master node, open `/mydata/hadoop/etc/hadoop/yarn-site.xml` and add the following properties:

    ```xml
    <property>
        <name>yarn.nodemanager.vmem-check-enabled</name>
        <value>false</value>
    </property>
    <property>
        <name>yarn.nodemanager.pmem-check-enabled</name>
        <value>false</value>
    </property>
    ```

8. Copy reference Genome files, genome ID file, other necessary files, and fastq files. Nine reference files are needed to run the GATK pipeline. To copy these files from the master (vm0) to the worker nodes, it is assumed that the files are already in the mydata folder of the master. To copy the files to the workers, run the following script inside of the GAF folder. Ensure the following files are present in `/mydata` before running the command below:

    ```bash
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

9. We also need some extra files to be copied, so run the following commands:

    ```bash
    python3 ${HOME}/GAF/FABRIC/scripts/run_remote_command.py copy 8 ${HOME}/GAF/FABRIC/scripts/run_parabricks.sh /mydata/
    python3 ${HOME}/GAF/FABRIC/scripts/run_remote_command.py copy 8 ${HOME}/GAF/FABRIC/scripts/check_gpu_usage.sh /mydata/
    ```
## Downloading FASTQ Files Using Google Drive API

10. To download FASTQ files, we will need a Google OAuth token. Follow the steps below to obtain the token:

    a. Open a browser window and navigate to [OAuth 2.0 Playground](https://developers.google.com/oauthplayground/).
    
    b. In the "Select the Scope" box, paste `https://www.googleapis.com/auth/drive.readonly` and press enter.
    
    c. Click "Authorize APIs" and then "Exchange authorization code for tokens" button.
    
    d. Copy the Access token.

11. On the FABRIC vm0 (master node), navigate to the Google Drive Experimental Codes directory:
    ```bash
    cd ${HOME}/GAF/FABRIC/Google Drive Experimental Codes/
    ```
12. Run `fastqDownloads.py` to start downloading fastq files on 8 nodes:
    ```bash
    python3 ./fastqDownloads.py
    ```
    When prompted, paste the token copied in the previous step and hit enter. The script should start downloading fastqFiles on 8 nodes in screen sessions. Downloaded fastq files will be placed under `/mydata/fastqFiles` on vm0 till vm7.

13. Once all downloads are complete, on each node run:
    ```bash
    hdfs dfs -put /mydata/fastqFiles/*fastq.gz /
    ```
    If you want to move downloaded fastq files from all the VMs to VM0, in a screen run this command:
    ```bash
    for i in {0..7}; do scp vm$i:/mydata/fastqFiles/* /mydata/genomes ;done
    ```
    This will copy the downloaded fastq files from all the VMs to `vm0:/mydata/genomes/`.

    ❗ Note: Copying might throw the following error: "Name node is in safe mode". Restarting dfs should resolve the error:
    ```bash
    /mydata/hadoop/sbin/stop-dfs.sh
    /mydata/hadoop/sbin/start-dfs.sh
    ```

14. Run `run_dstat.py` and `run_gpu_stat.py` before starting AVAH's execution:
    ```bash
    python3 ${HOME}/GAF/FABRIC/scripts/run_dstat.py 8 start
    python3 ${HOME}/GAF/FABRIC/scripts/run_gpu_stat.py 8 start
    ```

15. Run AVAH:
    ```bash
    ${HOME}/GAF/FABRIC/scripts/run_variant_analysis_at_scale.sh -i /proj/eva-public-PG0/${USER}-sampleIDs-vlarge.txt -d NONE -n 8 -b 2 -p 17 -P H -G -g
    ```
    For AVAH with CPU only execution (-G), use `-p 7`. Otherwise, you may see `.retry` files for some sequences. There are several ways to run AVAH. Refer to the original AVAH-STAR Repo for more options.

16. Once the experiment completes, stop `run_dstat.py` and `run_gpu_stat.py`:
    ```bash
    python3 ${HOME}/GAF/FABRIC/scripts/run_dstat.py 8 stop
    python3 ${HOME}/GAF/FABRIC/scripts/run_gpu_stat.py 8 stop
    ```

17. Collect the AVAH log file from the master node, and dstat and gpu_stat files from all the worker nodes:
    ```bash
    python3 ${HOME}/GAF/FABRIC/scripts/run_dstat.py 8 collect
    python3 ${HOME}/GAF/FABRIC/scripts/run_gpu_stat.py 8 collect
    ```
