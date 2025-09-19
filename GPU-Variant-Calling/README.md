# Optimizing Variant Calling Pipeline Execution on Human Genomes Using GPU-Enabled Machines
Variant calling is the first step in analyzing a human genome
and aims to detect variants in an individual’s genome compared to a reference
genome. Due to the computationally-intensive nature of variant
calling, genomic data are increasingly processed in cloud environments
as large amounts of compute and storage resources can be acquired with
the pay-as-you-go pricing model. In this paper, we address the problem
of efficiently executing a variant calling pipeline for a workload of human
genomes on graphics processing unit (GPU)-enabled machines. We
propose a novel machine learning (ML)-based approach for optimizing
the workload execution to minimize the total execution time. Our approach
encompasses two key techniques: The first techniques employs
ML to predict the execution times of different stages in a variant calling
pipeline based on the characteristics of a genome sequence. Using
the predicted times, the second technique generates optimal execution
plans for the machines by drawing inspiration from the flexible job shop
scheduling problem. The plans are executed via careful synchronization
across different machines. We evaluated our approach on a workload of
publicly available genome sequences using a testbed with different types
of GPU hardware. We observed that our approach was effective in predicting
the execution times of variant calling pipeline stages using ML
based on features such as sequence size, read quality, percentage of duplicate
reads, and so on. In addition, our approach was nearly 2× faster
than a greedy approach on the tested workload.   

**Keywords:** Human genomes · Variant calling · Optimized execution  
<p align="center">
  <img src="https://github.com/user-attachments/assets/dcdaad73-af72-4cbb-b4e8-bd4f0a470f3d" alt="Screenshot 2025-09-18 at 14 19 06" width="600" height="600">
</p>


## Instructions: 
**Step: 1** Create `n` node cluster in [FABRIC](https://portal.fabric-testbed.net) research testbed using, `create_cluster.ipynb.html` script in JupyterLab of FABRIC account (copy-paste the `create_cluster.ipynb.html` script in FABRIC JupyterLab from `/notebooks/` directory).   
[Click here to see the cluster](https://shorturl.at/Aw5bn)  
_Note: For this experimentation, a FABRIC slice cluster of 6-nodes (vm0 to vm5), 8-cores each, 64-GB RAM each, and 1000-GB in each HDD was used. The slice was created in `FIU or STAR` site._
   
**Step: 2** Clone the repository `GPU-Variant-Calling` on `vm0` and `vm1`.  
```
git clone https://github.com/raopr/GPU-Variant-Calling
```

**Step: 3** Configure NFS-Server for distributed workload execution from `vm1 to vm5`. Follow instructions here, [NFS-configuration-instructions](https://github.com/raopr/GPU-Variant-Calling/blob/main/repo/config/nfs_server/README.md).

**Step: 4** Install Nvidia drivers on worker nodes (`vm1` to `vm5`) using [Nvidia-instructions](https://github.com/raopr/GPU-Variant-Calling/tree/main/repo/config/setup).  
Note: Please make sure nvidia drivers are correctly installed. You can check using `nvidia-smi` command on all the workers nodes.  

**Step: 5** Attach `NVME storage` at `vm1` and share using `NFS` from `VM2 to VM5` using [NVME-instructions](https://github.com/raopr/GPU-Variant-Calling/blob/main/repo/config/NVME_setup.MD).  

**Step: 6** Install Anaconda if not already installed using documentation, [anaconda-documentation](https://docs.anaconda.com/anaconda/install/linux/).  
   - Create and activate conda environment using [requirements.yml](https://github.com/raopr/GPU-Variant-Calling/blob/main/repo/requirements.yml).
   - Command to run on all worker nodes,   
```
conda env create -n myenv -f requirements.yml    
```   
```  
conda activate myenv
```  
**Step: 7** Download or keep the input files (`.fastq.gz` files) in the `/mnt/nvme` directory, you can use [URLs.txt](https://github.com/raopr/GPU-Variant-Calling/blob/main/repo/data/URLs.txt) file to download input files using `wget -i URLs.txt`.  

**Step: 8** Download the reference genome at `vm1` node in a NFS-shared folder `/mydata/GPU-Variant-Calling/`.  

**Step: 9** Moving the files from [folder](https://github.com/raopr/GPU-Variant-Calling/src/vm0_files) to `/home/ubuntu/` of `vm0`.  

**Step: 10** Utilize the python notebook for prediction and testing task, [Notebooks](https://github.com/raopr/GPU-Variant-Calling/notebooks).  

**Step: 11** Execute the `script.sh` from `vm0` $HOME directory like,  
```
./script.sh number_subsets number_worker_vms number_subsets algorithm input_filename
```
Example: 
```
./script.sh 10 5 10 1 ./data/input.csv
```
```
./script.sh 10 5 10 2 ./data/input_S2.csv
```


# Personnel

## Faculty
Prof. Praveen Rao (University of Missouri)

Prof. Peter Sanders (Karlsruhe Institute of Technology)

## Ph.D. Student
Ajay Kumar (University of Missouri)

# Acknowledgments
This work is supported by the National Science Foundation Grant No. 2201583.

