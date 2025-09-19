# Optimizing Variant Calling Pipeline Execution on Human Genomes Using GPU-Enabled Machines

## Prequisites: 
1. Install anaconda, create environment, install `packages using requirement.txt`.
2. Keep the data (input files) in `./data` directory and, refernece genome at `vm1` in NFS-shared directory.

## Instructions: 
**Step: 1** Create `n` node (`for this experiment consider using 5-node cluster`) cluster in your chosen testbed.

**Step: 2** Clone the repository `GAF` on `vm0` and `vm1`.  
```
git clone http://github.com/MU-Data-Science/GAF.git
```

**Step: 3** Configure NFS-Server for distributed workload execution from `vm1 to vm5`. Follow instructions here, [NFS-configuration-instructions](https://github.com/MU-Data-Science/GAF/tree/main/GPU-Variant-Calling/config/nfs_server/README.md).

**Step: 4** Install Nvidia drivers on worker nodes (`vm1` to `vm5`) using [Nvidia-instructions](https://github.com/MU-Data-Science/GAF/tree/main/GPU-Variant-Calling/config/setup).  
Note: Please make sure nvidia drivers are correctly installed. You can check using `nvidia-smi` command on all the workers nodes.  

**Step: 5** Attach `NVME storage` at `vm1` and share using `NFS` from `VM2 to VM5` using [NVME-instructions](https://github.com/MU-Data-Science/GAF/tree/main/GPU-Variant-Calling/config/NVME_setup.MD).  

**Step: 6** Utilize the python notebook for prediction and testing task, [Notebooks](https://github.com/MU-Data-Science/GAF/tree/main/GPU-Variant-Calling/notebooks).  

**Step: 7** Execute the `script.sh` from `vm0` $HOME directory like,  
```
./script.sh number_subsets number_worker_vms number_subsets algorithm input_filename
```
Example: 
```
./script.sh 10 5 10 1 input.csv
```
```
./script.sh 10 5 10 2 input_S2.csv
```
