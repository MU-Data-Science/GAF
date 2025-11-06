## Downloading metadata of SRA samples & calculating execution time (_in seconds_) of variant calling from fastq files 
### This workflow works in four major phases:
## Phase: 1 Downloading Metadata for ERR/SRR samples,
Step: 1 After clonning this GitHub repository using,
`git clone https://github.com/raopr/GPU-Variant-Calling.git`  
Run the configure file,  
   `bash configure.sh`  
   Explanation: It would download the human genome hg38 reference genome and index the files in pwd. You should be having six hs38 reference index file. 
   Note: Make sure your conda environment is active, otherwise do, `conda activate myenv`  
Prequisite for step 2,  (Esearch installation)   
`sh -c "$(curl -fsSL https://ftp.ncbi.nlm.nih.gov/entrez/entrezdirect/install-edirect.sh)"`  
`export PATH=${HOME}/edirect:${PATH}`  
Step: 2 Run the command to download metadata,  
   `bash download_metadata.sh`   
   Note: You need to keep the SRR or ERR sample names in the array mentioned in _download_metadata.sh_ file  
   Keep the samples in this script, can't be passed in a file containing sample id's.  
   Your metadata would be downloaded then and saved as `metadata.csv`  
   
## Phase: 2 Running parabricks and save execution time,  
Step: 1 Keep the paired zipped fastq files in `./data` folder.  
Step: 2 Keep all the sample names in `sample_names.txt` file.    
Step: 3 Run the parabricks using,  
   `bash parabricks.sh`  
   Explanation: It runs GPU based parabricks variant calling pipeline and keeps the output in output folder.  
Step: 4 Explore `./results/execution_times.csv` to see the execution time.    
   Note: results directory will also contain all the processed vcf files.  
   
## Phase: 3 Performing FastQC/MultiQC analysis and Merging features with execution time/metadata,
Step: 1 Execute the command,  
`bash multiqc_gen.sh`  
Explanation: It will perform data processing on multiqc analysis and merge with execution time and, metadata obtained. 
Here, In this step, a single file carrying multiqc features, metadata features with execution time as label is generated in the `results` directory.  
Step: 2 Explore the `./results` directory to have a look on multiqc generated report.  

## Phase: 4 Comparative analysis of Machine Learning methods,  
Step: 1 Verify if, `./results/Processed_data.csv` generated in results directory. Then Execute,  
`python3 models.py`  
Step: 2 Explore, `./results/ML_results.csv` stored in results directory.  

You can now sit and digest the results.  


## Machine Details:  
For fabric testbed is attached below: 
Parabricks is functioning optimally with the following system configurations:  
Parabricks Version: 4.0.1-1  
Nvidia Driver Version: 525.60.13  
CUDA Version: 12.0  
GPU: 2x Tesla T4  
CPU: 64 cores  
RAM: 364GB  
HDD: 1000GB  
These configurations have ensured smooth and efficient operation of our tasks.   
The RAM seems pretty high (I requested for 64 but since fabric allocated upper available limit thus they allotted 364GB RAM).  
For Reference visit https://learn.fabric-testbed.net/knowledge-base/how-vms-are-sized-in-fabric/.
