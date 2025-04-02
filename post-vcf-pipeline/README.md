# Post-VCF analysis workflow

## This workflow performs vcf-annotation, phylogeny generation, and gene-variant analysis for [AVAH](https://github.com/raopr/AVAH/tree/master) generated vcf files.

![image](https://github.com/raopr/NSF-CC-GAF/blob/main/post-vcf-pipeline/workflow.png)



## Pre-requisites:
1. Use https://conda.io/projects/conda/en/latest/user-guide/install/index.html link to install conda (_this is where you will be installing the packages_)
2. Install pip using https://pip.pypa.io/en/stable/installation/.
3. Used python3.10 for this project (_available in conda_env.yaml file_).

## Instructions:
Step: 1 Clone repository using,  
        `git clone https://github.com/raopr/NSF-CC-GAF`

Step: 2 Create a conda virtual environment using,   
        `conda env create -f conda_env.yaml -n myconda_env`  
        Explanation-  
        This command creates a virtual conda env using the provided conda_env.yaml file and you can always change the name of your conda environment using the -n flag. 

Step: 3 Activate conda environment,  
        `conda activate myconda_env`

Step: 4 Configure using configure script,  
        `bash configure.sh`

Step: 5 Keep your vcf files in the data folder or use the already provided 10-demo vcf files for analysis.  
        `/data/ ` 

Note: Before running make sure you have `java -version` java version 13 or above. 
If you dont have 13 or above, use [documentation](https://askubuntu.com/questions/1202075/how-to-install-java-13-on-ubuntu-18-04-and-newer) for installation.

Step: 6 Run **script.py** file using shell,  
        `bash setup.sh`

Step: 7 Results will be stored in the results folder.  
        `/results`

Step: 8 To deactivate `conda deactivate`
