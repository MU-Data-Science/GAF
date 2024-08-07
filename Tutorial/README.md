# HeDAI 2024 Tutorial
## Advanced Cyberinfrastructure for Large-Scale Health Data Analysis

Here are the [tutorial slides](https://drive.google.com/file/d/1wiAZHboUWxXR7gBSBpsG1Zw9q48yjXDN/view?usp=sharing) presented at the [HeDAI 2024](https://sites.google.com/view/hedai2024) Workshop (co-located with [EDBT/ICDT 2024](https://dastlab.github.io/edbticdt2024/)).

## Setup
1. Create a slice/experiment on FABRIC by using the SliceBuilder; select Ubuntu 20.04.
2. SSH to the slice.
3. `$ git clone https://github.com/MU-Data-Science/GAF.git`
4. For the genomics use case
   1. `$ ${HOME}/GAF/Tutorial/scripts/setup-GPUs-genomics.sh` 
5. For the pathology use case
   1. `$ ${HOME}/GAF/Tutorial/scripts/setup-GPUs-WSI.sh`

## Additional instructions
1. Download the reference genome files from [here](https://mailmissouri-my.sharepoint.com/:u:/g/personal/raopr_umsystem_edu/EcAAl7ea7kJGiVzbMuI5XicBOq2jEXeLN38rL8NSqii5HQ?e=8njtmq).
2. To download a (paired-end) genome sequence using wget:

   `wget ftp://ftp.sra.ebi.ac.uk/vol1/fastq/ERR016/ERR016314/ERR016314_1.fastq.gz`

   `wget ftp://ftp.sra.ebi.ac.uk/vol1/fastq/ERR016/ERR016314/ERR016314_2.fastq.gz`
3. Parabricks command line:
   `sudo docker run --gpus 1 --volume $(pwd):/workdir --volume $(pwd):/outputdir nvcr.io/nvidia/clara/clara-parabricks:4.2.1-1 pbrun germline --ref /workdir/hs38.fa --in-fq /workdir/ERR016314_1.fastq.gz /workdir/ERR016314_2.fastq.gz --out-bam /outputdir/ERR016314.bam --out-variants /outputdir/ERR016314.vcf --low-memory`
