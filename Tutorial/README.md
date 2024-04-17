# HeDAI 2024 Tutorial
## Advanced Cyberinfrastructure for Large-Scale Health Data Analysis

Here are the [tutorial slides](HeDAI-2024-Praveen-Rao-tutorial.pdf) presented at the [HeDAI 2024](https://sites.google.com/view/hedai2024) Workshop (co-located with [EDBT/ICDT 2024](https://dastlab.github.io/edbticdt2024/)).

## Setup
1. Create a slice/experiment on FABRIC by using the SliceBuilder; select Ubuntu 20.04.
2. SSH to the slice.
3. `$ git clone https://github.com/MU-Data-Science/GAF.git`
4. `$ ${HOME}/GAF/Tutorial/scripts/docker_installation.sh`
5. For the genomics use case
   1. `$ ${HOME}/GAF/Tutorial/scripts/setup-GPUs-genomics.sh` 
6. For the pathology use case
   1. `$ ${HOME}/GAF/Tutorial/scripts/setup-GPUs-WSI.sh`
