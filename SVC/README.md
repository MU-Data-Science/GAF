The `gaf_crypto` application is the core DPDK application for secure variant calling (SVC). It is inspired by the `l2fwd-crypto` application in DPDK.

# FABRIC Setup
1. Set up a slice on [FABRIC](https://portal.fabric-testbed.net/) with Ubuntu 22.04 and ConnectX-6 SmartNIC using SliceBuilder.
2. Login to the slice using SSH.

# SVC Installation Steps

1. Download DPDK (tested version: 23.03.0)
```
$ cd ${HOME}
$ wget https://github.com/DPDK/dpdk/archive/refs/tags/v23.03.tar.gz
$ tar xvfz v23.03.tar.gz
$ ln -sf dpdk-23.03 dpdk 
```
Alternative URL for downloading: `http://fast.dpdk.org/rel/dpdk-23.03.tar.gz`

2. Clone the GAF project
```
$ git clone https://github.com/MU-Data-Science/GAF.git
```
3. Copy `gaf_crypto` code to DPDK directory
```
cp -r GAF/SVC/gaf_crypto dpdk/examples/
```
4. Edit the `dpdk/examples/meson.build` file to add `gaf_crypto` to `all_examples` 
```
all_examples = [
    'gaf_crypto',
    'bbdev_app',
    ...
]    
```
5. Copy required installation scripts
```
$ cp ${HOME}/dpdk/examples/gaf_crypto/setup/*.sh ${HOME}/
```
6. Run the installation scripts to install the drivers, build DPDK apps, and install the required genome tools.
```
$ ${HOME}/install_mlx_dpdk.sh
$ ${HOME}/install_genome_tools.sh
```
6. Copy the reference genome and sample genome sequences to the FABRIC slice in ${HOME}.
   - Download the reference genome tarball to a local machine from [here](https://mailmissouri-my.sharepoint.com/:u:/g/personal/raopr_umsystem_edu/EcAAl7ea7kJGiVzbMuI5XicBOq2jEXeLN38rL8NSqii5HQ?e=8njtmq). Use `scp` to copy to the FABRIC slice under `${HOME}` and untar the file.
   - On the FABRIC slice, download a (paired-end) genome sequence using `wget`:

      `wget ftp://ftp.sra.ebi.ac.uk/vol1/fastq/ERR062/ERR062934/ERR062934_1.fastq.gz`

      `wget ftp://ftp.sra.ebi.ac.uk/vol1/fastq/ERR062/ERR062934/ERR062934_2.fastq.gz`

7. Here are the instructions for running SVC: [README.md](gaf_crypto/README.md)

# References
1. https://www.dpdk.org/