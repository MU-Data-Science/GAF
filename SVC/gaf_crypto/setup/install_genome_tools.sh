#!/usr/bin/env bash

echo "👉 Starting the installation process."

cd ${HOME}

# install Java
sudo DEBIAN_FRONTEND=noninteractive apt install openjdk-11-jre-headless -y

# install BWA
git clone https://github.com/lh3/bwa.git
cd bwa; make
cd ${HOME}

# install Picard
PICARD_VERSION="2.27.4"
wget https://github.com/broadinstitute/picard/releases/download/${PICARD_VERSION}/picard.jar
mkdir picard; mv picard.jar picard

# install Freebayes
FREEBAYES_VERSION="1.3.6"
wget -O freebayes.gz https://github.com/freebayes/freebayes/releases/download/v${FREEBAYES_VERSION}/freebayes-${FREEBAYES_VERSION}-linux-amd64-static.gz
gunzip freebayes.gz
chmod +x freebayes 

# Samtools
SAMTOOLS_VERSION="1.18"
sudo DEBIAN_FRONTEND=noninteractive apt install libbz2-dev -y
sudo DEBIAN_FRONTEND=noninteractive apt install liblzma-dev -y
wget https://github.com/samtools/samtools/releases/download/${SAMTOOLS_VERSION}/samtools-${SAMTOOLS_VERSION}.tar.bz2
bunzip2 samtools-${SAMTOOLS_VERSION}.tar.bz2; tar xvf samtools-${SAMTOOLS_VERSION}.tar 
cd samtools-${SAMTOOLS_VERSION}
./configure --prefix=${HOME}/samtools-${SAMTOOLS_VERSION} --without-curses
make

echo "👉 Completed the installation process."