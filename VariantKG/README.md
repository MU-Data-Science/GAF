# VariantKG
A Scalable Tool for Analyzing Genomic Variants of Humans Using Knowledge Graphs and Machine Learning

# Table of Contents

[Setting up Conda](#conda-setup)

[Installing SnpEff](#installing-snpeff)

[Installing VCF2RDF](#installing-vcf2rdf)

[Setting up Blazegraph](#installing-blazegraph)

[Running VariantKG](#run-variantKG)

# Setting up Conda

Step 1: Set up Conda environment

    $ cd /mydata && wget https://repo.anaconda.com/archive/Anaconda3-2022.05-Linux-x86_64.sh
    $ bash Anaconda3-2022.05-Linux-x86_64.sh -b -p /mydata/anaconda3
    $ export PATH=/mydata/anaconda3/bin:$PATH
    $ echo 'export PATH=/mydata/anaconda3/bin:$PATH' >> ~/.profile && . ~/.profile
    $ conda init && exit

Step 2: SSH back into the experiment.

Step 3: Install the required packages:

    $ cd /mydata && conda create -n dglconda
    $ conda activate dglconda
    $ conda install -c dglteam dgl -y 
    $ sudo apt-get update
    $ sudo apt install openjdk-11-jre-headless -y
    $ sudo apt install openjdk-11-jre-headless gradle -y

Step 4: Clone the repository and navigate to the VariantKG folder.

# Installing SnpEff
To download the latest version of SnpEff and GRCh38.86, use the following steps after cloning the repository:

Step 1: Download the latest version.

    $ cd /mydata/GAF/VariantKG/codebase
    $ wget https://snpeff.blob.core.windows.net/versions/snpEff_latest_core.zip
    $ unzip snpEff_latest_core.zip
    
Note: Make sure you have compatible java version for SnpEff, on oct10th2024 Java 21 (updated JRE included) version works good.
Commands to install:  
 `$ sudo apt update`  
 `$ sudo apt upgrade -y`  
 `$ apt search openjdk`  
 `$ sudo apt install openjdk-21-jdk-headless -y`  

Step 2: Download SnpEff databases  

    $ cd /mydata/GAF/VariantKG/codebase/snpEff
    $ java -jar snpEff.jar download GRCh38.86

GRCh38.86 is the latest as of July 17, 2024.

To view all available databases: 

    $ java -jar snpEff.jar databases

To view all versions of GRCh38:

    $ java -jar snpEff.jar databases | grep GRCh38

# Installing VCF2RDF

To install vcf2rdf, use the following steps:

Step 1: Install the required prerequisites.

    $ cd /mydata/GAF/VariantKG/codebase
    $ sudo apt-get update
    $ sudo apt-get install autoconf automake gcc make pkg-config zlib1g-dev guile-2.2 guile-2.2-dev gnutls-bin libraptor2-dev libhts-dev texlive curl libxml2-dev gnutls-dev -y

Step 2: Download the source code.

    $ curl -LO https://github.com/UMCUGenetics/sparqling-genomics/releases/download/0.99.11/sparqling-genomics-0.99.11.tar.gz
    $ tar zxvf sparqling-genomics-0.99.11.tar.gz

Step 4: Build the Sparqling-genomics tool.

    $ cd /mydata/GAF/VariantKG/codebase/sparqling-genomics-0.99.11
    $ ./configure GUILD=/usr/bin/guild

Use the 'locate' command to find the path for the GUILD package.

Step 4: Once the configurations and installations are complete, make the following changes.

    $ cd tools/common/include
    $ vim helper.h

In the include packages section, add the following packages in this order:

    #include <gnutls/gnutls.h>
    #include <gnutls/crypto.h>

Repeat these steps for ../tools/bam2rdf/src/main.c, ../tools/json2rdf/src/main.c, ../tools/table2rdf/src/main.c, ../tools/vcf2rdf/src/main.c and ../tools/xml2rdf/src/main.c tools.

Step 5: Run the following commands:

    $ cd /mydata/GAF/VariantKG/codebase/sparqling-genomics-0.99.11/tools
    $ make

<b>NOTE:</b> The given instructions have been tested in an environment with Python version 3.7.3. If using any other Python versions, you may encounter version mismatch issues for other packages being used.

# Setting up Blazegraph

To setup Blazegraph for graph storage and querying, use the following steps:

Download the jar file:

    $ cd /mydata/GAF/VariantKG/codebase
    $ wget https://github.com/blazegraph/database/releases/download/BLAZEGRAPH_2_1_6_RC/blazegraph.jar

# Running VariantKG

To run our tool, VariantKG, we recommend using Cloudlab. Once you have cloned the repository, please install SnpEff, vcf2rdf and Blazegraph and then navigate to VariantKG/codebase and run:

    $ cd /mydata/GAF/VariantKG/codebase
    $ pip3 install -r requirements.txt
    $ sudo apt-get install gcc-4.9
    $ sudo apt-get install --only-upgrade libstdc++6 -y
    $ pip3 install torch==2.2.0
    $ pip3 install pymantic
    $ pip3 install torch-geometric
    $ python3 gradio_app.py

Open your browser (Safari or Chrome), and type the following:

    http://127.0.0.1:7865

If your system's default setting is in dark mode and you prefer a light mode for the tool, use the following URL:

    http://127.0.0.1:7865/?__theme=light
