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
    $ sudo apt install openjdk-17-jre-headless -y
    $ sudo apt install openjdk-17-jre-headless gradle -y

Step 4: Clone the repository and navigate to the VariantKG folder.

# Installing SnpEff
To download the latest version of SnpEff and GRCh38.86, use the following steps after cloning the repository:

Step 1: Download the latest version.

    $ cd /mydata/GAF/VariantKG/codebase
    $ wget https://sourceforge.net/projects/snpeff/files/snpEff_v4_3t_core.zip
    $ unzip snpEff_v4_3t_core.zip
    
Note: This workflow and tool are dependent on the Ubuntu and JDK versions. It has been tested thoroughly with Ubuntu18 and JDK 17. 

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
`$ sudo apt install mlocate` #If locate is not installed, use this command  

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

To run our tool, VariantKG, we recommend using Cloudlab and Visual Studio Code. Once you have cloned the repository, please install SnpEff, vcf2rdf and Blazegraph and then navigate to VariantKG/codebase and run:

    $ cd /mydata/GAF/VariantKG/codebase
    $ pip3 install -r requirements.txt
    $ conda install -c dglteam dgl
    $ sudo apt-get install --only-upgrade libstdc++6 -y
    $ pip3 install torch==2.2.0
    $ pip3 install torchdata==0.7.1
    $ pip3 install pymantic numpy==1.26.4 matplotlib scikit-learn pyarrow
    $ pip3 install torch-geometric
    $ python3 gradio_app.py

Add the port, in this case, 7865, to "Forwarding Ports" in VS Code. Then, open your browser (Safari or Chrome) and type the following:

    http://127.0.0.1:7865

If your system's default setting is in dark mode and you prefer a light mode for the tool, use the following URL:

    http://127.0.0.1:7865/?__theme=light

Please note: VariantKG takes unzipped VCF files as input.

# DEBUGGING

1. If you face an error like `ImportError: libssl.so.3: cannot open shared object file: No such file or directory`, it is because of the torchdata version.
2. If you face an error on the JDK version for SnpEff, it will most likely be because of the version mismatch between Ubuntu and JDK.
3. If you face an error with Gradio version, downgrade the version to 4.39.0 using:

   $ pip3 install gradio==4.39.0
   
