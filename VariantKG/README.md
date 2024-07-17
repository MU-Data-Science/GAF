# VariantKG
A Scalable Tool for Analyzing Genomic Variants of Humans Using Knowledge Graphs and Machine Learning

# Table of Contents

[Installing SnpEff](#installing-snpeff)

[Installing VCF2RDF](#installing-vcf2rdf)

[Running VariantKG](#run-variantKG)

# Installing SnpEff
The tool is already packaged with the latest version (as of July 17, 2024) of SnpEff. If you wish to upgrade or face an error with the GRCh version being unavailable, use the following steps.

Step 1: Go to home directory, '/mydata' in cloudlab and download the latest version.

    $ cd /mydata
    $ wget https://snpeff.blob.core.windows.net/versions/snpEff_latest_core.zip
    $ unzip snpEff_latest_core.zip

Step 2: Download SnpEff databases

    $ java -jar snpEff.jar download GRCh38.105

GRCh38.105 is the latest as of Dec 7, 2022.

To view all available databases: 

    $ java -jar snpEff.jar databases

To view all versions of GRCh38:

    $ java -jar snpEff.jar databases | grep GRCh38

# Installing VCF2RDF

The tool is already packaged with the latest version (as of July 17, 2024) of vcf2rdf. If you wish to upgrade or face an error with the vcf2rdf tool, use the following steps.

Step 1: Install the required prerequisites.

    $ sudo apt-get update
    $ sudo apt-get install autoconf automake gcc make pkg-config zlib1g-dev guile-2.2 guile-2.2-dev gnutls-bin libraptor2-dev libhts-dev texlive curl libxml2-dev gnutls-dev -y

Step 2: Download the source code.

    $ curl -LO https://github.com/UMCUGenetics/sparqling-genomics/releases/download/0.99.11/sparqling-genomics-0.99.11.tar.gz
    $ tar zxvf sparqling-genomics-0.99.11.tar.gz

Step 4: Build the Sparqling-genomics tool.

    $ cd sparqling-genomics-0.99.11
    $ ./configure GUILD=/usr/bin/guild

Use the 'locate' command to find the path for the GUILD package.

Step 4: Once the configurations and installations are complete, make the following changes.

    $ cd tools/common/include
    $ vim helper.h

In the include packages section, add the following packages in this order:

    #include <gnutls/gnutls.h>
    #include <gnutls/crypto.h>

Repeat these steps for bam2rdf/src/main.c, json2rdf/src/main.c, table2rdf/src/main.c, vcf2rdf/src/main.c and xml2rdf/src/main.c tools.

Step 5: Run the following commands:

    $ make

You are now ready to process VCFs and generate knowledge graphs by following the execution instructions in the [Processing-VCF-README.md](/NSF-RAPID-KGV/Processing-VCF/README.md)!

<b>NOTE:</b> The given instructions have been tested in an environment with Python version 3.7.3. If using any other Python versions, you may encounter version mismatch issues for other packages being used.

# Running VariantKG

To run our tool, VariantKG, we recommend using Cloudlab. Once you have cloned the repository, navigate to VariantKG/codebase and run:

    python3 gradio_app.py

Open your browser (Safari or Chrome), and type the following:

    127.1.1.1:7865

If your system's default setting is in dark mode and you prefer a light mode for the tool, use the following URL:

    127.1.1.1:7865/?__theme=light
