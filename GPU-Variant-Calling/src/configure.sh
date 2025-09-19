pip install multiqc
conda install bioconda::fastqc -y
sudo apt install ncbi-entrez-direct
#human genome download and indexing using bwa
sudo wget https://storage.googleapis.com/genomics-public-data/references/hg38/v0/Homo_sapiens_assembly38.fasta -O hs38.fa
sudo apt install bwa
#pip packages
pip install pandas
pip install -U scikit-learn
pip install xgboost
pip install matplotlib
pip install seaborn
#long step - indexing ref genome
sudo bwa index hs38.fa
