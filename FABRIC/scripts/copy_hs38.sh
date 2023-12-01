#!/usr/bin/env bash
if [[ $# -lt 1 ]]; then
    echo "Usage: copy_hs38 <num_nodes>"
    exit
fi

num_nodes=${1}
for ((i=0 ; i < $num_nodes ; i++))
do
  ssh vm${i} cp -R /proj/eva-public-PG0/Genome_Data/hs38.* /mydata
  echo "Finished copying hs38 files"
  ssh vm${i} cp -R /proj/eva-public-PG0/Homo_sapiens_assembly38.* /mydata
  echo "Finished copying Homo_sapiens_assembly38 files"
  echo "Finished copying to vm"${i}
done
