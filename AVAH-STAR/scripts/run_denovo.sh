#!/usr/bin/env bash

CANNOLI_HOME_DIR="/mydata/cannoli"
SPARK_HOME_DIR="/mydata/spark"
HOMEBREW_DIR="/home/linuxbrew/.linuxbrew"
HDFS_PREFIX="hdfs://vm0:9000"
LOCAL_PREFIX="file:/"
MASTER_URL="yarn --deploy-mode client"
DATE=$(date "+%Y-%m-%d-%s")
LOGFILE="/mydata/${USER}-denovo-${DATE}.log"
EVA_JAR=${HOME}"/AVAH/lib/avah_2.12-0.1.jar"
DEFAULT_KMER_LEN=51

if [[ $# -lt 3 ]]; then
    echo "Usage: run_denovo.sh <file1> <file2> <num_nodes> [kmer_len]"
    echo ""
    echo "Required arguments:"
    echo "<file1> - file containing sample IDs (e.g., SRR077487), one per line"
    echo "<file2> or NONE - file containing URLs of FASTQ files to download (one per line)"
    echo "                  NONE means don't download any FASTQ files"
    echo "<num_nodes> - number of nodes in the cluster"
    echo ""
    echo "Optional arguments: "
    echo "[kmer_len] - k-mer length [default: 51]"
    exit
elif [[ $# -eq 3 ]]; then
    KMER_LEN=${DEFAULT_KMER_LEN}
else
    KMER_LEN=${4}
fi

let NUM_EXECUTORS=${3}-1
COMMAND="D"

$SPARK_HOME/bin/spark-submit --master ${MASTER_URL} --num-executors ${NUM_EXECUTORS} \
    --conf spark.yarn.appMasterEnv.CANNOLI_HOME=${CANNOLI_HOME_DIR} \
    --conf spark.yarn.appMasterEnv.SPARK_HOME=${SPARK_HOME_DIR} \
    --conf spark.yarn.appMasterEnv.HOMEBREW_PREFIX=${HOMEBREW_DIR} \
    --conf spark.executorEnv.CANNOLI_HOME=${CANNOLI_HOME_DIR} \
    --conf spark.executorEnv.SPARK_HOME=${SPARK_HOME_DIR} \
    --conf spark.executorEnv.HOMEBREW_PREFIX=${HOMEBREW_DIR} \
    ${EVA_JAR} -i ${LOCAL_PREFIX}/${1} -d ${LOCAL_PREFIX}/${2} -c ${COMMAND} -k ${KMER_LEN} &> ${LOGFILE} &

echo "See log file for progress: "${LOGFILE}