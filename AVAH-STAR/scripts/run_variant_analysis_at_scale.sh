#!/usr/bin/env bash

CANNOLI_HOME_DIR="/mydata/cannoli"
ADAM_HOME_DIR="/mydata/adam"
SPARK_HOME_DIR="/mydata/spark"
HADOOP_HOME_DIR="/mydata/hadoop"
HOMEBREW_DIR="/home/linuxbrew/.linuxbrew"
HDFS_PREFIX="hdfs://vm0:9000"
LOCAL_PREFIX="file:/"
MASTER_URL="yarn --deploy-mode client"
DATE=$(date "+%Y-%m-%d-%s")
LOGFILE="/mydata/${USER}-avah-${DATE}.log"
EVA_JAR=${HOME}"/AVAH-FABRIC/lib/avah_3-0-0_2.12-0.1.jar"
DEFAULT_REFERENCE="hs38"
EVA_HOME=${HOME}"/EVA"
BWA_HOME="/mydata/bwa"
BWAMEM2_HOME="/mydata/bwa-mem2"
FREEBAYES_HOME="/mydata/freebayes"
DATA_DIR="/mydata"
GATK_HOME="/mydata/gatk-4.1.8.0"
KNOWN_SNPS="/mydata/Homo_sapiens_assembly38.dbsnp138.vcf.gz"
KNOWN_INDELS="/mydata/Homo_sapiens_assembly38.known_indels.vcf.gz"

usage()
{
  echo "Usage: run_variant_analysis_at_scale [ -h ] [ -i <file1> ] [ -d <file2> ] [ -n <num_nodes> ] "
  echo "                                     [ -b <batch_size> ] [ -p <num_partitions> ] [ -r <reference> ] "
  echo "                                     [ -s ] [ -f ] [ -P <type> ] [ -B ] [ -e ] [ -G ] [ -g ] [-b <num_buckets> ] [ -F ]"
  echo ""
  echo "Required arguments:"
  echo "<file1>         - file containing sample IDs (e.g., SRR077487), one per line"
  echo "<file2> or NONE - file containing URLs of FASTQ files to download (one per line)"
  echo "                  NONE means don't download any FASTQ files"
  echo "<num_nodes>     - number of cluster nodes"
  echo "<batch_size>    - batch size for outstanding futures"
  echo "<num_partitions> - number of partitions (of sequences IDs) to create"
  echo ""
  echo "Optional:"
  echo " -r <reference>  - reference genome [default: hs38]"
  echo " -s              - naive, process one sequence at-a-time"
  echo " -f              - use AVHAx (or fork-join approach)"
  echo " -P <R|H|D|S>    - [R]ange or [H]ash or [D]efault or [S]orted default [default: D]"
  echo " -B              - enable BQSR and INDEL realignment"
  echo " -e              - enable early retry of failed sequences"
  echo " -G              - use GATK pipeline (default: ADAM-Cannoli)"
  echo " -g              - use GPUs when available (default: use CPUs only)"
  echo " -m              - no. of buckets to hash RDD partition IDs for GPU execution"
  echo "                                          (1 -> all RDD partitions will use GPUs),"
  echo "                                          (2 -> every other partition will use GPUs), ... "
  echo " -F              - first-come, first-served approach for using GPUs in an RDD partition (default: entire RDD partition uses GPUs)"
  exit 1
}

REF_GENOME="hs38"
BATCH_SIZE=0
NUM_NODES=0
NUM_PARTITIONS=0
ID_FILE=""
URL_FILE=""
PARTITION_TYPE="D"
NAIVE=0
FORK_JOIN=0
BQSR_INDEL=0
EARLY_RETRY=0
USE_GATK=0
USE_GPU=0
NUM_BUCKETS=0
USE_FCFS=0

while getopts 'sfhBeGgFm:i:d:n:b:p:r:P:' value
do
  case $value in
    b) BATCH_SIZE=$OPTARG ;;
    r) REF_GENOME=$OPTARG ;;
    n) NUM_NODES=$OPTARG ;;
    i) ID_FILE=$OPTARG ;;
    d) URL_FILE=$OPTARG ;;
    p) NUM_PARTITIONS=$OPTARG ;;
    P) PARTITION_TYPE=$OPTARG ;;
    s) NAIVE=1 ;;
    f) FORK_JOIN=1 ;;
    B) BQSR_INDEL=1 ;;
    e) EARLY_RETRY=1 ;;
    G) USE_GATK=1 ;;
    g) USE_GPU=1 ;;
    F) USE_FCFS=1 ;;
    m) NUM_BUCKETS=$OPTARG ;;
    h|?) usage ;;
  esac
done

if [[ ${BATCH_SIZE} -eq 0 || ${NUM_NODES} -eq 0 || ${NUM_PARTITIONS} -eq 0 ]]; then
    usage
fi

let NUM_EXECUTORS=${NUM_NODES}-1

# Delete *.ifq, *.bam*, *.vcf* files, *.retry files
$HADOOP_HOME/bin/hdfs dfs -rm -r -skipTrash /*.ifq /*.bam* /*.vcf* /*.retry

# Delete any temporary files from previous execution
for ((i=0; i<${NUM_NODES}; i++))
do
  RM_CMD="'rm -r ${DATA_DIR}/*bam* ${DATA_DIR}/*fastq.gz* ${DATA_DIR}/tmp_*'"
  ssh vm${i} bash -c ${RM_CMD}
done

# network timeout for shuffle
TIMEOUT="420s"

# max attempts
MAX_ATTEMPTS=3

# command
COMMAND="W"

# Spark conf
SPARK_CONF="--conf spark.yarn.appMasterEnv.CANNOLI_HOME=${CANNOLI_HOME_DIR}
        --conf spark.yarn.appMasterEnv.ADAM_HOME=${ADAM_HOME_DIR}
        --conf spark.yarn.appMasterEnv.SPARK_HOME=${SPARK_HOME_DIR}
        --conf spark.yarn.appMasterEnv.HADOOP_HOME=${HADOOP_HOME_DIR}
        --conf spark.yarn.appMasterEnv.HOMEBREW_PREFIX=${HOMEBREW_DIR}
        --conf spark.yarn.appMasterEnv.EVA_HOME=${EVA_HOME}
        --conf spark.yarn.appMasterEnv.BWA_HOME=${BWA_HOME}
        --conf spark.yarn.appMasterEnv.BWAMEM2_HOME=${BWAMEM2_HOME}
        --conf spark.yarn.appMasterEnv.FREEBAYES_HOME=${FREEBAYES_HOME}
        --conf spark.yarn.appMasterEnv.DATA_DIR=${DATA_DIR}
        --conf spark.yarn.appMasterEnv.GATK_HOME=${GATK_HOME}
        --conf spark.executorEnv.CANNOLI_HOME=${CANNOLI_HOME_DIR}
        --conf spark.executorEnv.ADAM_HOME=${ADAM_HOME_DIR}
        --conf spark.executorEnv.SPARK_HOME=${SPARK_HOME_DIR}
        --conf spark.executorEnv.HADOOP_HOME=${HADOOP_HOME_DIR}
        --conf spark.executorEnv.HOMEBREW_PREFIX=${HOMEBREW_DIR}
        --conf spark.executorEnv.EVA_HOME=${EVA_HOME}
        --conf spark.executorEnv.BWA_HOME=${BWA_HOME}
        --conf spark.executorEnv.BWAMEM2_HOME=${BWAMEM2_HOME}
        --conf spark.executorEnv.FREEBAYES_HOME=${FREEBAYES_HOME}
        --conf spark.executorEnv.DATA_DIR=${DATA_DIR}
        --conf spark.executorEnv.GATK_HOME=${GATK_HOME}
        --conf spark.executorEnv.KNOWN_SNPS=${KNOWN_SNPS}
        --conf spark.executorEnv.KNOWN_INDELS=${KNOWN_INDELS}
        --conf spark.network.timeout=${TIMEOUT}
        --conf spark.yarn.maxAppAttempts=${MAX_ATTEMPTS} "

EXTRA_ARGS=""

if [[ ${NAIVE} -eq 1 ]]; then
    EXTRA_ARGS="-s"
fi

if [[ ${FORK_JOIN} -eq 1 ]]; then
    EXTRA_ARGS=${EXTRA_ARGS}" -f"
fi

if [[ ${BQSR_INDEL} -eq 1 ]]; then
    EXTRA_ARGS=${EXTRA_ARGS}" -B"
fi

if [[ ${EARLY_RETRY} -eq 1 ]]; then
    EXTRA_ARGS=${EXTRA_ARGS}" -e"
fi

if [[ ${USE_GATK} -eq 1 ]]; then
    EXTRA_ARGS=${EXTRA_ARGS}" -G"
fi

if [[ ${USE_GPU} -eq 1 ]]; then
    EXTRA_ARGS=${EXTRA_ARGS}" -g"
fi

if [[ ${USE_FCFS} -eq 1 ]]; then
    EXTRA_ARGS=${EXTRA_ARGS}" -F"
fi

if [[ ${NUM_BUCKETS} -gt 0 ]]; then
    EXTRA_ARGS=${EXTRA_ARGS}" -m "${NUM_BUCKETS}
fi

echo ${SPARK_CONF}
echo ${EXTRA_ARGS}

scala_version=$(spark-submit --version |& grep 'Using Scala version')
if [[ ${scala_version} == *"Using Scala version 2.11"* ]]; then
    EVA_JAR=${HOME}"/AVAH-FABRIC/lib/avah_2-4-7_2.11-0.1.jar"
fi

echo "Using "${EVA_JAR}

if [[ ${EXTRA_ARGS} == "" ]]; then
    ${SPARK_HOME}/bin/spark-submit --master ${MASTER_URL} --num-executors ${NUM_EXECUTORS} \
            ${SPARK_CONF} \
            ${EVA_JAR} -i ${LOCAL_PREFIX}/${ID_FILE} -d ${LOCAL_PREFIX}/${URL_FILE} -c ${COMMAND} -r ${REF_GENOME} \
            -n ${NUM_NODES} -b ${BATCH_SIZE} -p ${NUM_PARTITIONS} -P ${PARTITION_TYPE} &> ${LOGFILE} &
else
    ${SPARK_HOME}/bin/spark-submit --master ${MASTER_URL} --num-executors ${NUM_EXECUTORS} \
            ${SPARK_CONF} \
            ${EVA_JAR} -i ${LOCAL_PREFIX}/${ID_FILE} -d ${LOCAL_PREFIX}/${URL_FILE} -c ${COMMAND} -r ${REF_GENOME} \
            -n ${NUM_NODES} -b ${BATCH_SIZE} -p ${NUM_PARTITIONS} -P ${PARTITION_TYPE} ${EXTRA_ARGS} &> ${LOGFILE} &
fi

echo "See log file for progress: "${LOGFILE}
