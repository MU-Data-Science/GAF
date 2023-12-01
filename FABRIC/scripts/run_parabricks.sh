#!/bin/bash

if [[ $# -lt 7 ]]; then
  echo "Usage: run_parabricks.sh <cmd> <timeout> <vol1> <vol2> <ref> <file1> <file2> <file3>"
  echo ""
  exit
fi

cmd=${1}
timeout=${2}
waitTime=30
let num_cpus=$(nproc)/4

if [[ ${timeout} -lt 0 ]]; then
  exec 123</mydata/gpu.free || exit 1
  flock -w ${waitTime} 123 || exit 1
else
  while true
  do
    echo "WAITING_FOR_LOCK: "${cmd}"-"${6}"-"${7};
    exec 123</mydata/gpu.free || { sleep ${timeout}; continue; }
    flock -w ${waitTime} 123 || { continue; }
    echo "FLOCK_status: "$?" "${cmd}"-"${6}"-"${7}
    break
  done
fi
echo "Acquired lock for "${cmd}"-"${6}"-"${7}

PARABRICKS_DOCKER_IMG="nvcr.io/nvidia/clara/clara-parabricks:4.0.0-1"

dirs=($(echo ${3} | tr ":" "\n")); DATADIR=${dirs[0]}; WORKDIR=${dirs[1]}
dirs=($(echo ${4} | tr ":" "\n")); OUTPUTDIR=${dirs[1]}
HDFS_PREFIX="hdfs://vm0:9000"
HADOOP_HOME=${DATADIR}"/hadoop"

if [[ ${cmd} == "fq2bam" ]];
then
  sudo docker run --cpus=${num_cpus} --gpus all \
    --volume ${3} --volume ${4} \
    ${PARABRICKS_DOCKER_IMG} pbrun ${cmd} \
    --ref ${WORKDIR}/${5} \
    --in-fq ${WORKDIR}/${6} ${WORKDIR}/${7} \
    --out-bam ${OUTPUTDIR}/${8}
elif [[ ${cmd} == "haplotypecaller" ]];
then
  ${HADOOP_HOME}"/bin/hdfs" dfs -get ${HDFS_PREFIX}/${6} ${DATADIR}
  sudo docker run --cpus=${num_cpus} --gpus all \
    --volume ${3} --volume ${4} \
    ${PARABRICKS_DOCKER_IMG} pbrun ${cmd}  \
    --ref ${WORKDIR}/${5} \
    --in-bam ${WORKDIR}/${6} \
    --out-variants ${OUTPUTDIR}/${7}
else
  echo "Unsupported command: "${1}
fi