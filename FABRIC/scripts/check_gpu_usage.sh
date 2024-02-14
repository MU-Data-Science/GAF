#!/usr/bin/env bash

if [[ $# -ne 1 ]]; then
  echo "Usage: check_gpu.sh <interval>"
  echo ""
  exit
fi

DATADIR="/mydata"

while [ true ]
do
  x=$(nvidia-smi -q -d PIDS | grep Processes)
  file_lock=${DATADIR}/gpu.lock
  file_free=${DATADIR}/gpu.free
  if [[ ${x} == *": None" ]]; then
    echo "GPU is idle"
    if [[ -f ${file_lock} ]]; then
      rm -f ${file_lock}
      echo "Removing lock file"
    fi
    if [[ ! -f ${file_free} ]]; then
      touch ${file_free}
      echo "Creating free file"
    fi
  else
    echo "GPU is busy: ${x}"
    if [[ ! -f ${file_lock} ]]; then
      touch ${file_lock}
      echo "Creating lock file"
    fi
    if [[ -f ${file_free} ]]; then
      rm -f ${file_free}
      echo "Removing free file"
    fi
  fi
  sleep ${1}
done