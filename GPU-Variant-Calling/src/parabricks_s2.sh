#!/bin/bash
{
# Create a CSV file for the execution times if it doesn't exist
if [ ! -f results/execution_times.csv ]; then
  echo "Sample,RealTime" > results/execution_times.csv
fi

# Initialize variables
flag_s=false
arg_value=""

# Parse options
while getopts "s:" opt; do
  case $opt in
    s)
      flag_s=true
      arg_value=$OPTARG
      ;;
    \?)
      echo "Invalid option: -$OPTARG" 1>&2
      exit 1
      ;;
  esac
done

if $flag_s; then
  # Echo the sample name
  echo "Processing sample: $arg_value"

  # Capture the start time
  start_time=$(date +%s)

  if [[ "${arg_value}" == *_0 ]]; then
    # Remove _0 from the argument value
    arg_value=${arg_value%_0}
    echo "Updated sample name: $arg_value"

    # Define the paths for the input and output files
    file1="${arg_value}_1.fastq.gz"
    file2="${arg_value}_2.fastq.gz"
    output_bam="./results/${arg_value}.bam"
    output_vcf="./results/${arg_value}.vcf"
    sudo docker run --gpus all --rm \
      --volume "/mydata/GPU-Variant-Calling/vcf_time_predict":/refdir \
      --volume "/mnt/nvme":/inputdir \
      --volume "/mydata/GPU-Variant-Calling/vcf_time_predict/results":/outputdir \
      nvcr.io/nvidia/clara/clara-parabricks:4.1.0-1 pbrun fq2bam \
      --ref "/refdir/hs38.fa" \
      --in-fq "/inputdir/$file1" "/inputdir/$file2" \
      --out-bam "/outputdir/${arg_value}.bam" \
      --low-memory --verbose
    echo "FQ2BAM completed for ${arg_value}"

  elif [[ "${arg_value}" == *_1 ]]; then
    # Remove _1 from the argument value
    arg_value=${arg_value%_1}
    echo "Updated sample name: $arg_value"
    # Define the paths for the input and output files
    file1="${arg_value}_1.fastq.gz"
    file2="${arg_value}_2.fastq.gz"
    output_bam="./results/${arg_value}.bam"
    output_vcf="./results/${arg_value}.vcf"
    sudo docker run --gpus all --rm \
      --volume "/mydata/GPU-Variant-Calling/vcf_time_predict":/refdir \
      --volume "/mnt/nvme":/inputdir \
      --volume "/mydata/GPU-Variant-Calling/vcf_time_predict/results":/outputdir \
      nvcr.io/nvidia/clara/clara-parabricks:4.1.0-1 pbrun haplotypecaller \
      --ref "/refdir/hs38.fa" \
      --in-bam "/outputdir/${arg_value}.bam" \
      --out-variants "/outputdir/${arg_value}.vcf" \
      --verbose
    echo "HaplotypeCaller completed for ${arg_value}"

  else
    echo "Check your argument value! Please"
    exit 1
  fi

  # Capture the end time
  end_time=$(date +%s)

  # Calculate the execution time
  execution_time=$((end_time - start_time))

  # Append the execution time to the CSV file
  echo "$arg_value,$execution_time" >> results/execution_times.csv

  # Delete all files in the output directory except .vcf and .csv files

  find ./results -type f -not \( -name  "*.bam" -name -o "*.vcf" -o -name "*.csv" \) -delete

else
  echo "Please provide the sample names using the -s flag"
fi
} 2>&1 | tee ./logs/parabricks_$(hostname).logs
