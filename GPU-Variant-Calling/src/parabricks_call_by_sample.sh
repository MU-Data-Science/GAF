#!/bin/bash
{
# Create a CSV file for the execution times
#echo "Sample,RealTime" > results/execution_times.csv

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

  # Define the paths for the input and output files
  file1="${arg_value}_1.fastq.gz"
  file2="${arg_value}_2.fastq.gz"
  output_bam="./results/${arg_value}.bam"
  output_vcf="./results/${arg_value}.vcf"

  # Capture the start time
  start_time=$(date +%s)

  # Run the Docker command for each sample
  sudo docker run --gpus all --rm  --volume "/mydata/GPU-Variant-Calling/":/refdir --volume "/mnt/nvme":/inputdir  --volume "/mydata/GPU-Variant-Calling/results":/outputdir nvcr.io/nvidia/clara/clara-parabricks:4.1.0-1 pbrun germline --ref "/refdir/hs38.fa" --in-fq "/inputdir/$file1" "/inputdir/$file2" --out-bam "/outputdir/${arg_value}.bam" --out-variants "/outputdir/${arg_value}.vcf" --low-memory --verbose
  

  # Capture the end time
  end_time=$(date +%s)

  # Calculate the execution time
  execution_time=$((end_time - start_time))

  # Append the execution time to the CSV file
  echo "$arg_value,$execution_time" >> results/execution_times.csv

  # Delete all files in output directory except .vcf and .csv files
  #find results -type f -not \( -name "*.vcf" -o -name "*.csv" \) -delete
else
  echo "Please provide the sample names using the -s flag"
fi

} 2>&1 | tee ./logs/parabricks_$(hostname).logs
