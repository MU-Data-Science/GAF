{
# Create a CSV file for the execution times
echo "Sample,RealTime" > results/execution_times.csv

# Read each sample name from the file
while IFS= read -r sample
do
    # Echo the sample name
    echo "Processing sample: $sample"

    # Define the paths for the input and output files
    file1="./data/${sample}_1.fastq.gz"
    file2="./data/${sample}_2.fastq.gz"
    output_bam="./results/${sample}.bam"
    output_vcf="./results/${sample}.vcf"

    # Capture the start time
    start_time=$(date +%s)

    # Run the Docker command for each sample
    sudo docker run --gpus 1,2 --rm --volume "$(pwd)":/workdir --volume "$(pwd)/results":/outputdir nvcr.io/nvidia/clara/clara-parabricks:4.0.1-1 pbrun germline --ref /workdir/hs38.fa --in-fq /workdir/"$file1" /workdir/"$file2" --out-bam "/outputdir/${sample}.bam" --out-variants "/outputdir/${sample}.vcf" --verbose

    # Capture the end time
    end_time=$(date +%s)

    # Calculate the execution time
    execution_time=$((end_time - start_time))

    # Append the execution time to the CSV file
    echo "$sample,$execution_time" >> results/execution_times.csv

    # Delete all files in output directory except .vcf and .csv files
    find results -type f -not \( -name "*.vcf" -o -name "*.csv" \) -delete

done < sample_names.txt
} 2>&1 | tee ./logs/parabricks.logs
