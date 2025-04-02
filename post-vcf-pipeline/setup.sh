#!/bin/bash


echo ""Cleaning results and logs folders first...Please wait a bit!""
rm -r -f ./results/annotated_vcf/*
rm -r -f ./results/phylip/*
rm -r -f ./results/snpeff/*
rm -r -f ./results/stats/*
rm -r -f ./results/tree/*
rm -r -f ./logs/*

# Step 1: Annotation - parallelized improved version
# Exit the script as soon as a command fails
set -e
# Define absolute paths
data_dir="./data/"
results_dir="./results/"
logs_dir="./logs/"
tools_dir="./tools"

# Function to annotate a file
annotate() {
    file=$1
    f=$(basename "$file")
    E=${f%%.*}
    echo "Step 1: Annotation - Sample $E running..." | tee -a "$logs_dir/log_annotation.txt"
    # Run the command and check if it was successful
    java -jar "$tools_dir/snpEff/snpEff.jar" -v GRCh38.p14 "$file" > "$results_dir/annotated_vcf/${E}_ann.vcf" && \
    echo "Step 1: Annotation for Sample $E completed successfully!" | tee -a "$logs_dir/log_annotation.txt" || \
    { echo "Step 1: Annotation for Sample $E failed!" | tee -a "$logs_dir/log_annotation.txt"; exit 1; } &
}
for file in "$data_dir"*; do
    annotate "$file"
done
# Wait for all background jobs to finish
wait
mv ./snpEff* ./results/snpeff/


# Define absolute paths
results_dir="./results/annotated_vcf/"
# Step 2: Merging files
for i in "$results_dir"*.vcf; do
    echo "$i"
    bgzip "$i" && tabix "${i}.gz" || { echo "Error processing file $i" >&2; exit 1; }
done
{
ls "$results_dir"*.vcf.gz > "$results_dir/merged.txt"
bcftools merge --force-samples -l "$results_dir/merged.txt" -Oz -o "$results_dir/merged.vcf.gz" || { echo "Error merging files" >&2; exit 1; }
gunzip "$results_dir/merged.vcf.gz" || { echo "Error unzipping merged file" >&2; exit 1; }
# Renaming of headers in merged file
gunzip -f "$results_dir"/*_ann.vcf.gz || { echo "Error unzipping annotated files" >&2; exit 1; }
ls "$results_dir"/*_ann.vcf > "$results_dir/filename.txt"
bcftools reheader -s "$results_dir/filename.txt" "$results_dir/merged.vcf" > "$results_dir/merged_new.vcf" || { echo "Error renaming headers" >&2; exit 1; }
mv "$results_dir/merged_new.vcf" "$results_dir/merged.vcf" || { echo "Error renaming file" >&2; exit 1; }
echo "Step 2: Merging files completed successfully!"
} 2>&1 | tee ./logs/logs_merging_file.txt

#-------------------------------------------------------------------------------------------------------------------------------------------------------#
	#Ignore this part - for developer only - for extension of merge files
#This below comment is for more than 1000 file samples
#Get the number of .vcf.gz files
#num_files=$(ls *.vcf.gz | wc -l)
# Calculate the number of batches
#num_batches=$(( (num_files + 999) / 1000 ))
# Create batches in parallel
#seq "$num_batches" | parallel 'start=$(( ({} - 1) * 1000 + 1 )); end=$(( start + 999 )); ls ./results/annotated_vcf/#*.vcf.gz | sed -n "${start},${end}p" > "./results/annotated_vcf/batch_{}.txt"'
# Process each batch in parallel
#seq "$num_batches" | parallel 'echo "Processing batch {}..."; bcftools merge --force-samples -l "./results/annotated_vcf/#batch_{}.txt" -Oz -o "./results/annotated_vcf/merged_batch_{}.vcf.gz"'
# Create a list of all merged batch files
#ls ./results/annotated_vcf/merged_batch_*.vcf.gz > ./results/annotated_vcf/merged_batches.txt
# Merge all batch merged files into a single file
#bcftools merge --force-samples -l ./results/annotated_vcf/merged_batches.txt -Oz -o ./results/annotated_vcf/#final_merged.vcf.gz
#echo "Step 2: Merging files completed successfully!"

#-------------------------------------------------------------------------------------------------------------------------------------------------------#


# Step 3: Phylip file generation
# Define absolute paths
results_dir="./results/annotated_vcf/"
phylip_dir="./results/phylip/"
tools_dir="./tools/vcf2phylip/"
{
python3 "$tools_dir/vcf2phylip.py" -i "$results_dir/merged.vcf" --output-folder "$phylip_dir" || { echo "Error generating Phylip file" >&2; exit 1; }
echo "Step 3: Phylip file generation completed successfully!"
} 2>&1 | tee ./logs/logs_phylip.txt


# Step 4: Tree generation
{
phylip_dir="./results/phylip/"
tree_dir="./results/tree/"
raxmlHPC -s "$phylip_dir/merged.min4.phy" -n merged.tree -m GTRGAMMA -p 12345 || { echo "Error generating tree" >&2; exit 1; }
mv RAxML_*.tree "$tree_dir" || { echo "Error moving tree files" >&2; exit 1; }
echo "Step 4: Tree generation completed successfully!"
} 2>&1 | tee ./logs/logs_tree.txt
sed 's/\.vcf//g' ./results/tree/RAxML_bestTree.merged.tree > ./results/tree/output.tree
sed -i 's/\.vcf//g' ./results/tree/output.tree
sed -i 's/results\/annotated_vcf\///g' ./results/tree/output.tree
sed -i -r 's/_ann//g' ./results/tree/output.tree
sed -i 's|/||g' ./results/tree/output.tree
#sed -i -E 's/(ERR[0-9]*).[a-zA-Z0-9]{3}/\1/g' ./results/tree/output.tree

# Step 5: Phylogenetic analysis 
# Define absolute paths
{
tools_dir="./tools/"
# Step 5: Phylogenetic analysis using ITOL
echo "Step 5: Perform phylogenetic analysis using ITOL."
python3 "$tools_dir/phylogeny_script.py" || { echo "Error running phylogeny_script.py" >&2; exit 1; }
echo "Step 5: Phylogenetic analysis completed successfully!"
} 2>&1 | tee ./logs/logs_phylogeny.txt

***
# Step 6: Gene-variant analysis
# Define absolute paths
results_dir="./results/annotated_vcf/"
stats_dir="./results/stats/"
tools_dir="./tools/snpEff/"

# Array of impact levels
impact_levels=("HIGH" "LOW" "MODIFIER" "MODERATE")
# Array of effect types
effect_types=("missense_variant" "synonymous_variant")

# This code takes each annotated vcf file and finds genes and their SNPs
for file in "$results_dir"*.tbi
do
    if [ -e "$file" ]
    then
        rm "$file"
    fi
done

# Loop over the VCF files
for vcf in "$results_dir"*_ann.vcf
do
    # Extract the base name of the file (without the .vcf extension) to use in the output file names
    base_name=$(basename "$vcf" .vcf)

    # Loop over the impact levels
    for impact in ${impact_levels[@]}
    do
        java -jar "$tools_dir/SnpSift.jar" filter "(ANN[*].IMPACT has '$impact')" "$vcf" | java -jar "$tools_dir/SnpSift.jar" extractFields - CHROM POS ID REF ALT "ANN[*].GENE" "ANN[*].IMPACT" > "$stats_dir/${base_name}_${impact}_impact_variants.txt" || { echo "Error processing impact $impact for $vcf" >&2; exit 1; }
    done

    # Loop over the effect types
    for effect in ${effect_types[@]}
    do
        java -jar "$tools_dir/SnpSift.jar" filter "(ANN[*].EFFECT has '$effect')" "$vcf" | java -jar "$tools_dir/SnpSift.jar" extractFields - CHROM POS ID REF ALT "ANN[*].GENE" "ANN[*].EFFECT" > "$stats_dir/${base_name}_${effect}.txt" || { echo "Error processing effect $effect for $vcf" >&2; exit 1; }
    done

    echo "Step 6: Gene-variant analysis for Sample $base_name completed successfully!"
done 2>&1 | tee ./logs/logs_gene-variant.txt




# Step 7: Make a table out of above files generated - calling python script
# Define absolute paths
tools_dir="./tools/"
{
python3 "$tools_dir/script.py" || { echo "Error running script.py" >&2; exit 1; }
echo "Step 7: Table generation completed successfully!"
} 2>&1 | tee ./logs/logs_step7.txt
***
echo "Thank you for running the postVCF pipeline!"




