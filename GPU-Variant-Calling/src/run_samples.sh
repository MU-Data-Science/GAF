
#Logg file
log_file="run_samples.log"

# Redirect stdout and stderr to the log file
exec > >(tee -a "$log_file") 2>&1

# Start time
start_time=$(date +%s)

echo "Script started at: $(date)"

# Read each line from sample_names.txt and run the command
while IFS= read -r sample; do
	    ./parabricks_call_by_sample.sh -s "$sample"
    done < sample_names.txt

# End time
end_time=$(date +%s)

echo "Script ended at: $(date)"


# Calculate and print the time taken
time_taken=$((end_time - start_time))
echo "Time taken: $time_taken seconds"
