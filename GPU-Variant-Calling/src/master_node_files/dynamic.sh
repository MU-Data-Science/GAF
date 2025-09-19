#!/bin/bash
# Define the log file
log_file="dynamic_logs_$(date +%Y%m%d_%H%M%S).log"
# Redirect all output (stdout and stderr) to the log file
exec > >(tee -a "$log_file") 2>&1

ssh vm1 <<EOF
rm -r -f /mydata/GPU-Variant-Calling/results/*
EOF
# Extracting the list of SRR/ERR samples - using python seed to get the same sample names as used with greedy and jobshop
samples=$(python3 - <<EOF
import pandas as pd
df = pd.read_csv('/home/ubuntu/GPU-Variant-Calling/data/input.csv')
samples = df.iloc[:, 0].sample(n=10, random_state=$1)
print(' '.join(samples))
EOF
)
echo $samples
# Function to execute the job on a VM
execute() {
    vm=$1  # Empty VM
    sample=$2  # SRR/ERR sample
    echo "Executing sample $sample on vm${vm}"
    screen -dmS vm${vm}_session bash -c "
    ssh vm${vm} << 'EOF'
<<<<<<< HEAD
    cd /mydata/GPU-Variant-Calling/src
    ./parabricks_call_by_sample.sh -s $sample
=======
    cd /mydata/GPU-Variant-Calling
    # ./parabricks_call_by_sample.sh -s $sample
>>>>>>> 0ba6a5b4fa8fb912b6ab35f375779b1c0a06f1ed
    EOF
    "
}
# Function to wait for GPUs to be free and screen sessions to finish for all VMs
wait_for_gpus_and_screens() {
    local num_vms=$1
    for vm in $(seq 1 $num_vms); do
        while true; do
            if ! screen -list | grep -q "vm${vm}_session"; then
                echo "Screen sessions are running"
                break
            else
                # echo "GPUs on vm${vm} are in use or screen sessions are running, waiting..."
                sleep 5
            fi
        done
    done

    num_vms=$1
    for vm in $(seq 1 $num_vms); do
        while true; do
            if ssh vm${vm} "nvidia-smi | grep -q 'No running processes found'"; then
                echo "GPUs on vm${vm} are free "
                break
            else
                # echo "GPUs on vm${vm} are in use or screen sessions are running, waiting..."
                sleep 1
            fi
        done
    done
}

# Function to check for free VMs and execute jobs
check_free_vms() {
    num_vms=5
    sample=$1
    while true; do
        all_busy=true
        for vm in $(seq 1 $num_vms); do
            if ssh vm${vm} "nvidia-smi | grep -q 'No running processes found'" && ! screen -list | grep -q "vm${vm}_session"; then
                echo "GPUs on vm${vm} are free and ready to use"
                echo "executing  $sample on $vm ✅ "
                execute $vm $sample 
                # sleep 10
                return  # Exit the loop once a job is assigned
            fi
        done
        if $all_busy; then
            echo "All GPUs are busy. Waiting for a free machine..."
            all_busy=false
        fi
        sleep 10  # Wait before checking again
    done
}
# Start the timer
start=$(date +%s)
# Loop through each sample and assign it to a free VM
for sample in $samples; do
    echo "Processing sample: $sample"
    check_free_vms $sample
done
echo "Waiting for GPUs and screens to be idle..."
wait_for_gpus_and_screens 5
# Check the return value of the function
if [ $? -eq 0 ]; then
    echo "System is idle, subset completed 🥳 "
else
    echo "System is busy, please keep patience"
fi
# End the timer
end=$(date +%s)
duration=$((end - start))
echo "Total Time: $duration seconds"
ssh vm1 <<EOF
rm -r -f  /mydata/GPU-Variant-Calling/results/*.bam
rm -r -f /mydata/GPU-Variant-Calling/results/*.chrs*
mkdir -p /mydata/GPU-Variant-Calling/experiment_dynamic_5_${1}
mv  /mydata/GPU-Variant-Calling/results/* /mydata/GPU-Variant-Calling/experiment_dynamic_5_${1}
EOF
scp ./dynamic*.log vm1:/mydata/GPU-Variant-Calling/experiment_dynamic_5_${1}
rm -r -f ./*.log

<<<<<<< HEAD
# ./src/master_node_files/dynamic 10
=======
# ./src/master_node_files/dynamic 10
>>>>>>> 0ba6a5b4fa8fb912b6ab35f375779b1c0a06f1ed
