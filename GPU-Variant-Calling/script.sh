#!/bin/bash

# Function to clean up files
cleanup() {
    rm -r -f ~/GPU-Variant-Calling/schedule.log ~/GPU-Variant-Calling/schedule_m*.txt
    ssh vm1 "rm -r -f /mydata/GPU-Variant-Calling/results/* /mydata/GPU-Variant-Calling/logs/*"
}

# Function to start/stop/collect CPU/GPU utilization plots
utilization() {
	local action=$1
	local num_nodes=$2
	local algo=$3
	local subset=$4
	cd ~/GPU-Variant-Calling/plots || exit
	python3 run_gpu_stat.py "$action" "$num_nodes"
	python3 run_dstat.py "$action" "$num_nodes"
	if [ "$action" == "collect" ] ; then
		#mv vm*-gpu-report.out vm*-gpu-report_"$algo"_"$subset".out
	for file in vm*-report.out; do
		    if [ -f "$file" ]; then
		    	mv "$file" "${file%.out}_"$algo"_"$subset".out"
		    fi
	done	
	fi
	cd ~
}

plot() {
	local num_nodes=$1
	cd ./plots/ || exit
	python3 run_gpu_stat.py plot "$num_nodes" sm
	#python3 run_dstat.py plot "$num_nodes"
	cd ~
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

# Function to execute schedule generation and execution
execute_schedule() {
    local algo=$1
    local stage=$2
    local machines=$3
    local subset_size=$4
    local input_file=$5
    local experiment_type=$6 # greedy/jobshop variable 
    local subset_number=$7 # $i comes here
    cd ~/GPU-Variant-Calling/
    python3 ./src/master_node_files/schedule_generation.py -a $algo -s $stage -m $machines -b $subset_size -sn $subset_number $input_file
    scp ./schedule_m*.txt vm1:/mydata/GPU-Variant-Calling/
    echo "Running schedule_execution.sh"
    if [ "$stage" = "stage1" ]; then
<<<<<<< HEAD
        ./src/master_node_files/schedule_execution_s1.sh $machines
        wait_for_gpus_and_screens $machines #waiting for GPUs to be free and screen sessions to finish
    else
        ./src/master_node_files/schedule_execution_s2.sh $machines
=======
        # ./src/master_node_files/schedule_execution_s1.sh $machines
        wait_for_gpus_and_screens $machines #waiting for GPUs to be free and screen sessions to finish
    else
        # ./src/master_node_files/schedule_execution_s2.sh $machines
>>>>>>> 0ba6a5b4fa8fb912b6ab35f375779b1c0a06f1ed
        wait_for_gpus_and_screens $machines #waiting for GPUs to be free and screen sessions to finish
    fi
    ssh  vm1 <<EOF
mkdir /mydata/GPU-Variant-Calling/experiment_${experiment_type}_${machines}_${subset_number}_${stage}
rm -r -f /mydata/GPU-Variant-Calling/results/*.bam
rm -r -f /mydata/GPU-Variant-Calling/results/*.chrs*
mv /mydata/GPU-Variant-Calling/results/* /mydata/GPU-Variant-Calling/experiment_${experiment_type}_${machines}_${subset_number}_${stage}
mv /mydata/GPU-Variant-Calling/logs/* /mydata/GPU-Variant-Calling/experiment_${experiment_type}_${machines}_${subset_number}_${stage}
mv /mydata/GPU-Variant-Calling/schedule_m* /mydata/GPU-Variant-Calling/experiment_${experiment_type}_${machines}_${subset_number}_${stage}
rm -r -f /mydata/GPU-Variant-Calling/flocks/*

EOF

    scp ./schedule.log vm1:/mydata/GPU-Variant-Calling/experiment_${experiment_type}_${machines}_${subset_number}_${stage}
}
# Main script execution
cleanup
for i in $(seq 1 $1); do # $1 is the number of subsets and $i goes from 1,2,3,...,$1
    echo "Running subset $i"
    if [ "$4" -eq 0 ]; then
        # Greedy Schedule Generation and Execution
        echo "Executing plan for Greedy"
	    utilization start 6
        echo $pwd
        execute_schedule "greedy" "stage1" $2 $3 $5 "greedy" $i
        utilization stop 6 
	    utilization collect 6 "greedy" "$i"
        scp ~/GPU-Variant-Calling/plots/vm* vm1:/mydata/GPU-Variant-Calling/experiment_greedy_${2}_${i}_stage1
        rm -r -f ~/GPU-Variant-Calling/plots/vm*
	cleanup
    elif [ "$4" -eq 1 ]; then	
        # JobShop Schedule Generation
        echo "Executing plan for JobShop"
	    utilization start 6
        execute_schedule "jobshop" "stage1" $2 $3 $5 "jobshop" $i
        utilization stop 6
        utilization collect 6 "jobshop_stage1" "$i"
        scp ~/GPU-Variant-Calling/plots/vm* vm1:/mydata/GPU-Variant-Calling/experiment_jobshop_${2}_${i}_stage1
        rm -r -f ~/GPU-Variant-Calling/plots/vm*
	cleanup
    elif [ "$4" -eq 2 ]; then
        # JobShop Schedule Generation and Execution
        echo "Executing plan for JobShop"
        utilization start 6
	    execute_schedule "jobshop" "stage2" $2 $3 $5 "jobshop" $i
	    utilization stop 6
        utilization collect 6 "jobshop_stage2" "$i"
        scp ~/GPU-Variant-Calling/plots/vm* vm1:/mydata/GPU-Variant-Calling/experiment_jobshop_${2}_${i}_stage2
        rm -r -f ~/GPU-Variant-Calling/plots/vm*
	    ssh vm1 'if [ -z "$(ls -A /mydata/GPU-Variant-Calling/results/ 2>/dev/null)" ]; then echo "Directory is empty. Running the next command..."; else echo "Directory is not empty."; fi'
        cleanup 
    elif [ "$4" -eq 3 ]; then
        # Calling dynamic plan execution
        echo "Executing samples dynamically"
        utilization start 6
        cd ~/GPU-Variant-Calling
        ./src/master_node_files/dynamic.sh $i 
        utilization stop 6
        utilization collect 6 "dynamic" "$i"
        scp ~/GPU-Variant-Calling/plots/vm* vm1:/mydata/GPU-Variant-Calling/experiment_dynamic_${2}_${i}
        rm -r -f ~/GPU-Variant-Calling/plots/vm*
    elif [ "$4" -eq 4 ]; then
        # Greedy Schedule Generation and Execution
        echo "Executing plan for Greedy2"
	    utilization start 6
        echo $pwd
        execute_schedule "greedy2" "stage1" $2 $3 $5 "greedy2" $i
        utilization stop 6 
	    utilization collect 6 "greedy2" "$i"
        scp ~/GPU-Variant-Calling/plots/vm* vm1:/mydata/GPU-Variant-Calling/experiment_greedy2_${2}_${i}_stage1
        rm -r -f ~/GPU-Variant-Calling/plots/vm*
    else
        echo "Invalid stage entered please correct the stage/algorithm"
    fi
done
# uncommenting the first row in the gpu-report.out files
for file in vm*-gpu-report_*.out; do
  [ -f "$file" ] && sed -i '1s/^# *//' "$file"
done


# Directions of use
# python3 schedule_generation.py -a greedy -s stage1 -m 5 -b 10 ./data/input.csv
# ./script.sh subsets_number machines subset_size stage input_file
# ./script.sh 10 5 10 1 input.csv
# ./script.sh 3 5 10 2 input_S2.csv 
# 0 - greedy, 1 - JS, 2 - JS2, 3 - Dynamic, 4 - Greedy2 
<<<<<<< HEAD

=======
>>>>>>> 0ba6a5b4fa8fb912b6ab35f375779b1c0a06f1ed
