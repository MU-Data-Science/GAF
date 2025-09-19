num_vms=$1
for vm in $(seq 1 $num_vms); do
    while true; do
        if ssh vm${vm} "nvidia-smi | grep -q 'No running processes found'"; then
            echo "GPUs on vm${vm} are free "
            break
        else
            echo "GPUs on vm${vm} are in use or screen sessions are running, waiting..."
            sleep 1
        fi
    done
done
