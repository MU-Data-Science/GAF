# List of nodes
nodes=("vm0" "vm1" "vm2" "vm3" "vm4" "vm5" "vm6" "vm7")

# Loop over each node
for node in "${nodes[@]}"; do
    # Run the mkdir command on the node
    ssh "$node" "mkdir -p /mydata/genomes/renamed"
done
