#!/bin/bash

# Size of the swap file in gigabytes
SWAP_SIZE=100

# Check if swap is already enabled
if swapon --show | grep -q 'filename'; then
    echo "Swap already enabled."
    exit 0
fi

# Create a swap file
sudo fallocate -l ${SWAP_SIZE}G /swapfile

# Set the correct permissions
sudo chmod 600 /swapfile

# Set up the swap file
sudo mkswap /swapfile

# Enable the swap file
sudo swapon /swapfile

# Make the swap file permanent
echo '/swapfile none swap sw 0 0' | sudo tee -a /etc/fstab

# Verify the swap file
sudo swapon --show

echo "Swap has been created and enabled."
