#!/usr/bin/env bash

# Assuming all the nodes in the cluster uses the same OS image
distribution=$(. /etc/os-release;echo $ID$VERSION_ID)

remote_command="
sudo apt-get update --yes
sudo apt install docker.io --yes
curl -fsSL https://nvidia.github.io/libnvidia-container/gpgkey | sudo gpg --dearmor -o /usr/share/keyrings/nvidia-container-toolkit-keyring.gpg \
      && curl -s -L https://nvidia.github.io/libnvidia-container/'$distribution'/libnvidia-container.list | \
            sed 's#deb https://#deb [signed-by=/usr/share/keyrings/nvidia-container-toolkit-keyring.gpg] https://#g' | \
            sudo tee /etc/apt/sources.list.d/nvidia-container-toolkit.list
sudo apt-get update --yes
sudo apt-get install -y nvidia-docker2
sudo systemctl restart docker
sudo modprobe overlay && sudo modprobe br_netfilter
"

for ip in `cat /home/$USER/gpu_ips.txt`
do
        ssh -tt -o "StrictHostKeyChecking no" $ip "$remote_command" > /home/$USER/NSF-CC-GAF/GAF/LOG_parabricks_install.log 2>&1
done
