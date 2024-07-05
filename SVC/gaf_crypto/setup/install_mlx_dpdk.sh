#!/usr/bin/env bash

echo "👉 Starting the installation process."

# update /etc/hosts
sudo -- sh -c "echo 127.0.0.1 $(hostname) >> /etc/hosts"

# nat
chmod +x ${HOME}/nat64.sh
${HOME}/nat64.sh

# update
sudo DEBIAN_FRONTEND=noninteractive apt update
sudo DEBIAN_FRONTEND=noninteractive apt install bzip2 -y
sudo DEBIAN_FRONTEND=noninteractive apt install build-essential -y

# intel-ipsec
wget https://github.com/intel/intel-ipsec-mb/archive/refs/tags/v1.3.tar.gz
tar xvfz v1.3.tar.gz
cd intel-ipsec-mb-1.3/
sudo DEBIAN_FRONTEND=noninteractive apt install nasm -y
make
sudo make install
cd ${HOME}

# Mellanox drivers
MLNX_SRC=MLNX_OFED_LINUX-24.01-0.3.3.1-ubuntu22.04-x86_64
wget https://content.mellanox.com/ofed/MLNX_OFED-24.01-0.3.3.1/${MLNX_SRC}.tgz
tar xvfz ${MLNX_SRC}.tgz
cd ${MLNX_SRC}
sudo ./mlnxofedinstall --add-kernel-support --without-fw-update --force
cd ${HOME}

# infiniband
sudo /etc/init.d/openibd restart
sudo mst start

# Uncomment if you want to use dpdk releases
# wget http://fast.dpdk.org/rel/dpdk-23.03.tar.gz
# tar xvfz dpdk-23.03.tar.gz
# cd dpdk-23.03/
cd ${HOME}/dpdk
sudo DEBIAN_FRONTEND=noninteractive apt-get update
sudo DEBIAN_FRONTEND=noninteractive apt-get install python3-pip -y
sudo DEBIAN_FRONTEND=noninteractive apt install libnuma-dev -y
sudo pip3 install ninja
sudo pip3 install meson
sudo pip3 install pyelftools
meson -Dexamples=all build
cd build
sudo ninja install
sudo ldconfig
cd ${HOME}

# huge pages
sudo mkdir -p /dev/hugepages
sudo mountpoint -q /dev/hugepages || mount -t hugetlbfs nodev /dev/hugepages
sudo -- sh -c "echo 128 > /sys/devices/system/node/node0/hugepages/hugepages-2048kB/nr_hugepages"

# ifconfig
sudo DEBIAN_FRONTEND=noninteractive apt install net-tools -y

# for process/system monitoring
sudo pip3 install psutil

echo "👉 Completed the installation process."