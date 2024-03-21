#!/usr/bin/env bash

UBUNTU_VERSION="ubuntu2004"
PARABRICKS="nvcr.io/nvidia/clara/clara-parabricks:4.2.1-1"
ARCHITECTURE='x86_64'

sudo apt-get -q update

if [[ -f /usr/bin/nvidia-smi ]]; then
  echo "👉 CUDA already installed."
else
  echo "👉 Installing CUDA for "$UBUNTU_VERSION"."

  # Source: https://developer.nvidia.com/cuda-12-0-0-download-archive
  wget https://developer.download.nvidia.com/compute/cuda/repos/$UBUNTU_VERSION/$ARCHITECTURE/cuda-$UBUNTU_VERSION.pin
  sudo mv cuda-$UBUNTU_VERSION.pin /etc/apt/preferences.d/cuda-repository-pin-600
  wget https://developer.download.nvidia.com/compute/cuda/12.0.0/local_installers/cuda-repo-$UBUNTU_VERSION-12-0-local_12.0.0-525.60.13-1_amd64.deb
  sudo dpkg -i cuda-repo-$UBUNTU_VERSION-12-0-local_12.0.0-525.60.13-1_amd64.deb
  sudo cp /var/cuda-repo-$UBUNTU_VERSION-12-0-local/cuda-*-keyring.gpg /usr/share/keyrings/
  sudo apt-get update
  sudo DEBIAN_FRONTEND=noninteractive apt-get -y install cuda
  echo "👉 Reboot the server to load the driver."
fi

if [[ -f /usr/bin/docker ]]; then
  echo "👉 Docker already installed."
else
  echo "👉 Installing Docker, nvidia-docker2."

  # Source: https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/1.8.1/install-guide.html
  curl https://get.docker.com | sh \
    && sudo systemctl --now enable docker

  distribution=$(. /etc/os-release;echo $ID$VERSION_ID) \
     && curl -s -L https://nvidia.github.io/nvidia-docker/gpgkey | sudo apt-key add - \
     && curl -s -L https://nvidia.github.io/nvidia-docker/$distribution/nvidia-docker.list | sudo tee /etc/apt/sources.list.d/nvidia-docker.list

  sudo apt-get update
  sudo DEBIAN_FRONTEND=noninteractive apt-get install -y nvidia-docker2
  sudo systemctl restart docker
  # To test if GPUs are being recognized by Docker
  #sudo docker run --rm --gpus all nvidia/cuda:12.0.0-base-ubuntu20.04 nvidia-smi
fi

sudo apt update
sudo DEBIAN_FRONTEND=noninteractive apt install nvtop -y
sudo DEBIAN_FRONTEND=noninteractive apt install python3-pip -y

echo "👉 Done installing the GPU driver, Docker, and other tools."
