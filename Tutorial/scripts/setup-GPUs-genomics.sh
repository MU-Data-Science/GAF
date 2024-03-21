#!/usr/bin/env bash

echo "!!! Use the screen command to run this script !!!"

${HOME}/GAF/Tutorial/scripts/setup-GPUs.sh

PARABRICKS="nvcr.io/nvidia/clara/clara-parabricks:4.2.1-1"
echo "👉 Downloading Parabricks."
sudo docker pull ${PARABRICKS}
echo "👉 Done installing all the software tools."
