#!/usr/bin/env bash

echo "!!! Use the screen command to run this script !!!"

${HOME}/GAF/Tutorial/scripts/setup-GPUs.sh

# Source: https://pytorch.org/tutorials/intermediate/tiatoolbox_tutorial.html
sudo DEBIAN_FRONTEND=noninteractive apt-get -y install libopenjp2-7-dev libopenjp2-tools openslide-tools libpixman-1-dev
sudo DEBIAN_FRONTEND=noninteractive apt install python3-pip
pip install histoencoder
pip install git+https://github.com/TissueImageAnalytics/tiatoolbox.git@develop
pip install pandas
echo "👉 Done installing all the software tools."