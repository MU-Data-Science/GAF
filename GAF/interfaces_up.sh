#!/usr/bin/env bash

path_to_interface=/home/ubuntu/interfaces.txt

ls /sys/class/net > $path_to_interface

interface_name="ens"
interface_list=""

while IFS= read -r line
do
  echo $interface_list >> $HOME/log.txt
  if [[ "$line" == *"$interface_name"* ]]; then
        interface_list="$interface_list $line"
  fi
done < "$path_to_interface"

interface_number=""


for interface in ${interface_list[@]};
do
        interface_number="$interface_number ${interface:0-1}"
done

largest=0

for number in ${interface_number[@]};
do
        if (( $largest <= $((number)) )); then
                largest=$((number))
        fi
done

interface_name="$interface_name${largest}"
echo $interface_name > /home/$USER/hosts
hostname=`hostname`
IP=${hostname:0-1}

sudo ifconfig $interface_name up
sudo ifconfig $interface_name 192.168.1.$IP
sudo ifconfig $interface_name broadcast 0.0.0.0

rm $path_to_interface
