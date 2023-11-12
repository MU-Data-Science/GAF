#!/usr/bin/env bash

remote_command='
[ -b "/dev/nvme0n1" ] && ( yes | ( sudo pvcreate /dev/nvme0n1 ; 
sudo vgcreate mydata_vg /dev/nvme0n1 ;
sudo lvcreate -l 100%FREE -n mydata_lv mydata_vg ;
sudo mkfs.ext4 /dev/mydata_vg/mydata_lv ;
sudo mkdir /mydata ;
sudo mount /dev/mydata_vg/mydata_lv /mydata ) )
'

remote_command1='
[ ! -d "/mydata" ] && ( sudo mkdir /mydata )
'

pid_list=""
nodes=0

# Assumption is that in master node there is no NVMe drive
for ip in `cat /home/$USER/ips.txt`
do
        if [ "$ip" = "192.168.1.1" ];
        then
                ssh -o "StrictHostKeyChecking no" $ip "$remote_command1" & > /dev/null
        else
                ssh -o "StrictHostKeyChecking no" $ip "$remote_command" & > /dev/null
		pid_list="$pid_list $!"
                echo $pid_list
                nodes=$((nodes+1))
        fi
done

pid_list_finished=""
total_nodes=$nodes

while true; 
do
        echo "instaling.."
        for pid in $pid_list;
        do
                if ps -p $pid > /dev/null
                then
                        continue
                else
                        if [ "$pid_list_finished" = "" ];
                        then
                                pid_list_finished="$pid_list_finished $pid"
                                nodes=$((nodes-1))
                                echo "Nodes Remaining : " "$total_nodes /" "$nodes" 
                        else
                                if [[ $pid_list_finished == *"$pid"* ]];
                                then
                                        nodes=$((nodes-1))
                                        echo "Nodes Remaining : " "$total_nodes /" "$nodes"
                                fi
                        fi
                fi
        done

        if [ $nodes -eq 0 ] ;
        then
                break
        fi

        sleep 3
done


for ip in `cat /home/$USER/ips.txt`
do
        if [ "$ip" = "192.168.1.1" ];
        then
                continue
        else
                ssh -o "StrictHostKeyChecking no" $ip "$remote_command1" & > /dev/null
        fi
done

