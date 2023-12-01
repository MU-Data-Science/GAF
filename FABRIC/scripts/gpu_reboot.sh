#!/usr/bin/env bash

remote_command="
crontab $HOME/cron_command.txt
sleep 2
sudo reboot
"

pid_list=""
nodes=0

# Assumption that master node is 192.168.1.1

for ip in `cat /home/$USER/gpu_ips.txt`
do
	#not required since gpu_ips.txt contains IPs of GPU containing nodes: Remove it.
        if [ "$ip" = "192.168.1.1" ]; 
        then
                continue
        else
                scp $HOME/GAF/FABRIC/scripts/interfaces_up.sh $ip:$HOME/
                scp $HOME/GAF/FABRIC/scripts/cron_command.txt $ip:$HOME/
                ssh -o "StrictHostKeyChecking no" $ip "$remote_command" > /dev/null 2>&1
        fi
done

echo "waiting for reboot to finish"

for ip in `cat /home/$USER/gpu_ips.txt`
do
        until nc -vzw 2 $ip 22; do sleep 2; done
done
