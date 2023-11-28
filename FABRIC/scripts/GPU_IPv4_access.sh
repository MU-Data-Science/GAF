
remote_command='
sudo bash /home/ubuntu/nat64.sh >/dev/null 2>&1'

for ip in `cat /home/$USER/gpu_ips.txt`
do
        ssh -o "StrictHostKeyChecking no" $ip "$remote_command" &
done

echo "hopefully can download from IPv4 sites"

