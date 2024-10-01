SHARE_DIR="/mydata"
INSTALL_DIR="/mydata"

cmd='
rm /mydata/hs*
'
temp=0

#for ip in `cat /home/ubuntu/ips.txt`
#do
#       if [ $temp -eq 0 ];
#       then
#               temp=$((temp+1))
#       else
#               ssh -o "StrictHostKeyChecking no" $ip "$cmd"
#       fi
#done
#
#exit

for ip in `cat /home/$USER/ips.txt`
do
        if [ $temp -eq 0 ] ;
        then
                temp=$((temp+1))
        else
                scp /mydata/hs38.dict $ip:$SHARE_DIR
                scp /mydata/hs38.fa.amb $ip:$SHARE_DIR
                scp /mydata/hs38.fa.bwt $ip:$SHARE_DIR
                scp /mydata/hs38.fa.img $ip:$SHARE_DIR
                scp /mydata/hs38.fa.sa $ip:$SHARE_DIR
                scp /mydata/hs38.fa $ip:$SHARE_DIR
                scp /mydata/hs38.fa.ann $ip:$SHARE_DIR
                scp /mydata/hs38.fa.fai $ip:$SHARE_DIR
                scp /mydata/hs38.fa.pac $ip:$SHARE_DIR
		scp /home/ubuntu/NSF-CC-GAF/GAF/sampleIDs-vlarge.txt $ip:$SHARE_DIR
        fi

done




#alternative code
##!/bin/bash

# Array of VM IP addresses or hostnames
#vms=("vm1" "vm2" "vm3" "vm4" "vm5" "vm6" "vm7")

# Loop over the VMs
#for vm in "${vms[@]}"; do
  # Use scp to copy the files
 # scp /mydata/hs38.*  ubuntu@$vm:/mydata
#done
