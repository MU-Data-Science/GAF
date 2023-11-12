export DEBIAN_FRONTEND=noninteractive

if [[ $# -lt 1 ]];
then
        echo " Usage: cluster_config.sh [option]"
        echo ""
        echo "option: use -G for GATK"
        echo "option: use -A for Adam/Cannoli"
        exit

fi

if [ "$1" = "-A" ];
then
        echo "Adam/Cannoli is selected"
        scala_version="2.12.8"
        spark_version="3.2.2"
        spark_hadoop_version="3.2"
        hadoop_version="3.3"
        hadoop_sub_version="1"

elif [ "$1" = "-G" ];
then
        echo "GATK is selected"
        scala_version="2.11.8"
        spark_version="2.4.7"
        spark_hadoop_version="2.7"
        hadoop_version="2.7"
        hadoop_sub_version="6"
else
        echo "Please look for usage in this script"
        exit
fi

# TO DO: to check for each node in cluster whether nat64 is there or not and run it.
scripts=("nvme_config" \
	"install_java" \
        "install_hadoop" \
        "install_spark" \
	"install_gatk" \
	"install_utils" \
	"install_gpu_driver" \
	"install_parabricks" \
	"gpu_reboot" \
	"GPU_IPv4_access")

pid=""

completed=1
master_ip=""

for ip in `cat /home/$USER/ips.txt`
do
        master_ip=$ip
        break
done

echo ">>> Installing necessary tools and softwares <<<"
echo " "
echo ">>> It will take no more than watching a movie trailer!!! <<< "
echo " "

for script in "${scripts[@]}"
do
        log_file="LOG-"$script".log"
        command='bash /home/'$USER'/NSF-CC-GAF/GAF/'$script'.sh '$scala_version' '$spark_version' '$spark_hadoop_version' '$hadoop_version' '$hadoop_sub_version' &> /home/'$USER'/NSF-CC-GAF/GAF/'$log_file''
        ssh -o "StrictHostKeyChecking no" $master_ip "$command" &
        pid="$pid $!"

        while true;
        do
                if ps -p $pid > /dev/null
                then
                        sleep 3
                else
                        break
                fi
        done

        echo "Running script $script :" "Done with stages ($completed / ${#scripts[@]})"
        completed=$((completed+1))

done

echo ">>> Done with the installation <<<"
echo ""
echo "*************************************************************************"
echo ">>> Please Logout and login into the system OR run \"source ~/.bashrc\" <<<"
echo "   >>> After that run the script \"start_spark_hadoop_cluster.sh\" <<< "
echo "*************************************************************************"
                                                                                     
