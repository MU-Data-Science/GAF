DATA_DIR="/mydata"
GATK_VER="4.1.8.0"

wget https://repo.anaconda.com/miniconda/Miniconda3-py37_4.12.0-Linux-x86_64.sh -P $DATA_DIR > /dev/null 2>&1

for ip in `cat /home/$USER/ips.txt`
do
        scp $DATA_DIR/Miniconda3-py37_4.12.0-Linux-x86_64.sh $ip:$DATA_DIR > /dev/null 2>&1
done

remote_command='
bash '$DATA_DIR'/Miniconda3-py37_4.12.0-Linux-x86_64.sh -b -p '$DATA_DIR'/miniconda
'$DATA_DIR'/miniconda/bin/conda init
'

nodes=0
pid_list=""

for ip in `cat /home/$USER/ips.txt`
do
        echo $ip
        ssh -tt -o "StrictHostKeyChecking no" $ip "$remote_command" & > /dev/null
        pid_list="$pid_list $!"
        nodes=$((nodes+1))
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

echo "Installation of conda done"
echo "downloading and configuring GATK"

wget https://github.com/broadinstitute/gatk/releases/download/$GATK_VER/gatk-$GATK_VER.zip -P $DATA_DIR > /dev/null 2>&1

for ip in `cat /home/$USER/ips.txt`
do
        scp $DATA_DIR/gatk-$GATK_VER.zip $ip:$DATA_DIR/gatk-$GATK_VER.zip
done

echo "GATK copied to all the nodes"

remote_command='
sudo apt install unzip ;
unzip '$DATA_DIR'/gatk-'$GATK_VER'.zip -d '$DATA_DIR' ;
mkdir '$DATA_DIR'/gatk-tmp ;
'$DATA_DIR'/miniconda/bin/conda env create -n gatk -f '$DATA_DIR'/gatk-'$GATK_VER'/gatkcondaenv.yml ;
echo "export CONDA_HOME='$DATA_DIR'/miniconda
export PATH=\$PATH:\$CONDA_HOME/envs/gatk/bin:\$CONDA_HOME/bin" >> ~/.bashrc ;
sudo ln -s /mydata/miniconda/envs/gatk/bin/python3.6 /usr/bin/python
'

nodes=0
pid_list=""

for ip in `cat /home/$USER/ips.txt`
do
        ssh -tt -o "StrictHostKeyChecking no" $ip "$remote_command" & > /dev/null
        pid_list="$pid_list $!"
        nodes=$((nodes+1))
        
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

nodes=0
pid_list=""

conda_package='
/mydata/miniconda/bin/conda install pandas --yes
/mydata/miniconda/bin/conda install matplotlib --yes
'

for ip in `cat /home/$USER/ips.txt`
do
        ssh -tt -o "StrictHostKeyChecking no" $ip "$conda_package" & > /dev/null
        pid_list="$pid_list $!"
        nodes=$((nodes+1))
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
