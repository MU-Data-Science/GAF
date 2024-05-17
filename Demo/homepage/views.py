from django.shortcuts import render, redirect
from django.core.cache import cache 
from django.http import StreamingHttpResponse
from django.http import JsonResponse
from pydrive.auth import GoogleAuth
from pydrive.drive import GoogleDrive
import subprocess, time
import json 
from .models import *
import logging
from django.http import HttpResponse
from flask import Flask, render_template, jsonify
from django_lock import lock
import sys 


genomeName = ""
doCheck = True
uuid = ""


def pipelinerun():
    
    cmd = '/users/ks9dw/AVAH-FABRIC/scripts/run_variant_analysis_at_scale.sh -i /proj/eva-public-PG0/main.txt -d NONE -n 8 -b 2 -p 1 -P H -G -g'
    cmd = '/users/ks9dw/AVAH-FABRIC/scripts/run_variant_analysis_at_scale.sh -i /proj/eva-public-PG0/main.txt -d NONE -n 8 -b 2 -p 1 -P H -G -g'
    host = 'c240g5-110229.wisc.cloudlab.us' 
    username = 'ks9dw'         
    ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',f'{username}@{host}',cmd] 
    command_string = ' '.join(ssh_args)
    ret = subprocess.run(command_string, capture_output=True,shell = True,executable='/bin/bash',text=True,check=True) 
    ret = ret.stdout
    print("avah ret is ",ret)
    print("im here after the command")                



def genomesAccessionDownload(accessionIDs,cluster):

    host = 'c220g5-110932.wisc.cloudlab.us'  
    username = 'shared'
    cmd = ""          
    ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',f'{username}@{host}',cmd]
    
    ##create file content
    srr_ids = "<<EOF\n"
    for gn in accessionIDs:
        srr_ids+= gn+"\n"           
    srr_ids+="EOF"
    
    ##empty file on cluster 
    cmd = 'sudo bash -c "> /mydata/NSF-CC-GAF/download_data/srr_ids.txt"'
    ssh_args[-1] = cmd
    executeCommand(ssh_args)
    
    ##write to file on cluster
    cmd = "sudo tee -a /mydata/NSF-CC-GAF/download_data/srr_ids.txt {}".format(srr_ids)
    ssh_args[-1] = cmd 
    out = executeCommand(ssh_args)
    
    ##download the accession ids
    cmd = "bash /mydata/NSF-CC-GAF/download_data/downloading_SRR_samples.sh"
    ssh_args[-1] = cmd 
    
    completed_process = subprocess.run(ssh_args,shell = True,executable='/bin/bash',capture_output=True, text=True, check=True)
    output = completed_process.stdout
    oerr = completed_process.stderr
    print("output from downloading is ",output)
    print("output err from downloading is ",oerr)
    
    ##move them to /mydata/genomes/
    cmd = "mv /mydata/NSF-CC-GAF/download_data/data/* /mydata/genomes"
    ssh_args[-1] = cmd 
    out = executeCommand(ssh_args)
    

def fileRenames():
    
    uuid = "Ab78c"
    genomeList = ["ERR016314" ,"ERR016316" ,"ERR016317","ERR016320","ERR016326","ERR016327","ERR016338","ERR016344","ERR016350","ERR018197","ERR018198","ERR018204", "ERR018395", "ERR018416","ERR018423" ,"ERR018429", "ERR018435", "ERR018436", "ERR018442", "ERR018448", "ERR018454", "ERR018460", "ERR018463", "ERR018469"] 
    #genomeList = ["1144451014", "1316511682", "1253702634", "1712582059", "2962859843", "4091919508", "2312019116", "2441875387", "1405628913", "2769601391", "2940489805", "2834987902", "1155966013", "4642960047", "2074564581", "1802900012", "2074804536", "1810796785", "4238943221", "1889860913", "4780263286", "2262114215", "2164110037", "2239111491"]
    host = 'c240g5-110229.wisc.cloudlab.us' 
    username = 'ks9dw'    
    cmd = ""     
    ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',f'{username}@{host}',cmd] 
    
    for genome in genomeList:
        print("renaming genome ",genome)
        gnId = genome+uuid
        cmd = "cp /mydata/genomes/{}_1.fastq.gz /mydata/genomes/renamed/{}_1.fastq.gz".format(genome,gnId) 
        ssh_args[-1] = cmd
        command_string = ' '.join(ssh_args)
        subprocess.run(command_string, shell=True,capture_output=True, text=True, check=True)
        
        cmd = "cp /mydata/genomes/{}_2.fastq.gz /mydata/genomes/renamed/{}_2.fastq.gz".format(genome,gnId) 
        ssh_args[-1] = cmd
        command_string = ' '.join(ssh_args)
        subprocess.run(command_string, shell=True,capture_output=True, text=True, check=True)
            
    
    print("copying done")   



def getClusterUtilisations(request):
    
    #json_data = json.loads(request.body)
    #uuid = json_data.get('uuid')
    
    return JsonResponse({'result':[1,2,3]})
    
    ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null','ks9dw@c220g5-111307.wisc.cloudlab.us','cat /etc/prometheus/utilization.json']
    completed_process = subprocess.run(ssh_args,capture_output=True, text=True, check=True)
    output = completed_process.stdout
    utilisation = json.loads(output)
    cpu = utilisation["cpu"]  
    cpu_cluster1 = cpu['clemDemo16']
    cpu_cluster2 = cpu['wiscdemo8']
    cpu_cluster3 = cpu['demo16Wisc']
    cpu_cluster4 = cpu['gpu8']
    print("utilisations are ",cpu_cluster1, cpu_cluster2,cpu_cluster3,cpu_cluster4)
    
    utis = [cpu_cluster1, cpu_cluster2,cpu_cluster3,cpu_cluster4]
    
    #utis = utis.to_json(orient='records')
    
    utis = json.dumps(utis)
    print('response being sent is ',utis)
    
    return JsonResponse({'result':utis})


def autoSelectCluster():
    cluster = ""
    
    # ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null','ks9dw@c220g5-111307.wisc.cloudlab.us','cat /etc/prometheus/utilization.json']
    # completed_process = subprocess.run(ssh_args,capture_output=True, text=True, check=True)
    # output = completed_process.stdout
    # utilisation = json.loads(output)
    # cpu = utilisation["cpu"]  
    # cpu_cluster1 = cpu['Cluster1']
    # cpu_cluster2 = cpu['Cluster2']
    # print("utilisations are ",cpu_cluster1, cpu_cluster2)
    
    
    # ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null','ks9dw@c220g5-111020.wisc.cloudlab.us','cat /proj/eva-public-PG0/secondary.txt | wc -l']
    # jobQueueCluster1  = subprocess.run(ssh_args,capture_output=True, text=True, check=True).stdout
    # print("queue length on cl1 ",float(jobQueueCluster1))
    
    # ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null','ks9dw@clnode162.clemson.cloudlab.us','cat /proj/eva-public-PG0/secondary.txt | wc -l']
    # jobQueueCluster2  = subprocess.run(ssh_args,capture_output=True, text=True, check=True).stdout
    # print("queue length on cl2 ",float(jobQueueCluster2))
    
    # print("type of cpu_clust is",type(float(cpu_cluster1)))
    
    # res1 = 0.5 * float(cpu_cluster1) + 0.5 * jobQueueCluster1 
    # res2 = 0.5 * float(cpu_cluster2) + 0.5 * jobQueueCluster2 
    
    # if res1 < res2:
    #     cluster = "1" 
    # else:
    #     cluster = "2" 
    
    # print("auto selected clsuter is ",cluster)
    return cluster 

def getSSH(cluster):
    
    cmd = ''
    cluster = "cloudlab"
    if cluster == "Fabric":
        #print(cluster+' selected') 
        host = '2001:1948:417:7:f816:3eff:fea9:faff' 
        username = 'ubuntu'
        ssh_args = ['ssh', '-F','/Users/khawar/.ssh/fabric_ssh_config','-i','/Users/khawar/.ssh/sliver',f'{username}@{host}',cmd]
    else:
        #print(cluster+' selected') 
        host = 'c220g5-110932.wisc.cloudlab.us'  
        username = 'shared'
        cmd = ""          
        ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',f'{username}@{host}',cmd]
       
    return ssh_args

def getCommand(commandOptions):
    if commandOptions == 'gpuHigh':
        cmd = "${HOME}/AVAH/scripts/run_variant_analysis_at_scale.sh -i /proj/eva-public-PG0/${USER}-sampleIDs-vlarge.txt -d NONE -n 16 -b 2 -p 1 -P H -G"
    elif commandOptions == 'cpu':
        cmd = "${HOME}/AVAH-FABRIC/scripts/run_variant_analysis_at_scale.sh -i /proj/eva-public-PG0/${USER}-sampleIDs-vlarge.txt -d NONE -n 8 -b 2 -p 17 -P H -G"
    elif commandOptions == 'gpu':
        cmd = "${HOME}/AVAH-FABRIC/scripts/run_variant_analysis_at_scale.sh -i /proj/eva-public-PG0/${USER}-sampleIDs-vlarge.txt -d NONE -n 8 -b 2 -p 17 -P H -G -g -m 2"
    elif commandOptions == 'gpuLow':      
        cmd = "${HOME}/AVAH-FABRIC/scripts/run_variant_analysis_at_scale.sh -i /proj/eva-public-PG0/${USER}-sampleIDs-vlarge.txt -d NONE -n 8 -b 2 -p 17 -P H -G -g -F"  
    return cmd  


def executeCommand(ssh_args):
    try:
        completed_process = subprocess.run(ssh_args,capture_output=True, text=True, check=True)
        output = completed_process.stdout
    
    except subprocess.CalledProcessError as e:
        output = e.stderr   
        print(output)
                     
    return output 
        

def checkAvahRunning(ssh_args):
    return executeCommand(ssh_args)   


logger = logging.getLogger(__name__)

def cronFunc(): 
    host = 'c240g5-110229.wisc.cloudlab.us'
    username = 'ks9dw'        
    
    #1- read all the logs and update record for stages 
    
    cmd = '/users/ks9dw/cmd.sh'
    ## cmd.sh is a bash file that contains the following 
    ## #!/usr/bin/env bash
    ## /mydata/Anaconda3/bin/python3 ${HOME}/AVAH-FABRIC/scripts/run_remote_command.py grep 16 "Completed" /mydata/hadoop/logs/userlogs/ | grep -o "Completed-[^/]*" | grep "ERR018538"
    ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',f'{username}@{host}',cmd]
    command_string = ' '.join(ssh_args)
    
    try:
        result = subprocess.run(command_string, capture_output=True,shell=True,executable='/bin/bash',text=True,check=True)
        result = result.stdout.split()
        if len(result) > 0:
        #open the csv 
            stages = ['BAM','BWAMarkDuplicates','SortSam','GATK_BQSR','GATK_HaplotypeCaller']
            records = pd.read_csv('/home/ubuntu/GAF/Demo/homepage/records.csv')
            print("in new cron job")
            for res in result:
                resSplitted = res.split('-')
                #print(" ")
                #print("grep splits are : ",resSplitted)
                
                # fact we can see a stage here means stage is completed i.e, assign 1 
                stage = resSplitted[1] 
                #print('stage is : ',stage)
                #id = resSplitted[2][:3]
                #print("id is ",id)
                split2 = resSplitted[2]
                #print('split2 is ',split2)
                uuid = split2[-4:] 
                genome = split2[:-4] 
                
                #print('genome is : ',genome)
                #print('uuid is : ',uuid)
                
                ##check if genome is already present in the record or not 
                ## if present, then simply update its stages and vcf/link 
                ## if not, add it 
                ## add or update if our desired stage is reached
                if stage in stages:
                    ##if loc is 0, not found 
                    loc = records[(records['genome'] == genome) & (records['uuid'] == uuid)]
                    if len(loc)>0:
                        #found 
                        loc = loc.index[0]
                        #print("genome present : updating stage")
                        records.loc[loc,stage] = 1
                        records.to_csv('/home/ubuntu/GAF/Demo/homepage/records.csv',index = False)
                    else:
                        #not found 
                        #print("adding genome and stage")
                        data = {'genome':genome, stage:1}  
                        records.loc[len(records)] = data 
                        records.to_csv('/home/ubuntu/GAF/Demo/homepage/records.csv',index = False)
        else:
            print("could not grep userLogs")
            print("checking vcfs")
                
    except subprocess.CalledProcessError as e:   
        print('')
        print("could not grep userLogs")
        #print("Error Output:", e.stderr)
        print('')
    
    #2- check hdfs for vcf files, upload and update 
    ## read the old function (loop over all the files)
    checkVCF()

      

def checkVCF(): 
        
        username_host = 'ks9dw@c240g5-110229.wisc.cloudlab.us' 
        
        #check if vcf is present
        cmd = '/mydata/hadoop/bin/hdfs dfs -test -e /ERR*.vcf && echo "1" || echo "0"'
        ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',username_host,cmd]
        command_string = ' '.join(ssh_args)
        ret = subprocess.run(command_string, shell=True, executable='/bin/bash', capture_output=True, text=True, check=True)
        ret = ret.stdout
        
        #some vcf found 
        if ret[0] == "1":
            print("vcf found")
            cmd = '/mydata/hadoop/bin/hdfs dfs -ls /*.vcf | grep -o "[^/[:space:]]*$"'
            ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',username_host,cmd]
            command_string = ' '.join(ssh_args)
            ret = subprocess.run(command_string, shell=True, executable='/bin/bash', capture_output=True, text=True, check=True)
            vcfsList = ret.stdout.split()
            print("vcf list is ",vcfsList)
            
            #for each vcf, copy - upload and update link 
            # copy to mydata
            for vcf in vcfsList:
                try:
                    print("copying to mydata : ",vcf) 
                    cmd = '/mydata/hadoop/bin/hdfs dfs -copyToLocal /{} /mydata'.format(vcf)
                    ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',username_host,cmd]
                    command_string = ' '.join(ssh_args)
                    ret = subprocess.run(command_string, shell=True, executable='/bin/bash', capture_output=True, text=True, check=True)

                    print("copying to server : ",vcf)
                    cmd = "scp {}:/mydata/{} /home/ubuntu/GAF/Demo/homepage".format(username_host,vcf)
                    result = subprocess.run(cmd, capture_output=True, text=True, check=True, shell = True) 
                    
                    print("uploading  ",vcf)
                    cmd = "python3 /home/ubuntu/GAF/Demo/homepage/drive_upload.py /home/ubuntu/GAF/Demo/homepage/{}".format(vcf)
                    
                    try:
                        driveLink = subprocess.run(cmd, shell = True, capture_output= True, text= True)
                        print("drive link here is ",driveLink)
                        driveLink = driveLink.stdout.strip()
                        print("shareable link is ",driveLink)   
                    except subprocess.CalledProcessError as e: 
                        print('failed to upload, error is ',e.stderr)
                            
                    
                    print("updating link : ",vcf)         
                    records = pd.read_csv('/home/ubuntu/GAF/Demo/homepage/records.csv')
                    vcfGenome = vcf.split('.')[0]
                    uuid = vcfGenome[-4:] 
                    genome = vcfGenome[:-4] 
                    loc = records[(records['genome'] == genome) & (records['uuid'] == uuid)].index[0]
                    
                    records.loc[loc,'link'] = driveLink
                    records.loc[loc,'vcf'] = 1
                    records.to_csv('/home/ubuntu/GAF/Demo/homepage/records.csv',index = False)    
                    
                    
                    print("deleting from mydata ", vcf) 
                    cmd = 'rm /mydata/{}'.format(vcf)
                    ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',username_host,cmd]
                    command_string = ' '.join(ssh_args)
                    ret = subprocess.run(command_string, shell=True, executable='/bin/bash', capture_output=True, text=True, check=True)
                    
                    print("deleting from django server ", vcf)
                    cmd = "rm /home/ubuntu/GAF/Demo/homepage/{}".format(vcf)
                    ret = subprocess.run(cmd, shell = True, capture_output= True, text= True)
                       
                       
                    print("deleting from hdfs : ",vcf)
                    try:
                        cmd = '/mydata/hadoop/bin/hdfs dfs -rm /{}'.format(vcf)
                        ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',username_host,cmd]
                        command_string = ' '.join(ssh_args)
                        ret = subprocess.run(command_string, shell=True, executable='/bin/bash', capture_output=True, text=True, check=True)
                        ret = ret.stdout
                    except subprocess.CalledProcessError as e: 
                        print("error deleting vcf from hdfs : ",e.stderr)
                   
                   
                except subprocess.CalledProcessError as e: 
                    print(f"Command failed with a non-zero exit status. Error: {e}")
                    print(e.stderr)          
        
        else:
            print("vcfs not generated yet")
            
        

def run_single_node():
            host = 'c220g2-010802.wisc.cloudlab.us'  
            username = 'ks9dw'
            
            ##start screen session with screen name 
            cmd = "screen -dm -S startPipelinex"
            ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',f'{username}@{host}',cmd]
            #print("starting screen session..")
            #executeCommand(ssh_args)
            
            ##change working directory to /mydata
            #cmd = 'screen -S startPipelinex -X stuff "cd /mydata\n"'
            cmd = 'screen -S startPipelinex -X stuff "ls\n"'
            ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',f'{username}@{host}',cmd]
            #print("changing directory in screen..")
            #executeCommand(ssh_args)
            
            
            cmd = '[ -e "/mydata/ERR016314_1.fastq.gz" ] && echo "1" || echo "0"'
            cmd = '[ -e "/mydata/test.txt" ] && echo "1" || echo "0"'
            ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',f'{username}@{host}',cmd]
            
            #ret = executeCommand(ssh_args)

            
            ##run pipeline 
            # cmd = "screen -dm bash -c '${HOME}/EVA/scripts/run_variant_analysis_gatk.sh hs38 ERR016314_1.fastq.gz ERR016314_2.fastq.g; exec sh' "
            # cmd = 'screen -S startPipeline -X stuff "${HOME}/EVA/scripts/run_variant_analysis_gatk.sh hs38 ERR016314_1.fastq.gz ERR016314_2.fastq.gz\n"'
            # ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',f'{username}@{host}',cmd]
            #executeCommand(ssh_args)
        
                
def execute_command_view(request):
    records = pd.read_csv("/home/ubuntu/GAF/Demo/homepage/records.csv")
    allGenomeSizes = ["1144451014","1316511682","1253702634","1712582059","2962859843","4091919508","2312019116","2441875387","1405628913","2769601391","2940489805","2834987902","1155966013","4642960047","2074564581","1802900012","2074804536","1810796785","4238943221","1889860913","4780263286","2262114215","2164110037","2239111491","2211401602","7356216287","2170573337"];
    if request.method == 'POST':
        json_data = json.loads(request.body)
        pipeline = json_data.get('pipeline')
        print("pipeline is ",pipeline)
        accessionIDs = json_data.get('accessionIDs')
        print("accession ids are ",accessionIDs)
        uuid = json_data.get('uuid')
        uuid = uuid[:4]
        cluster = json_data.get('cluster')
        print("cluster is ",cluster)
        
        #just to conform with previous code without email
        email = ""
        
        if cluster == "auto":
            cluster = autoSelectCluster()
          
        ssh_args = getSSH(cluster)
        
        if pipeline == 'svc':
            downloadFileContent = json_data.get('fileContent')   
            print('content of download file are',downloadFileContent) 
        else:
            genomeList = json_data.get('genomeList')
            genomeSizes = json_data.get('genomeSizeList')
            print('genome list is ',genomeList)
            print('genome sizes are ',genomeSizes)
            
            ## user choose accession IDs 
            if len(genomeList)<1: 
               accessionIDs = accessionIDs.split(",") 
               for i in range(len(accessionIDs)):
                   accessionIDs[i] = accessionIDs[i].strip 
        
               genomesAccessionDownload(accessionIDs,cluster)
               genomeSizes = genomeSizes[:len(accessionIDs)]
        
        #preparing secondary file and poppulating records.csv
        genomeMain = "<<EOF\n"
        for size, genome in zip(genomeSizes,genomeList):
            genomeMain+= size+" "+genome+uuid+"\n"
            #storing data in records
            data = {'uuid':uuid,'userName': email,'cluster':cluster,'genome':genome,'BAM':0,'BWAMarkDuplicates':0,'SortSam':0,'GATK_BQSR':0,'GATK_HaplotypeCaller':0,'vcf':0,'link':''}
            records.loc[len(records)] = data
            
        genomeMain+="EOF"
    
    
        ##saving records.csv
        records.to_csv('/home/ubuntu/GAF/Demo/homepage/records.csv',index = False)    
        
        
        #adding genome names and user email 
        genomeNames = "<<EOF\n"
        for size, genome in zip(genomeSizes,genomeList):
            genomeNames+= genome+'-'+email+"\n"
        genomeNames+="EOF"
        
 
        cmd = '/mydata/hadoop/bin/yarn application -list | grep "Large-scale genome processing" | grep "RUNNING" && echo "true" || echo "false"'
        ssh_args[-1] = cmd  
        ret = checkAvahRunning(ssh_args)
        print("check avah running output is ",ret)
           
        if ret.find("true") != -1:
            print("pipeline is running")
            pass
            # #Pipeline is running
            # return
            # #start while 
            # print("pipeline is runnning")
            # cmd = '/mydata/hadoop/bin/yarn application -list | grep "Large-scale genome processing" | grep "RUNNING" && echo "true" || echo "false"'
            # ssh_args[-1] = cmd
            # ret = checkAvahRunning(ssh_args)
            
            # while ret.find("true") != 1:
            #     #write genome to secondary file 
            #     cmd = "sudo tee -a /proj/eva-public-PG0/secondary.txt {}".format(genomeMain)
            #     ssh_args[-1] = cmd 
            #     executeCommand(ssh_args)    
                
            #     #save genome names to move to rename genome and move to hdfs 
            #     cmd = "sudo tee -a /proj/eva-public-PG0/genomeNames.txt {}".format(genomeNames)
            #     ssh_args[-1] = cmd 
            #     executeCommand(ssh_args)    
                
            #     #wait 
            #     time.sleep(60)
            
            # #empty main
            # cmd = 'sudo bash -c "> /proj/eva-public-PG0/main.txt"'
            # ssh_args[-1] = cmd
            # executeCommand(ssh_args)
            
            # #copy from secondary to main 
            # cmd = 'cp /proj/eva-public-PG0/secondary.txt /proj/eva-public-PG0/main.txt'
            # ssh_args[-1] = cmd
            # executeCommand(ssh_args)
            
            # #empty secondary 
            # cmd = 'sudo bash -c "> /proj/eva-public-PG0/secondary.txt"'
            # ssh_args[-1] = cmd
            # executeCommand(ssh_args)
            
            #copy files to hdfs 
            
            
            #empty genomeNames file 
            
            
            #run pipeline  
               
                 
        
        
        ################################################################
        ##########################WORKING###############################
        ################################################################
            # 1 . write to secondary 
            # 2 . append to main
            # 3 . if main.length > 30 { 
            #       -rename files and move to hdfs 
            #       -run job
            #       -clear main 
            #       -clear secondary }
            #   else { pass }
        ################################################################
        ################################################################
        ################################################################    
        else:
            #Pipeline is not running 
            print("pipeline not runnning so here we are!")

            #write to secondary file
            cmd = "sudo tee -a /proj/eva-public-PG0/secondary.txt {}".format(genomeMain)
            ssh_args[-1] = cmd 
            out = executeCommand(ssh_args)
            print("wriitng to secondary file :",out)
            
            cmd = "sudo cat /proj/eva-public-PG0/secondary.txt"
            ssh_args[-1] = cmd
            out = executeCommand(ssh_args)
            print("secondary file is :",out)
            
            
            cmd = "sudo cat /proj/eva-public-PG0/main.txt"
            ssh_args[-1] = cmd
            out = executeCommand(ssh_args)
            print("main file is :",out)
            
            
            #append to main file 
            cmd = "sudo cat /proj/eva-public-PG0/secondary.txt >> /proj/eva-public-PG0/main.txt"
            ssh_args[-1] = cmd 
            out = executeCommand(ssh_args)
            print("append to main file is :",out)
            
            cmd = "sudo cat /proj/eva-public-PG0/main.txt"
            ssh_args[-1] = cmd
            out = executeCommand(ssh_args)
            print("main file after append is :",out)
            
            
            #empty secondary file 
            cmd = 'sudo sudo bash -c "> /proj/eva-public-PG0/secondary.txt"'
            ssh_args[-1] = cmd
            executeCommand(ssh_args)
            
            #check main file length (start job when we have 30 genomes job in queue)
            cmd = "sudo cat /proj/eva-public-PG0/main.txt | wc -l"
            ssh_args[-1] = cmd 
            out = int(executeCommand(ssh_args))
            
            if out > 17:
                print("got enough sequences to run job!")
                # #empty main genome file 
                # cmd = 'sudo bash -c "> /proj/eva-public-PG0/main.txt"'
                # ssh_args[-1] = cmd
                # #executeCommand(ssh_args)

                cmd = "cat /proj/eva-public-PG0/main.txt"
                ssh_args[-1] = cmd
                names = executeCommand(ssh_args)
                splits = list(names.split())
                genomeNames = [splits[i] for i in range(len(splits)) if i%2!=0]
                
                
                print("total genomes to be renamed and moved are : ",len(genomeNames))
                print("their names are : ",genomeNames)
                for gn in genomeNames:  
                    print()          
                    genome = gn[:-4]
                    id = gn[-4:]
                    gnId = genome+id
                    
                    #copying part1
                    print("id is : ",id, " genome is : ",genome, "renamed name is : ",gnId)
                    
                    cmd = "cp /mydata/genomes/{}_1.fastq.gz /mydata/genomes/renamed/{}_1.fastq.gz".format(genome,gnId) 
                    print("copy command is ", cmd)
                    ssh_args[-1] = cmd
                    command_string = ' '.join(ssh_args)
                    subprocess.run(command_string, shell=True,capture_output=True, text=True, check=True)
                    
                    #copying part1
                    #cmd = "cp /mydata/genomes/{}_2.fastq.gz /mydata/genomes/renamed/{}-{}_2.fastq.gz".format(genome,id,genome) 
                    cmd = "cp /mydata/genomes/{}_2.fastq.gz /mydata/genomes/renamed/{}_2.fastq.gz".format(genome,gnId)
                    ssh_args[-1] = cmd
                    command_string = ' '.join(ssh_args)
                    subprocess.run(command_string, shell=True,capture_output=True, text=True, check=True)
                    
                    print()
                
                
                print(" --- moving files to hdfs --- ")
                cmd = "/mydata/hadoop/bin/hdfs dfs -put /mydata/genomes/renamed/* /"
                ssh_args[-1] = cmd
                command_string = ' '.join(ssh_args)
                ret = subprocess.run(command_string, shell=True, executable='/bin/bash', capture_output=True, text=True, check=True)
                ret = ret.stdout
        
                
                #run pipeline 
                ## modify the run_variant_analysis_scale.sh 
                ## export HADOOP_HOME="/mydata/hadoop"
                ## export HADOOP_CONF_DIR="/mydata/hadoop/etc/hadoop"
                ## export SPARK_HOME="/mydata/spark"
                cmd = '/users/ks9dw/AVAH/scripts/run_variant_analysis_at_scale.sh -i /proj/eva-public-PG0/main.txt -d NONE -n 16 -b 2 -p 1 -P H -G'
                cmd = '/users/ks9dw/AVAH-FABRIC/scripts/run_variant_analysis_at_scale.sh -i /proj/eva-public-PG0/main.txt -d NONE -n 8 -b 2 -p 15 -P H -G -g'
                host = 'c240g5-110229.wisc.cloudlab.us' 
                username = 'ks9dw'         
                ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',f'{username}@{host}',cmd] #f'export HADOOP_CONF_DIR=/mydata/hadoop/etc/hadoop && {cmd}'
                ssh_args[-1] = cmd
                command_string = ' '.join(ssh_args)
                
                ret = subprocess.run(command_string, capture_output=True,shell = True,executable='/bin/bash',text=True,check=True) #executable='/bin/bash'
                ret = ret.stdout
                print("avah ret is ",ret)
                print("im here after the command")
            
                
            else:
                print("Not enough seqeunces to run the job. Waiting for more users")            
                
        
        print("moving out of main function")
        
    #return render(request, 'options_menu.html')
    return render(request, 'CIKM DEMO.html')
    #return render(request, 'command_form.html')
    


@lock("global")
def checkFiles(request):
    
        print("in file check method")            
        
        json_data = json.loads(request.body)
        uuid = json_data.get('uuid')
        uuid = uuid[:4]
        genomeList = json_data.get('genomeList')
        email = json_data.get('email')
        vcfTotal = json_data.get('vcfTotal')
        
        host = 'c240g5-110229.wisc.cloudlab.us' 
        username = 'ks9dw'
    
        print(uuid, genomeList)
        
        retRecord = pd.DataFrame(columns=['uuid','userName','cluster','genome','BAM','BWAMarkDuplicates','SortSam','GATK_BQSR','GATK_HaplotypeCaller','vcf','link'])
        frames = []
         
        #check the record csv and return matching rows            
        try:
            mainRecord = pd.read_csv('/home/ubuntu/GAF/Demo/homepage/records.csv')
            print("all the record is ")
            print(mainRecord)
            result = mainRecord.query("uuid == '{}'".format(uuid))
            #result = mainRecord.query("uuid == 1923")
            #print("result matching user request is ",result)
        except subprocess.CalledProcessError as e:
            print("Command returned non-zero exit status:", e.returncode) 
            # #print("command is ",ssh_args)
            # try:
            #     result = subprocess.run(ssh_args,  shell=True, executable='/bin/bash', capture_output=True, text=True, check=True)
            #     print(result.stdout)
            # except subprocess.CalledProcessError as e:
            #     print("Command returned non-zero exit status:", e.returncode)    

            # return JsonResponse({'shareable_link': "False",'file_name':"fileName",'uuid':"uuid",'stop':False})
            
            # jsonRes = jsonify(result.to_json(orient='records'))            
        print("result is ",result)
        retRecord = result.to_json(orient='records')        
        #print("response to be sent is ",retRecord)
        return JsonResponse({'result':retRecord})
    
