You can try generating an Outh token using the following the steps. It's needed for downloading fastq files from Google Drive 
</br>
• Go to OAuth 2.0 Playground `https:developers.google.com/oauthplayground/`   </br>
• In the Select the Scope box, paste https://www.googleapis.com/auth/drive.readonly  </br>
• Click Authorize APIs and then Exchange authorization code for tokens   </br>
• Copy the Access token
 
</br>
</br>
</br>

We are using Google API to download fastq files from Google Drive. There are 3 datasets that can be downloaded. 
1. Entire Dataset for germline pipeline consisting of 650ish GB 
2. Dataset for somatic variant calling pipeline
3. Sampled germline dataset for Demo app (around 54 files)
</br> 

Instructions to download for each dataset are </br> 

1. :exclamation: Following method assumes you have atleast 8 node cluster to utilise goole drive api for fastq files download :exclamation: <br> 
  - In a borwser window, Go to OAuth 2.0 Playground  `developers.google.com/oauthplayground/`
  - In the "Select the Scope" box, paste  `https://www.googleapis.com/auth/drive.readonly` and press enter 
  - Click Authorize APIs and then `Exchange authorization code for tokens` button and copy the `Acces token`
    ![Screenshot 2023-11-29 at 11 22 41 AM](https://github.com/MU-Data-Science/GAF/assets/22073166/1f41c846-08f2-432b-b7cf-2c9169561f02)
  - On FABRIC vm0 (master node) `cd ${HOME}/GAF/FABRIC/Google Drive Experimental Codes/`
  - run `fastqDownloads.py` to start downloading fastq files on 8 nodes using `python3 ./fastqDownloads.py` 
  - Running this script will ask to input the auth token. Paste the token copied in previous step and hit enter
  - Script should start downloading fastqFiles on 8 nodes in screen sessions. Downloaded fastq files will be placed under `/mydata/fastqFiles` on vm0 till vm7. 
  - Once all downloads are complete, on each node run `hdfs dfs -put /mydata/fastqFiles/*fastq.gz /`
  - Or if you want to move downloaded fastq files from all the VMs to VM0, in a screen run this command `for i in {0..7}; do scp vm$i:/mydata/fastqFiles/* /mydata/genomes ;done`. It will copy the downladed fastq files from all the vms to vm0:/mydata/genomes/
</br> </br> 

:exclamation: Datasets 2 and 3 have sample and much simpler instructions. :exclamation: 
`downloadSomaticFiles.py` & `downloadDemoFastqs.py` present in this directory, download the somatic files and files that are needed for demo (24ish files). </br>
For demo files, which are a small sample from our original 650GB dataset (1) , they need to be placed at the following location on each cluster at `/mydata/genomes/` </br></br> 

note: </br>
This directory needs to be manually created on master node of each cluster only. Moreover make another renamed directory inside `/mydata/genomes/`  that django will use to create renamed files. </br>
Hence in total we need to make the following directory structure on all clusters that django/demo will be using :
```
/mydata/genomes/
/mydata/genomes/renamed 
```
Initiate the download from inside the directory using this command : `python3 ~/GAF/FABRIC/Google\ Drive\ Experimental\ Codes/downloadDemoFastqs.py`


