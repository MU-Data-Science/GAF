import os
import re 
import subprocess

#kill all screen sessions 
#screen -ls | grep -o '^\s*[0-9]\+\.[^\t]*' | awk '{print $1}' | xargs -I{} screen -X -S {} quit m

#changing working directory with subprocess doesnt work 
cmd = "cd /mydata"
subprocess.run(cmd, shell=True, capture_output=True, text=True)

os.chdir("/mydata")

print('Enter OAuth 2 Token:')
token = str(input())
print()

##download and unzip fastq download scripts
print("Downloading scripts..")
file_id = "1sEm8i9CuXcyepYqnFYw4TklluOKIdl3h" #1sEm8i9CuXcyepYqnFYw4TklluOKIdl3h
output_file = "fqDownloadScripts.zip"
cmd = 'curl -H "Authorization: Bearer %s" https://www.googleapis.com/drive/v3/files/%s?alt=media -o %s'%(token,file_id,output_file)
subprocess.run(cmd, shell=True, capture_output=True, text=True)
print("Unzipping..")

os.chdir("/mydata")

cmd = "unzip -o /mydata/fqDownloadScripts.zip"
subprocess.run(cmd, shell=True, capture_output=True, text=True)

##update tokens 
print("Updating auth tokens..")
files = ["fq1","fq2","fq3","fq4","fq5","fq6","fq7","fq8"]
for file in files:
    target_file = "/mydata/fastqDownloadScripts/%s download.py"%file
    with open(target_file, "r") as file:
        content = file.read()
        
    mod = re.sub(r'token = ".+"', 'token = "{}"'.format(token), content)

    with open(target_file, "w") as file:
        file.write(mod)


#create folder in vm0 and copy fq1 in mydata
if not os.path.exists("/mydata/fastqFiles"):
    subprocess.run('mkdir /mydata/fastqFiles', shell=True, capture_output=True, text=True)
cmd = "cp /mydata/fastqDownloadScripts/fq1\ download.py ."    
subprocess.run(cmd, shell=True, capture_output=True, text=True)

#copy files to nodes and make fastqFiles folder to download fastq files 
for i in range(1,8):
    print("copying download script on vm%s"%i)
    cmd = "scp /mydata/fastqDownloadScripts/fq%s\ download.py ssh vm%s:/mydata"%(i+1,i)
    #print(bash_command)
    subprocess.run(cmd, shell=True, capture_output=True, text=True)
    cmd = "ssh vm%s mkdir /mydata/fastqFiles"%i
    subprocess.run(cmd, shell=True, capture_output=True, text=True)
    print("making fastqFiles directory on vm%s"%i)
    print("")
        
    
print("")
print("")

#open screen sessions and start download 
print("Starting downloads...")
print()
for i in range(0,8):
    cmd = """ssh vm%s "screen -dm bash -c 'python3 /mydata/fq%s\ download.py; exec sh' " """%(i,i+1)
    print("opening screen on vm%s"%i)
    subprocess.run(cmd, shell=True, capture_output=True, text=True)
    

    

    
    
    
