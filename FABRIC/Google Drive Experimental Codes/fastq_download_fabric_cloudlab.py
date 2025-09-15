import os
import re 
import subprocess

#kill all screen sessions 
#screen -ls | grep -o '^\s*[0-9]\+\.[^\t]*' | awk '{print $1}' | xargs -I{} screen -X -S {} quit 

#changing working directory with subprocess doesnt work 
cmd = "cd /gss"
subprocess.run(cmd, shell=True, capture_output=True, text=True)

os.chdir("/gss")

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

os.chdir("/gss")

cmd = "unzip -o /gss/fqDownloadScripts.zip"
subprocess.run(cmd, shell=True, capture_output=True, text=True)

##update tokens 
print("Updating auth tokens..")
files = ["fq1","fq2","fq3","fq4","fq5","fq6","fq7","fq8"]
for file in files:
    target_file = "/gss/fastqDownloadScripts/%s download.py"%file
    with open(target_file, "r") as file:
        content = file.read()
    # Update token
    mod = re.sub(r'token = ".+"', 'token = "{}"'.format(token), content)
    # Update os.chdir line if it points to /mydata/fastqFiles/
    mod = re.sub(r'os\.chdir\(["\"]/mydata/fastqFiles/["\"]\)', 'os.chdir("/gss/fastqFiles/")', mod)

    with open(target_file, "w") as file:
        file.write(mod)


#create folder in vm0 and copy fq1 in gss
if not os.path.exists("/gss/fastqFiles"):
    subprocess.run('mkdir /gss/fastqFiles', shell=True, capture_output=True, text=True)
cmd = "cp /gss/fastqDownloadScripts/fq1\ download.py ."    
subprocess.run(cmd, shell=True, capture_output=True, text=True)

#copy files to nodes and make fastqFiles folder to download fastq files 
for i in range(1,8):
    print("copying download script on vm%s"%i)
    cmd = "scp /gss/fastqDownloadScripts/fq%s\ download.py ssh vm%s:/gss"%(i+1,i)
    #print(bash_command)
    subprocess.run(cmd, shell=True, capture_output=True, text=True)
    print("")
        
    
print("")
print("")

#open screen sessions and start download 
print("Starting downloads...")
print()
for i in range(0,8):
    cmd = """ssh vm%s "screen -dm bash -c 'python3 /gss/fq%s\ download.py; exec sh' " """%(i,i+1)
    print("opening screen on vm%s"%i)
    subprocess.run(cmd, shell=True, capture_output=True, text=True)
    

    

    
    
    
