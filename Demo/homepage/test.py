import subprocess

def main():
    # Call the script.py as a separate process
    file = 'someFile.txt'
    file = '/Users/khawar/Downloads/history.txt'
    cmd = ["scp", "ks9dw@clnode003.clemson.cloudlab.us:/mydata/test.txt", "."]
    cmd = "scp ks9dw@clnode003.clemson.cloudlab.us:/mydata/test.txt /Users/khawar/demoWebsite/homepage/"
    
    #subprocess.run(cmd, capture_output=True, text=True, check=True, shell = True)
    #process = subprocess.run(['python3', 'drive_upload.py',file], capture_output=True, text=True)

    # Get the standard output of the completed process (the link printed by script.py)
    #link = process.stdout.strip()

    #print("Link from script:", link)
    
    
    print("im operating from directory path: +",subprocess.run("pwd"))
    
    try:
        #result = subprocess.run(cmd, capture_output=True, text=True, check=True, shell = True)
        link = subprocess.run("python3 /Users/khawar/demoWebsite/drive_upload.py /Users/khawar/demoWebsite/test.txt")
    except subprocess.CalledProcessError as e:
        print("Command returned non-zero exit status:", e.returncode)

if __name__ == "__main__":
    main()