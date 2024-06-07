import subprocess
import time 
from homepage.views import cronFunc


def checkFiles():
    try:   
        print()
        print("checking logs and vcfs")
        cronFunc()
        print("going to sleep")
        print()
        time.sleep(100)
        
    
        
    except subprocess.CalledProcessError as e:
        print("Command returned non-zero exit status:", e.returncode)
        print("Error :", e.stderr)
    