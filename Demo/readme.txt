using pipenv for venv. 
pipfile.lock contains the env details. 

Here's how to use on new system/instance:
1- install pipenv 
2- cd to GAF/Demo 
3- install python 3.8 and dependencies following :
    sudo apt update
    sudo apt install software-properties-common
    sudo add-apt-repository ppa:deadsnakes/ppa
    sudo apt update
    sudo apt install python3.8
    sudo apt-get install -y build-essential libssl-dev libffi-dev python3-dev
    pip install --upgrade pip setuptools wheel
4- pipenv shell 
5- pip install backports.zoneinfo
6- pipenv install -r requirements.txt

goto settings.py and add the server ip to allowed hosts. 
example run : python3 manage.py runserver 107.20.128.88:8000



how to activate? cd to this directory and then `pipenv shell`
link to yt : https://www.youtube.com/watch?v=rHux0gMZ3Eg&t=712s

source /Users/khawar/.local/share/virtualenvs/demoWebsite-9snWIvmp/bin/activate


drive upload error on large file :
pip install httplib2==0.15.0
pip install google-api-python-client==1.6

//better than cron
Continuous logs grep and vcf check/upload:

python3 manage.py shell 
from homepage.checkFiles import checkFiles 
while True:
	checkFiles()

