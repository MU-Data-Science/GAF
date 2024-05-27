using pipenv for venv. 
pipfile.lock contains the env details. 

Here's how to use on new system/instance:
1- install pipenv 
2- cd to GAF/Demo 
3- pipenv shell 
4- pipenv install -r requirements.txt


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

