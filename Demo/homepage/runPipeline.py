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


def getSSH(cluster):
    cmd = ''
    #cluster = "cloudlab"
    if cluster == "cl1":
        #demo1
        host = 'clnode001.clemson.cloudlab.us'  
        username = 'shared'
        cmd = ""          
        ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',f'{username}@{host}',cmd]
    elif cluster == "cl2": 
        #demo2
        host = 'clnode029.clemson.cloudlab.us'  
        username = 'shared'
        cmd = ""          
        ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',f'{username}@{host}',cmd]
    elif cluster == "cl3":  
        #demo3
        host = 'clnode051.clemson.cloudlab.us'
        username = 'shared'
        cmd = ""          
        ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',f'{username}@{host}',cmd]
       
    return ssh_args

def pipelinerun():
    
    cluster = "cl1"
    
    cmd = '/users/shared/AVAH-FABRIC/scripts/run_variant_analysis_at_scale.sh -i /proj/eva-public-PG0/{}main.txt -d NONE -n 8 -b 2 -p 2 -P H -G'.format(cluster)
               
    ssh_args = getSSH(cluster)
    host = ssh_args[-2]
    ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',f'{host}',cmd] 
    command_string = ' '.join(ssh_args)
    
    ret = subprocess.run(command_string,capture_output=True, text=True, check=True,executable='/bin/bash', shell=True)
    ret = ret.stdout
    print("avah ret is ",ret)
    
    output = ret.stdout
    #err = ret.stderr
    print("out is ",output)
    

  