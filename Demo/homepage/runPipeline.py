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


def pipelinerun():
    
    cmd = '/users/shared/AVAH-FABRIC/scripts/run_variant_analysis_at_scale.sh -i /proj/eva-public-PG0/main.txt -d NONE -n 8 -b 2 -p 2 -P H -G'
    host = 'c220g1-031114.wisc.cloudlab.us' 
    username = 'shared'  
    ssh_args = ['ssh', '-o', 'StrictHostKeyChecking=no', '-o', 'UserKnownHostsFile=/dev/null',f'{username}@{host}',cmd] 
    command_string = ' '.join(ssh_args)
    ret = subprocess.run(command_string, capture_output=True,shell = True,executable='/bin/bash',text=True,check=True) 
    ret = ret.stdout
    print("avah ret is ",ret)
    print("im here after the command")      