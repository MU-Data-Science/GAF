#!/usr/bin/env bash
/mydata/Anaconda3/bin/python3 ${HOME}/GAF/FABRIC/scripts/run_remote_command.py grep 8 "Completed" /mydata/hadoop/logs/userlogs/ | grep -o "Completed-[^/]*"
