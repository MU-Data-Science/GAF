#!/bin/bash

printf "Starting cpu monitoring\n"

# Ensure sar keeps running indefinitely
while true; do
    sar -u 30 >> /mydata/cpu_usageFinal.log
done &   

SAR_PID=$!   
printf "cpu util job id is : "
echo $SAR_PID
printf "\n"
