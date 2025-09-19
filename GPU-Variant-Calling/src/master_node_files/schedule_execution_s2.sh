for vm in $(seq 1 $1); do
    screen -dmS vm${vm}_session bash -c "
    ssh vm${vm} << 'EOF'
    start=\$(date +%s)
    cd /mydata/GPU-Variant-Calling/
    cat ./schedule_m${vm}.txt
    ./schedule_execution_s2.sh ./schedule_m${vm}.txt
    end=\$(date +%s)
    duration=\$((end - start))
    echo \"Total Time: \$duration\" >> ./logs/times_\$HOSTNAME.log
    EOF
    "
done
