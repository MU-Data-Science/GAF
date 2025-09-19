for vm in {1..5}; do
    screen -dmS vm${vm}_session bash -c "
    ssh vm${vm} << 'EOF'
    start=\$(date +%s)
    cd /mydata/GPU-Variant-Calling/
    for i in \$(grep 'EXEC' ./schedule_m${vm}.txt | cut -d' ' -f2); do
        echo \$i
        ./src/parabricks_call_by_sample.sh -s \$i
    done
    end=\$(date +%s)
    duration=\$((end - start))
    echo \"Total Time: \$duration\" >> ./logs/times_\$HOSTNAME.log
    EOF
    "
done
