#!/usr/bin/env python3
#
import sys
import getopt
import subprocess
import time
import pandas as pd
import matplotlib.pyplot as plt

def main():

    if (len(sys.argv) < 3):
        usage(sys.argv[0])
        sys.exit(2)

    command = sys.argv[1]
    num_nodes = int(sys.argv[2])

    if (command == "grep"):
        pattern = sys.argv[3]
        log_dir = sys.argv[4]
        for i in range(1, num_nodes):
            print("================ vm{} ================".format(i))
            run_cmd = "ssh vm{} grep -R {} {}".format(i, pattern, log_dir)
            run_ret = subprocess.call(run_cmd, shell=True)
    elif (command == "copy"):
        file = sys.argv[3]
        target_dir = sys.argv[4]
        for i in range(1, num_nodes):
            print("================ vm{} ================".format(i))
            run_cmd = "scp {} vm{}:{}".format(file, i, target_dir)
            run_ret = subprocess.call(run_cmd, shell=True)
    elif (command == "set_mtu"):
        mtu_value = sys.argv[3]
        for i in range(0, num_nodes):
            print("================ vm{} ================".format(i))

            ip_addr_cmd = ''' ssh vm{} bash -c "'grep 'vm{}-lan' /etc/hosts | cut -f 1'" '''.format(i, i)

            with open('temp.txt', 'w+') as fout:
                run_ret = subprocess.call(ip_addr_cmd, shell=True, stdout=fout)
                fout.seek(0)
                ip_addr = fout.read().strip()

            print("Host IP address: ", ip_addr)

            interface_cmd = ''' ssh vm{} bash -c "'ip r | grep {} | cut -f 3 -d \\" \\" '"  '''.format(i, ip_addr)

            with open('temp.txt', 'w+') as fout:
                run_ret = subprocess.call(interface_cmd, shell=True, stdout=fout)
                fout.seek(0)
                interface_name = fout.read().strip()

            print("Host interface name: ", interface_name)

            run_cmd = "ssh vm{} sudo ip link set {} mtu {}".format(i, interface_name, mtu_value)
            run_ret = subprocess.call(run_cmd, shell=True)
    elif (command == "set_tcp_win"):
        max_win_size = sys.argv[3]
        default_size = sys.argv[4]
        for i in range(0, num_nodes):
            print("================ vm{} ================".format(i))
            run_cmd = "ssh vm{} sudo sysctl -w " \
                        "net.core.rmem_max={} net.core.wmem_max={} " \
                        "net.ipv4.tcp_rmem=\"4096\ {}\ {}\" net.ipv4.tcp_wmem=\"4096\ {}\ {}\" "\
                        "net.ipv4.route.flush=1" \
                        .format(i, max_win_size, max_win_size, default_size, max_win_size,
                                default_size, max_win_size)
            print(run_cmd)            
            run_ret = subprocess.call(run_cmd, shell=True)
    elif (command == "gpu_usage"):
        cmd = sys.argv[3]

        screen_name = "GPU_USAGE"
        datadir="/mydata"
        for i in range(0, num_nodes):
            print("================ vm{} ================".format(i))
            if (cmd == "start"):
                interval = sys.argv[4]
                run_cmd = "ssh vm{} screen -dmS {} {}/check_gpu_usage.sh {}".\
                    format(i, screen_name, datadir, interval)
            elif (cmd == "stop"):
                run_cmd = "ssh vm{} screen -S {} -X quit".\
                    format(i, screen_name)
            else:
                print("Unsupported command: {}".format(run_cmd))
                sys.exit(1)
            run_ret = subprocess.call(run_cmd, shell=True)
    elif (command == "docker"):
        cmd = sys.argv[3]
        for i in range(0, num_nodes):
            print("================ vm{} ================".format(i))
            if (cmd == "kill"):
                run_cmd = ''' ssh vm{} bash -c "'sudo docker kill \$(sudo docker ps -q)'" '''.format(i)
            elif (cmd == "list"):
                run_cmd = ''' ssh vm{} bash -c "'sudo docker stats --no-stream'" '''.format(i)
            else:
                print("Unsupported command: {}".format(run_cmd))
                sys.exit(1)
            run_ret = subprocess.call(run_cmd, shell=True)
    else:
        print("Unsupported command")
        usage(sys.argv[0])
        sys.exit(2)

def usage(prog_name):
    print("python3 {} <command> <num_nodes> <arg1> <arg2>".format(prog_name))
    print("")
    print(" Commands:")
    print(" copy    - copy a file to all worker nodes")
    print("     <arg1>   - file to copy")
    print("     <arg2>   - target directory")
    print("")
    print(" grep    - grep log files of all worker nodes")
    print("     <arg1>   - pattern")
    print("     <arg2>   - log directory")
    print("")
    print(" set_mtu - set MTU value on all nodes")
    print("     <arg1>   - MTU value (e.g., 9000)")
    print("")
    print(" set_tcp_win - set TCP window size on all nodes")
    print("     <arg1>   - max. window size")
    print("     <arg2>   - default size")
    print("")
    print(" gpu_usage - monitor gpu usage on all nodes")
    print("     <arg1>   - start | stop")
    print("     <arg2>   - interval")
    print(" docker - monitor docker containers on all nodes")
    print("     <arg1>   - kill | list")
    print("")

if __name__ == "__main__":
    main()
    print("👉 Done!")
