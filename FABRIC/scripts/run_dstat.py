#!/usr/bin/env python3
#
import sys
import getopt
import subprocess
import time
import pandas as pd
import matplotlib.pyplot as plt

def main():
    if (len(sys.argv) < 2):
        usage(sys.argv[0])
        sys.exit(2)

    command = sys.argv[1]
    num_hosts = int(sys.argv[2])
    output_file = "/mydata/report.csv"
    screen_name = "MYDSTAT"
    report_name = "-report.out"
    time_interval = "30"

    print("Num hosts", num_hosts)
    if (command=="start"):
        for i in range(1, num_hosts):
            run_cmd = "ssh vm{} rm -rf {}".format(i, output_file)
            run_ret = subprocess.call(run_cmd, shell=True)
            print(run_cmd)
            run_cmd = "ssh vm{} screen -dmS {} dstat -t -l -d -n -m --noupdate --output {} {}".\
                format(i, screen_name, output_file, time_interval)
            print(run_cmd)
            run_ret = subprocess.call(run_cmd, shell=True)
    elif (command=="stop"):
        for i in range(1, num_hosts):
            run_cmd = "ssh vm{} screen -S {} -X quit".format(i, screen_name)
            print(run_cmd)
            run_ret = subprocess.call(run_cmd, shell=True)
    elif (command=="collect"):
        for i in range(1, num_hosts):
            run_cmd = "scp vm{}{}{} vm{}{}".format(i, ":", output_file, i, report_name)
            print(run_cmd)
            run_ret = subprocess.call(run_cmd, shell=True)
    elif (command=="plot"):
        if (len(sys.argv) < 4):
            usage(sys.argv[0])
            sys.exit(2)

        plt_attr = sys.argv[3]

        for i in range(1, num_hosts):
            try:
                df = pd.read_csv("vm{}{}".format(i, report_name), skiprows=5)
                #print(df)
                #x = df['time']
                x = df.index
                y = df[plt_attr]
                #plt.fill_between(x, y, label="vm{}".format(i))
                plt.plot(x, y, label="vm{}".format(i))
            except FileNotFoundError:
                print("File vm{}{} not found".format(i, report_name))

        plt.xlabel("Time")
        plt.ylabel("{}".format(plt_attr))
        plt.legend(loc='upper center', prop={"size": 8}, handlelength=0.5, bbox_to_anchor=(0.5, 1.15), ncol=15)
        plt.savefig("plot-{}.png".format(plt_attr))
        plt.show()
    else:
        print("Unsupported command")
        usage(sys.argv[0])

def usage(prog_name):
    print("python3 {} <command> <num_nodes> [attr_name]".format(prog_name))
    print("")
    print(" Commands:")
    print(" start    - start dstat on all nodes")
    print(" stop     - stop dstat on all nodes")
    print(" collect  - get the reports from all nodes")
    print(" plot     - plot the data")
    print("")
    print(" Required:")
    print(" num_nodes - cluster size")
    print("")
    print(" Optional:")
    print(" attr_name - attribute name for plot command (1m, 5m, read, etc.)")

if __name__ == "__main__":
    main()
    print("👉 Done!")
