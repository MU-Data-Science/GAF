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
    output_file = "~/gpu-report.txt"
    screen_name = "MYGPUSTAT"
    #report_name = "-gpu-report.out"
    report_name = "-gpu-report.out"

    print("Num hosts", num_hosts)
    if (command=="start"):
        for i in range(1, num_hosts):
            run_cmd = "ssh vm{} rm -rf {}".format(i, output_file)
            run_ret = subprocess.call(run_cmd, shell=True)
            print(run_cmd)
            run_cmd = "ssh vm{} screen -dmS {} nvidia-smi dmon -d 1 -s pucvmet -o DT -f {}".\
                format(i, screen_name, output_file)
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

        #print("argv[6]: ", int(sys.argv[5]))
        xmax = -1
        if (len(sys.argv) >= 5):
            xmax = int(sys.argv[4])

        arrowLocation = 0
        if (len(sys.argv) == 6):
            print(sys.argv[5])
            arrowLocation = int(sys.argv[5])

        plt_attr = sys.argv[3]
        plt.clf()
        #plt.rc('text', usetex=True)
        #plt.rc('font', family='monospace')
        plt.rcParams.update({'font.size': 18})
        plt.figure(figsize=(8,5))
        print("xmax: {} arrowlocation: {}".format(xmax, arrowLocation))

        # http://www.openkb.info/2021/03/how-to-monitor-nvidia-gpu-performance.html
        for i in range(1, num_hosts):
            try:
                df = pd.read_csv("vm{}{}".format(i, report_name), delimiter=r"\s+", comment='#')
                #print(df)
                #print(df.columns)
                if (xmax > -1):
                    df.drop(df.index[xmax:], inplace=True)
                x = df.index
                y = df[plt_attr]
                marker='x'
                if i % 3 == 0:
                    marker = 's'
                elif i % 3 == 1:
                    marker = '<'
                plt.plot(x, y, marker, alpha=0.25, label="vm{}".format(i), markersize=2)
            except FileNotFoundError:
                print("File vm{}{} not found".format(i, report_name))

        plt.xlabel("Time (sec)")
        plt.ylabel("{}".format("GPU utilization (%)"))
        plt.legend(loc='upper center', prop={"size": 16}, markerscale=4, labelspacing=0.0, columnspacing=0.5, handlelength=0.25, bbox_to_anchor=(0.48, 1.15), ncol=15)
        font2 = {'family':'serif','color':'black','size':16}
        #plt.text(arrowLocation, 50, 'Stage 3 completed', fontdict=font2)
        #plt.arrow(arrowLocation, 40, 2000, 0, head_width=2, head_length=1, fc="k", ec="k")
        ax = plt.gcf().gca()
        ax.annotate("Stage 3 done", xy=(arrowLocation,50), xytext=(arrowLocation+4000, 50), arrowprops={'arrowstyle': '-|>', 'lw': 1})
        plt.savefig("plot-{}.png".format(plt_attr), bbox_inches='tight')
        #plt.show()
    else:
        print("Unsupported command")
        usage(sys.argv[0])

def usage(prog_name):
    print("python3 {} <command> <num_nodes>".format(prog_name))
    print("")
    print(" Commands:")
    print(" start    - start gpu monitoring on all nodes")
    print(" stop     - stop gpu monitoring on all nodes")
    print(" collect  - get the reports from all nodes")
    print(" plot     - plot the reports from all nodes")
    print("")
    print(" Required:")
    print(" num_nodes - cluster size")
    print("")

if __name__ == "__main__":
    main()
    print("👉 Done!")
