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
    # earliest version by dr rao
    # elif (command=="plot"):
    #     if (len(sys.argv) < 4):
    #         usage(sys.argv[0])
    #         sys.exit(2)

    #     #print("argv[6]: ", int(sys.argv[5]))
    #     xmax = -1
    #     if (len(sys.argv) >= 5):
    #         xmax = int(sys.argv[4])

    #     arrowLocation = 0
    #     if (len(sys.argv) == 6):
    #         print(sys.argv[5])
    #         arrowLocation = int(sys.argv[5])

    #     plt_attr = sys.argv[3]
    #     plt.clf()
    #     #plt.rc('text', usetex=True)
    #     #plt.rc('font', family='monospace')
    #     plt.rcParams.update({'font.size': 18})
    #     plt.figure(figsize=(8,5))
    #     print("xmax: {} arrowlocation: {}".format(xmax, arrowLocation))

    #     # http://www.openkb.info/2021/03/how-to-monitor-nvidia-gpu-performance.html
    #     for i in range(1, num_hosts):
    #         try:
    #             df = pd.read_csv("vm{}{}".format(i, report_name), delimiter=r"\s+", comment='#')
    #             #print(df)
    #             #print(df.columns)
    #             if (xmax > -1):
    #                 df.drop(df.index[xmax:], inplace=True)
    #             x = df.index
    #             y = df[plt_attr]
    #             marker='x'
    #             if i % 3 == 0:
    #                 marker = 's'
    #             elif i % 3 == 1:
    #                 marker = '<'
    #             plt.plot(x, y, marker, alpha=0.25, label="vm{}".format(i), markersize=2)
    #         except FileNotFoundError:
    #             print("File vm{}{} not found".format(i, report_name))

    #     plt.xlabel("Time (sec)")
    #     plt.ylabel("{}".format("GPU utilization (%)"))
    #     plt.legend(loc='upper center', prop={"size": 16}, markerscale=4, labelspacing=0.0, columnspacing=0.5, handlelength=0.25, bbox_to_anchor=(0.48, 1.15), ncol=15)
    #     font2 = {'family':'serif','color':'black','size':16}
    #     #plt.text(arrowLocation, 50, 'Stage 3 completed', fontdict=font2)
    #     #plt.arrow(arrowLocation, 40, 2000, 0, head_width=2, head_length=1, fc="k", ec="k")
    #     ax = plt.gcf().gca()
    #     # ax.annotate("Stage 3 done", xy=(arrowLocation,50), xytext=(arrowLocation+4000, 50), arrowprops={'arrowstyle': '-|>', 'lw': 1})
    #     plt.savefig("plot-{}.png".format(plt_attr), bbox_inches='tight')
    #     #plt.show()
    # else:
    #     print("Unsupported command")
    #     usage(sys.argv[0])
    # for each vms splitted with blue gradient
    # elif (command == "plot"):
    #     if len(sys.argv) < 4:
    #         usage(sys.argv[0])
    #         sys.exit(2)

    #     xmax = -1
    #     if len(sys.argv) >= 5:
    #         xmax = int(sys.argv[4])

    #     arrowLocation = 0
    #     if len(sys.argv) == 6:
    #         print(sys.argv[5])
    #         arrowLocation = int(sys.argv[5])

    #     plt_attr = sys.argv[3]
    #     plt.clf()
    #     plt.rcParams.update({'font.size': 14})
    #     fig, axes = plt.subplots(num_hosts - 1, 1, figsize=(10, 6), sharex=True, sharey=True)
    #     # fig.suptitle("GPU Utilization Across VMs", fontsize=16)

    #     print("xmax: {} arrowlocation: {}".format(xmax, arrowLocation))

    #     for i in range(1, num_hosts):
    #         try:
    #             df = pd.read_csv("vm{}{}".format(i, report_name), delimiter=r"\s+", comment='#')
    #             if xmax > -1:
    #                 df.drop(df.index[xmax:], inplace=True)
    #             #remove all rows where gpu value is 0
    #             df = df[df['gpu'] ==  0]
    #             x = df.index
    #             y = df[plt_attr]

    #             axes[i - 1].plot(x, y, label="VM{}".format(i), alpha=0.75)
    #             axes[i - 1].set_ylabel("Utilization (%)")
    #             axes[i - 1].legend(loc='upper right')
    #             axes[i - 1].grid(True)
    #         except FileNotFoundError:
    #             print("File vm{}{} not found".format(i, report_name))

    #     axes[-1].set_xlabel("Time (sec)")
    #     plt.tight_layout(rect=[0, 0, 1, 0.95])
    #     plt.savefig("plot-{}.png".format(plt_attr), bbox_inches='tight')
        # plt.show()
    #splitted colorul
    elif (command == "plot"):
        if (len(sys.argv) < 4):
            usage(sys.argv[0])
            sys.exit(2)

        xmax = -1
        if (len(sys.argv) >= 5):
            xmax = int(sys.argv[4])

        arrowLocation = 0
        if (len(sys.argv) == 6):
            arrowLocation = int(sys.argv[5])

        plt_attr = sys.argv[3]
        plt.clf()
        plt.rcParams.update({'font.size': 30})

        # Create subplots for each VM
        fig, axes = plt.subplots(nrows=num_hosts - 1, ncols=1, figsize=(12, 6 * (num_hosts - 1)), sharex=True)
        # fig.suptitle("GPU Utilization for Each VM", fontsize=18)

        colors = plt.cm.tab10.colors  # Use a colormap for distinct colors

        print("xmax: {} arrowlocation: {}".format(xmax, arrowLocation))

        for i in range(1, num_hosts):
            try:
                # Load data for each VM
                df = pd.read_csv(f"vm{i}{report_name}", delimiter=r"\s+", comment='#')
                if xmax > -1:
                    df.drop(df.index[xmax:], inplace=True)
                df = df[df["gpu"]==0]
                df=df.reset_index()
                x = df.index
                y = df[plt_attr]

                # Smooth the data using a rolling average for better visualization
                y_smooth = pd.Series(y).rolling(window=10).mean()

                # Plot data for each VM in its respective subplot
                ax = axes[i - 1] if num_hosts > 2 else axes  # Handle single subplot case
                ax.plot(x, y_smooth, color=colors[i % len(colors)], alpha=0.8, linewidth=2, label=f"vm{i}")
                # ax.set_title(f"VM{i}", fontsize=16)
                ax.set_ylabel("GPU utilization (%)", fontsize=35)
                ax.grid(True, linestyle='--', alpha=0.6)
                ax.legend(loc='upper right', fontsize=30)

                # Annotate the last point for each VM
                # ax.annotate(f"{y_smooth.iloc[-1]:.2f}%", xy=(x[-1], y_smooth.iloc[-1]),
                #             xytext=(x[-1] + 5, y_smooth.iloc[-1]),
                #             fontsize=12, color=colors[i % len(colors)],
                #             arrowprops=dict(arrowstyle="->", color=colors[i % len(colors)]))
                # Set x-axis limit to stop at 16000
                ax.set_xlim(0, 30000)
                ax.set_ylim(0,100)
                print(df.shape)
            except FileNotFoundError:
                print(f"File vm{i}{report_name} not found")

        # Add shared X-axis label
        fig.text(0.5, 0.01, "Time (in seconds)", ha='center', fontsize=35)
        plt.tight_layout(rect=[0, 0.03, 1, 0.95])  # Adjust layout to fit title
        plt.savefig(f"plot-gpu.png", bbox_inches='tight')
        # Uncomment the line below to display the plot interactively
        # plt.show()
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
