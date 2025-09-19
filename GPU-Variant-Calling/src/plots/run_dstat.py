#!/usr/bin/env python3
import sys
import subprocess
import pandas as pd
import matplotlib.pyplot as plt

def main():
    if len(sys.argv) < 2:
        usage(sys.argv[0])
        sys.exit(2)

    command = sys.argv[1]
    num_hosts = int(sys.argv[2])
    output_file = "/home/ubuntu/report.csv"  # Replace 'username' with your actual username
    screen_name = "MYDSTAT"
    report_name = "-report.out"
    time_interval = "30"

    print("Num hosts:", num_hosts)
    if command == "start":
        for i in range(1, num_hosts):
            run_cmd = f"ssh vm{i} rm -rf {output_file}"
            subprocess.call(run_cmd, shell=True)

            run_cmd = (
                f"ssh vm{i} 'screen -dmS {screen_name} bash -c "
                f"\"dstat -l -d -n -m --noupdate {time_interval} > {output_file}\"'"
            )
            print(run_cmd)
            subprocess.call(run_cmd, shell=True)

    elif command == "stop":
        for i in range(1, num_hosts):
            run_cmd = f"ssh vm{i} screen -S {screen_name} -X quit"
            print(run_cmd)
            subprocess.call(run_cmd, shell=True)

    elif command == "collect":
        for i in range(1, num_hosts):
            run_cmd = f"scp vm{i}:{output_file} vm{i}{report_name}"
            print(run_cmd)
            subprocess.call(run_cmd, shell=True)

    elif command == "plot":
        if len(sys.argv) < 4:
            usage(sys.argv[0])
            sys.exit(2)
        xmax = -1
        if len(sys.argv) == 5:
            xmax = int(sys.argv[4])

        plt_attr = sys.argv[3]
        plt.clf()
        plt.rcParams.update({"font.size": 18})
        plt.figure(figsize=(8, 5))
        for i in range(1, num_hosts + 1):
            try:
                subprocess.call(f"sed -i 's/|/ /g' ./vm{i}{report_name}", shell=True)
                df = pd.read_csv(
                    f"vm{i}{report_name}", skiprows=1, sep=r"\s+", on_bad_lines="warn"
                )
                if xmax > -1:
                    df.drop(df.index[xmax:], inplace=True)

                print(df)
                print(list(df.columns))
                x = df.index
                print(x)
                y = df[plt_attr]
                plt.plot(x, y, label=f"vm{i}")
            except FileNotFoundError:
                print(f"File vm{i}{report_name} not found")

        plt.xlabel("Time Steps (in 30 sec intervals)")
        plt.ylabel(f"{plt_attr}")
        plt.ylim(0, 40)
        plt.legend(
            loc="upper center",
            prop={"size": 16},
            labelspacing=0.0,
            columnspacing=0.5,
            handlelength=0.25,
            bbox_to_anchor=(0.44, 1.22),
            ncol=8,
        )
        plt.savefig(f"plot-{plt_attr}.png", bbox_inches="tight")
    else:
        print("Unsupported command")
        usage(sys.argv[0])

def usage(prog_name):
    print(f"python3 {prog_name} <command> <num_nodes> [attr_name]")
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

