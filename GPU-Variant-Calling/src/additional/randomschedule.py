import matplotlib.pyplot as plt
import pandas as pd
import random

def read_data():
    df_with_sample = pd.read_csv("./single_data.csv",sep="\t")
    exp_list = df_with_sample["Sample"].values.tolist()
    df_out_sample = df_with_sample.drop(columns=["Sample"])
    temp = df_out_sample.values.tolist()
    output_list = [[(item, index) for index, item in enumerate(sublist)] for sublist in temp]
    return output_list, exp_list

jobs, exp_list = read_data()

def call_random_schedule():
    # Initialize machine finish times
    machine_finish_times = {i: 0 for i in range(5)}  # {machine_id: finish_time}
    machine_jobs = {i: [] for i in range(5)}  # {machine_id: [(start_time, end_time), ...]}
    for i, job_list in enumerate(jobs):
        time, machine_id = random.choice(job_list)
        exp_id = exp_list[i]
        print(f"{exp_id} on machine {machine_id} for {time} seconds.")
        start_time = machine_finish_times[machine_id]
        end_time = start_time + time
        machine_jobs[machine_id].append((start_time, end_time))
        machine_finish_times[machine_id] = end_time
    # The makespan is the maximum machine finish time
    makespan = max(machine_finish_times.values())
    print(f"The makespan is {makespan}")
    return machine_jobs

machine_jobs = call_random_schedule()

# Plotting
fig, ax = plt.subplots()
for machine_id, jobs in machine_jobs.items():
    for start_time, end_time in jobs:
        ax.broken_barh([(start_time, end_time - start_time)], (machine_id - 0.4, 0.8))
ax.set_xlabel('Time')
ax.set_ylabel('Machine ID')
ax.grid(True)
plt.show()
