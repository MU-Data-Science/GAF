import pandas as pd
import random
import heapq
# Initialize machine finish times
machine_finish_times = [(0, i) for i in range(5)]  # (finish_time, machine_id)
# Turn machine_finish_times into a heap
heapq.heapify(machine_finish_times)

df = pd.read_csv("./split_data.csv")
sample = df["Sample"]
df = df.drop(columns=["Sample"])
data = df.values.tolist()
jobs = [[[(data[i][j], j//2) for j in range(0, len(data[i]), 2)], [(data[i][j], j//2) for j in range(1, len(data[i]), 2)]] for i in range(len(data))]

# Create a dictionary that maps job numbers to sample numbers
job_to_sample = {i: sample_num for i, sample_num in enumerate(sample)}

for job_batch_index, job_batch in enumerate(jobs):
    for job_list_index, job_list in enumerate(job_batch):
        job = random.choice(job_list)
        time, machine_id = job
        # Get the corresponding sample number
        sample_num = job_to_sample[job_batch_index]
        print(f"{sample_num}_{job_list_index} on machine {machine_id} for {time} seconds.")
        # print(f"Running task {job_list_index} of sample {sample_num} on machine {machine_id} for {time} seconds.")
        # Here you would add the code to actually run the job on the machine.
        # This could be a function call like: run_job_on_machine(time, machine_id)
        # Update the finish time for the selected machine
        for i in range(len(machine_finish_times)):
            if machine_finish_times[i][1] == machine_id:
                machine_finish_times[i] = (machine_finish_times[i][0] + time, machine_id)
                break
        # Re-heapify the machine_finish_times list
        heapq.heapify(machine_finish_times)
# The makespan is the maximum machine finish time
makespan = max(machine_finish_times)[0]

print(f"The makespan is {makespan}")
