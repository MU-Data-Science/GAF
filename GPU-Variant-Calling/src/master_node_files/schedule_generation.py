# Importing libraries
from ortools.sat.python import cp_model # type: ignore
import pandas as pd
import numpy as np 
import collections
import contextlib
import argparse
import logging
import warnings
import time
import sys
import io

# Configure logging - generates a schedule.log file
warnings.filterwarnings('ignore')
logger = logging.getLogger(__name__)
logger.info("Started Logging here!")
logging.basicConfig(filename='schedule.log',level=logging.INFO,format='%(message)s')

# Defining the function below
def parse_arguments():
    parser = argparse.ArgumentParser(description='Generate a schedule for a given input CSV file!')
    parser.add_argument('-a', '--algorithm', type=str, required=True, choices=['greedy2','greedy', 'jobshop'],
                        help='Algorithm to use: greedy or jobshop')
    parser.add_argument('-s', '--stage', type=str, required=True, choices=['stage1', 'stage2'], 
                        help='Stage to execute: stage1 or stage2')
    parser.add_argument('-m','--machines',type=int,required=True,help='Number of machines using')
    parser.add_argument('-b','--subset_size',type=int,required=False,help='Subset size for the algorithm')
    parser.add_argument('-sn','--subset_num',type=int,required=False,help='Subset Number for the algorithm')
    parser.add_argument('input_file', type=str, help='Input CSV file')
    return parser.parse_args()

class SolutionPrinter(cp_model.CpSolverSolutionCallback):
    """Print intermediate solutions."""

    def __init__(self):
        cp_model.CpSolverSolutionCallback.__init__(self)
        self.__solution_count = 0
        self.__start_time = time.time()

    def on_solution_callback(self):
        """Called at each new solution."""
        current_time = time.time()
        elapsed_time = current_time - self.__start_time
        print(
            "Solution %i, time = %f s, objective = %i"
            % (self.__solution_count, elapsed_time, self.ObjectiveValue())
        )
        
        self.__solution_count += 1
        if self.__solution_count == 50:
            self.StopSearch()
        
def greedy_stage_1(data_imported,machines):
    logging.info("Starting GreedyStage1 logs!")
    data = data_imported
    logging.info(f'Data (head) read from input.csv: {data.head()}')
    logging.info(f'Data shape: {data.shape}')
    data_dict = data.set_index('Run').T.to_dict('list')
    jobs = data_dict
    del data, data_dict
    def greedy_schedule(jobs, machines):
        execution_plan = {machine: [('BEGIN', 0)] for machine in machines}
        execution_time = {machine: 0 for machine in machines}
        execution_order = []
        J = list(jobs.keys())
        while J:
            T = []
            for job in J:
                for i, time in enumerate(jobs[job]):
                    T.append((job, machines[i], time))
            # Find the job-machine pair with the minimum time considering the current load
            J_alpha, m_beta, w = min(T, key=lambda x: (execution_time[x[1]], x[2]))
            # Append the execution to the plan
            execution_plan[m_beta].append((f'EXEC {J_alpha}', w))
            execution_time[m_beta] += w
            execution_order.append((J_alpha, m_beta, w))
            # Remove the job from the list
            J.remove(J_alpha)
        # Append 'END' to each machine's execution plan
        for machine in machines:
            execution_plan[machine].append(('END', 0))
            # print(execution_plan[machine])
        return execution_plan, execution_time, execution_order
    execution_plan, execution_time, execution_order = greedy_schedule(jobs, machines)
    # Print the results here
    for machine, plan in execution_plan.items():
        print(f"{machine}: {', '.join([f'{task} ({time} units)' for task, time in plan])}")
        logging.info(f"{machine}: {', '.join([f'{task} ({time} units)' for task, time in plan])}")
    print("\nExecution times:")
    logging.info("Execution times:")
    for machine, time in execution_time.items():
        print(f"{machine}: {time} units")
        logging.info(f"{machine}: {time} units")
    print("\nExecution order:")
    logging.info("Execution order:")
    for job, machine, time in execution_order:
        print(f"{job} executed on {machine} for {time} units")
        logging.info(f"{job} executed on {machine} for {time} units")
    # Write the results to files
    for machine, plan in execution_plan.items():
        # print(machine)
        with open(f'schedule_{machine}.txt', 'w') as file:
            for task, _ in plan:
                file.write(f"{task}\n")
    print("Execution plans have been written to files.")
    logging.info("Execution plans have been written to files.")
    
def new_greedy_stage_1(data_imported,machines):
    logging.info("Starting new Greedy Stage1 logs!")
    data = data_imported
    logging.info(f'Data (head) read from input.csv: {data.head()}')
    logging.info(f'Data shape: {data.shape}')
    data_dict = data.set_index('Run').T.to_dict('list')
    jobs = data_dict
    del data, data_dict
    def greedy2_schedule(jobs, machines):
        execution_plan = {machine: [('BEGIN', 0)] for machine in machines}
        execution_time = {machine: 0 for machine in machines}
        execution_order = []
        J = list(jobs.keys())
        while J:
            T = []
            for job in J:
                for i, time in enumerate(jobs[job]):
                    T.append((job, machines[i], time))
            # Find the job-machine pair with the minimum time considering the current load
            J_alpha, m_beta, w = min(T, key=lambda x: (execution_time[x[1]], x[2]))
            filtered_T = [entry for entry in T if entry[1] == m_beta]
            # print(f"Jobs on machine m_beta in T {m_beta}: {filtered_T}")
            filtered_T = max([entry for entry in T if entry[1] == m_beta],key=lambda x:x[2])
            # print(f"Max job {m_beta} on here",filtered_T)
            J_alpha, m_beta, w = filtered_T
            # print("Considering max job",J_alpha,m_beta,w,"\n")
            execution_plan[m_beta].append((f'EXEC {J_alpha}', w))
            execution_time[m_beta] += w 
            # print(w)
            execution_order.append((J_alpha, m_beta, w))
            J.remove(J_alpha)
        # Append 'END' to each machine's execution plan
        for machine in machines:
            execution_plan[machine].append(('END', 0))
            # print(execution_plan[machine])
        return execution_plan, execution_time, execution_order
    execution_plan, execution_time, execution_order = greedy2_schedule(jobs, machines)
    # Print the results here
    for machine, plan in execution_plan.items():
        print(f"{machine}: {', '.join([f'{task} ({time} units)' for task, time in plan])}")
        logging.info(f"{machine}: {', '.join([f'{task} ({time} units)' for task, time in plan])}")
    print("\nExecution times:")
    logging.info("Execution times:")
    for machine, time in execution_time.items():
        print(f"{machine}: {time} units")
        logging.info(f"{machine}: {time} units")
    print("\nExecution order:")
    logging.info("Execution order:")
    for job, machine, time in execution_order:
        print(f"{job} executed on {machine} for {time} units")
        logging.info(f"{job} executed on {machine} for {time} units")
    # Write the results to files
    for machine, plan in execution_plan.items():
        # print(machine)
        with open(f'schedule_{machine}.txt', 'w') as file:
            for task, _ in plan:
                file.write(f"{task}\n")
    print("Execution plans have been written to files.")
    logging.info("Execution plans have been written to files.")

def flexible_jobshop(data,machines):
    logging.info("Starting Flexible Jobshop logs!")
    """solve a small flexible jobshop problem."""
    #Provide data here
    data = data # you can change this to predicted
    data = data.rename(columns={"Run": "Sample"})
    run = data["Sample"].values.tolist()
    # make float to int
    values = data.iloc[:, 1:].astype(int)
    sample = data["Sample"].values.tolist()
    # merge values and sample
    data = pd.concat([pd.DataFrame(sample), values], axis=1)
    # rename the 0th column to Sample
    data = data.rename(columns={0: "Sample"})
    # print(run)
    logging.info(f'Data (head) read from input.csv: {data.head()}')
    def convert_data(data):
        data_list = data.iloc[:, 1:].values.tolist()
        output_list = [[[(item, index) for index, item in enumerate(sublist)]] for sublist in data_list]
        return output_list

    jobs = convert_data(data)
    num_jobs = len(jobs)
    all_jobs = range(num_jobs)
    num_machines = len(machines) # pass number of machines used 
    all_machines = range(num_machines)
    # Model the flexible jobshop problem.
    model = cp_model.CpModel()

    horizon = 0
    for job in jobs:
        for task in job:
            max_task_duration = 0
            for alternative in task:
                max_task_duration = max(max_task_duration, alternative[0])
            horizon += max_task_duration
            
    print("Horizon = %i" % horizon)
    logging.info(f'Horizon = {horizon}')

    # Global storage of variables.
    intervals_per_resources = collections.defaultdict(list)
    starts = {}  # indexed by (job_id, task_id).
    presences = {}  # indexed by (job_id, task_id, alt_id).
    job_ends = []

    # Scan the jobs and create the relevant variables and intervals.
    for job_id in all_jobs:
        job = jobs[job_id]
        num_tasks = len(job)
        previous_end = None
        for task_id in range(num_tasks):
            task = job[task_id]

            min_duration = task[0][0]
            max_duration = task[0][0]

            num_alternatives = len(task)
            all_alternatives = range(num_alternatives)

            for alt_id in range(1, num_alternatives):
                alt_duration = task[alt_id][0]
                min_duration = min(min_duration, alt_duration)
                max_duration = max(max_duration, alt_duration)

            # Create main interval for the task.
            suffix_name = "_j%i_t%i" % (job_id, task_id)
            start = model.NewIntVar(0, horizon, "start" + suffix_name)
            duration = model.NewIntVar(
                min_duration, max_duration, "duration" + suffix_name
            )
            end = model.NewIntVar(0, horizon, "end" + suffix_name)
            interval = model.NewIntervalVar(
                start, duration, end, "interval" + suffix_name
            )

            # Store the start for the solution.
            starts[(job_id, task_id)] = start

            # Add precedence with previous task in the same job.
            if previous_end is not None:
                model.Add(start >= previous_end)
            previous_end = end

            # Create alternative intervals.
            if num_alternatives > 1:
                l_presences = []
                for alt_id in all_alternatives:
                    alt_suffix = "_j%i_t%i_a%i" % (job_id, task_id, alt_id)
                    l_presence = model.NewBoolVar("presence" + alt_suffix)
                    l_start = model.NewIntVar(0, horizon, "start" + alt_suffix)
                    l_duration = task[alt_id][0]
                    l_end = model.NewIntVar(0, horizon, "end" + alt_suffix)
                    l_interval = model.NewOptionalIntervalVar(
                        l_start, l_duration, l_end, l_presence, "interval" + alt_suffix
                    )
                    l_presences.append(l_presence)

                    # Link the primary/global variables with the local ones.
                    model.Add(start == l_start).OnlyEnforceIf(l_presence)
                    model.Add(duration == l_duration).OnlyEnforceIf(l_presence)
                    model.Add(end == l_end).OnlyEnforceIf(l_presence)

                    # Add the local interval to the right machine.
                    intervals_per_resources[task[alt_id][1]].append(l_interval)

                    # Store the presences for the solution.
                    presences[(job_id, task_id, alt_id)] = l_presence

                # Select exactly one presence variable.
                model.AddExactlyOne(l_presences)
            else:
                intervals_per_resources[task[0][1]].append(interval)
                presences[(job_id, task_id, 0)] = model.NewConstant(1)

        job_ends.append(previous_end)

    # Create machines constraints.
    for machine_id in all_machines:
        intervals = intervals_per_resources[machine_id]
        if len(intervals) > 1:
            model.AddNoOverlap(intervals)

    # Makespan objective
    makespan = model.NewIntVar(0, horizon, "makespan")
    model.AddMaxEquality(makespan, job_ends)
    model.Minimize(makespan)
    solver = cp_model.CpSolver()
    solution_printer = SolutionPrinter()
    status = solver.solve(model, solution_printer)
    n = 0
    # Print final solution.
    if status in (cp_model.OPTIMAL, cp_model.FEASIBLE):
        print(f"Optimal objective value: {solver.objective_value}")
        for job_id in all_jobs:
            for task_id in range(len(jobs[job_id])):
                start_value = solver.Value(starts[(job_id, task_id)])
                machine = -1
                duration = -1
                selected = -1
                for alt_id in range(len(jobs[job_id][task_id])):
                    if solver.Value(presences[(job_id, task_id, alt_id)]):
                        duration = jobs[job_id][task_id][alt_id][0]
                        machine = jobs[job_id][task_id][alt_id][1]
                        selected = alt_id
                runner = run[n]
                print("%s_%i starts at %i (alt %i, machine %i, duration %i)" % (runner, job_id, start_value, selected, machine, duration))
            n = n + 1
        print("solve status: %s" % solver.StatusName(status))
        logging.info(f'solve status: {solver.StatusName(status)}')
        print("Optimal objective value: %i" % solver.ObjectiveValue())
        logging.info(f'Optimal objective value: {solver.ObjectiveValue()}')
        print("Statistics")
        print("  - conflicts : %i" % solver.NumConflicts())
        print("  - branches  : %i" % solver.NumBranches())
        print("  - wall time : %f s" % solver.WallTime())


def flexible_jobshop_2(temp_data,machines):
    df = temp_data
    
    # data = data # you can change this to predicted
    # data = data.rename(columns={"Run": "Sample"})
    # run = data["Sample"].values.tolist()
    # # make float to int
    # values = data.iloc[:, 1:].astype(int)
    # sample = data["Sample"].values.tolist()
    # # merge values and sample
    # data = pd.concat([pd.DataFrame(sample), values], axis=1)
    # # rename the 0th column to Sample
    # data = data.rename(columns={0: "Sample"})
    run= df[["Run"]];run = run["Run"].values.tolist()
    values = df.iloc[:, 1:].astype(int)
    data = values.values.tolist()
    jobs = [[[(data[i][j], j//2) for j in range(0, len(data[i]), 2)], [(data[i][j], j//2) 
                          for j in range(1, len(data[i]), 2)]] for i in range(len(data))]
    num_jobs = len(jobs)
    all_jobs = range(num_jobs)
    num_machines = len(machines)
    all_machines = range(num_machines)
    model = cp_model.CpModel()
    horizon = 0
    for job in jobs:
        for task in job:
            max_task_duration = 0
            for alternative in task:
                max_task_duration = max(max_task_duration, alternative[0])
            horizon += max_task_duration
    print("Horizon = %i" % horizon)
    logging.info(f'Horizon = {horizon}')
    # Global storage of variables.
    intervals_per_resources = collections.defaultdict(list)
    starts = {}  # indexed by (job_id, task_id).
    presences = {}  # indexed by (job_id, task_id, alt_id).
    job_ends = []
    # Scan the jobs and create the relevant variables and intervals.
    for job_id in all_jobs:
        job = jobs[job_id]
        num_tasks = len(job)
        previous_end = None
        for task_id in range(num_tasks):
            task = job[task_id]
            min_duration = task[0][0]
            max_duration = task[0][0]
            num_alternatives = len(task)
            all_alternatives = range(num_alternatives)
            for alt_id in range(1, num_alternatives):
                alt_duration = task[alt_id][0]
                min_duration = min(min_duration, alt_duration)
                max_duration = max(max_duration, alt_duration)
            # Create main interval for the task.
            suffix_name = "_j%i_t%i" % (job_id, task_id)
            start = model.NewIntVar(0, horizon, "start" + suffix_name)
            duration = model.NewIntVar(
                min_duration, max_duration, "duration" + suffix_name)
            end = model.NewIntVar(0, horizon, "end" + suffix_name)
            interval = model.NewIntervalVar(
                start, duration, end, "interval" + suffix_name)
            # Store the start for the solution.
            starts[(job_id, task_id)] = start
            # Add precedence with previous task in the same job.
            if previous_end is not None:
                model.Add(start >= previous_end)
            previous_end = end
            # Create alternative intervals.
            if num_alternatives > 1:
                l_presences = []
                for alt_id in all_alternatives:
                    alt_suffix = "_j%i_t%i_a%i" % (job_id, task_id, alt_id)
                    l_presence = model.NewBoolVar("presence" + alt_suffix)
                    l_start = model.NewIntVar(0, horizon, "start" + alt_suffix)
                    l_duration = task[alt_id][0]
                    l_end = model.NewIntVar(0, horizon, "end" + alt_suffix)
                    l_interval = model.NewOptionalIntervalVar(
                        l_start, l_duration, l_end, l_presence, "interval" + alt_suffix)
                    l_presences.append(l_presence)
                    # Link the primary/global variables with the local ones.
                    model.Add(start == l_start).OnlyEnforceIf(l_presence)
                    model.Add(duration == l_duration).OnlyEnforceIf(l_presence)
                    model.Add(end == l_end).OnlyEnforceIf(l_presence)
                    intervals_per_resources[task[alt_id][1]].append(l_interval)
                    # Store the presences for the solution.
                    presences[(job_id, task_id, alt_id)] = l_presence
                # Select exactly one presence variable.
                model.AddExactlyOne(l_presences)
            else:
                intervals_per_resources[task[0][1]].append(interval)
                presences[(job_id, task_id, 0)] = model.NewConstant(1)
        job_ends.append(previous_end)
    # Create machines constraints.
    for machine_id in all_machines:
        intervals = intervals_per_resources[machine_id]
        if len(intervals) > 1:
            model.AddNoOverlap(intervals)
    # Makespan objective
    makespan = model.NewIntVar(0, horizon, "makespan")
    model.AddMaxEquality(makespan, job_ends)
    model.Minimize(makespan)
    # Solve model
    solver = cp_model.CpSolver()
    solution_printer = SolutionPrinter()
    status = solver.solve(model, solution_printer)
    n = 0
    # Print final solution.
    if status in(cp_model.FEASIBLE,cp_model.OPTIMAL):
        print(f"Optimal objective value: {solver.objective_value}")
        logging.info(f'Optimal objective value: {solver.objective_value}')  
        for job_id in all_jobs:
            for task_id in range(len(jobs[job_id])):
                start_value = solver.Value(starts[(job_id, task_id)])
                machine = -1
                duration = -1
                selected = -1
                for alt_id in range(len(jobs[job_id][task_id])):
                    if solver.Value(presences[(job_id, task_id, alt_id)]):
                        duration = jobs[job_id][task_id][alt_id][0]
                        machine = jobs[job_id][task_id][alt_id][1]
                        selected = alt_id
                runner = run[n]
                print("%s_%i starts at %i (alt %i, machine %i, duration %i)" % (runner,task_id, start_value, selected, machine, duration))
                logging.info(f"{runner}_{task_id} starts at {start_value} (alt {selected}, machine {machine}, duration {duration})")
            n = n + 1
        print("solve status: %s" % solver.StatusName(status))
        logging.info(f'solve status: {solver.StatusName(status)}')
        print("Optimal objective value: %i" % solver.ObjectiveValue())
        logging.info(f'Optimal objective value: {solver.ObjectiveValue()}')
        print("Statistics")
        print("  - conflicts : %i" % solver.NumConflicts())
        print("  - branches  : %i" % solver.NumBranches())
        print("  - wall time : %f s" % solver.WallTime())

def flexible_jobshop_stage_1(data_imported,machines): 
    # flexible_jobshop(data_imported)
    # Redirect stdout to capture print statements
    output_capture = io.StringIO()
        # Redirect stdout to the StringIO object
    with contextlib.redirect_stdout(output_capture):
        flexible_jobshop(data_imported,machines)
    # Retrieve the captured output
    output = output_capture.getvalue()
    print(output)  # You can now use the 'output' variable as needed
    #start = output.find("ERR")
    err_index = output.find("ERR")  
    srr_index = output.find("SRR")  
    start = min(i for i in [err_index, srr_index] if i != -1)
    end = output.find("solve status")
    output = output[start:end]
    # import pdb; pdb.set_trace()
    output = output.split("\n")
    output = [x for x in output if x]
    output = [x.split() for x in output]
    output = [[x[0].split("_")[0], x[-3].split(",")[0], x[-7].split(")")[0] ,x[-1].split(")")[0]] for x in output]
    # convert the output to a dataframe
    output = pd.DataFrame(output, columns=["Job", "Machine","Start", "Duration"])
    # print(output)
    # output_sorted = output.sort_values(by=["Machine","Start"])
    output["Start"] = output["Start"].astype(int)
    output["Duration"] = output["Duration"].astype(int)
    output_sorted = output.groupby("Machine").apply(lambda x: x.sort_values("Start")).reset_index(drop=True)
    print(output_sorted)
    logging.info(output_sorted)
    grouped=output_sorted.groupby("Machine").agg({'Duration':'sum'}).reset_index()
    print(grouped)
    logging.info(grouped)
    t=output_sorted
    #increment by 1 in all values of Machine column
    t["Machine"] = t["Machine"].astype(int);t["Machine"] = t["Machine"] + 1; t = t[["Job","Machine"]];t
    # group Job by Machine and make a list of Jobs of dictionary keeping machine as key
    t_jobs = t.groupby('Machine')['Job'].apply(list).reset_index()
    t_jobs = t_jobs.set_index('Machine').to_dict(); t_jobs = t_jobs["Job"]
    # remove job name from the dictionary
    job_allocation=t_jobs;del t_jobs
    # job_allocation
    # Input ei - Execution plan for machine mi
    # Output: Gives execution schedule for VMs and Executes the schedule on machines
    start= time.time() 
    machines = range(1,len(machines)+1)
    for i in range(0,len(machines)):
        ei = job_allocation[machines[i]]
        with open("schedule_m{}.txt".format(machines[i]), "w") as f:
            f.write("BEGIN\n")
            for j in ei:
                f.write("EXEC " + j + "\n")
            f.write("END\n")
    # print("Copied scheduling plan on all machines")
    # logging.info("Copied scheduling plan on all machines")
    # print("Starting job execution on all machines")
    # logging.info("Starting job execution on all machines")
    for i in range(len(machines)):
        print(f"Jobs on Machine {machines[i]}: {job_allocation[machines[i]]}")
        logging.info(f"Jobs on Machine {machines[i]}: {job_allocation[machines[i]]}")
    end = time.time()
    # print("Time taken for JobShop schedlule workload generation: ", round(end-start,3))
    
def flexible_jobshop_stage_2(upload_data,machines):
    output_capture = io.StringIO()
    # print("Starting flexible jobshop stage 2")
        # Redirect stdout to the StringIO object
    with contextlib.redirect_stdout(output_capture):
        flexible_jobshop_2(upload_data,machines)
        # Retrieve the captured output
        output = output_capture.getvalue()
        print(output)  # You can now use the 'output' variable as needed
        logging.info(output)
        err_index = output.find("ERR")
        srr_index = output.find("SRR")
        start = min(i for i in [err_index, srr_index] if i != -1)
        end = output.find("solve status")
        output = output[start:end]
        # import pdb; pdb.set_trace()
        output = output.split("\n")
        output = [x for x in output if x]
        output = [x.split() for x in output]
        output = [[x[0], x[-3].split(",")[0], x[-7].split(")")[0] ,x[-1].split(")")[0]] for x in output]
        # convert the output to a dataframe
        output = pd.DataFrame(output, columns=["Job", "Machine","Start", "Duration"])
        # print(output)
        # output_sorted = output.sort_values(by=["Machine","Start"])
        output["Start"] = output["Start"].astype(int)
        output["Duration"] = output["Duration"].astype(int)
        output_sorted = output.groupby("Machine").apply(lambda x: x.sort_values("Start")).reset_index(drop=True)
        print(output_sorted)
        grouped=output_sorted.groupby("Machine").agg({'Duration':'sum'}).reset_index()
        print(grouped)
        logging.info(grouped)
        t=output_sorted
        #increment by 1 in all values of Machine column
        t["Machine"] = t["Machine"].astype(int);t["Machine"] = t["Machine"] + 1; t = t[["Job","Machine"]];t
        # group Job by Machine and make a list of Jobs of dictionary keeping machine as key
        t_jobs = t.groupby('Machine')['Job'].apply(list).reset_index()
        t_jobs = t_jobs.set_index('Machine').to_dict(); t_jobs = t_jobs["Job"]
        print(t_jobs)
        # remove job name from the dictionary
        job_allocation=t_jobs;del t_jobs
        # job_allocation
        # Input ei - Execution plan for machine mi
        # Output: Gives execution schedule for VMs and Executes the schedule on machines
        machines = range(1,len(machines)+1)
        print(machines)
        # logging(machines)
        print(job_allocation)
        logging.info(job_allocation)
        for i in range(0,len(machines)):
            ei = job_allocation[machines[i]]
            with open("schedule_m{}.txt".format(machines[i]), "w") as f:
                f.write("BEGIN\n")
                for j in ei:
                    print(j) # This works only for two stages needs to be modified for more stages
                    if j.endswith("1"):
                        # print("second stage")
                        f.write("WAIT " + j + "\n") # Here it wait and check if j_0 is completed
                        f.write("EXEC " + j + "\n")
                    else:
                        # print("first stage")
                        f.write("EXEC " + j + "\n")
                        f.write("SIGNAL " + j.split("_")[0] + "_1\n") # this means j_0 is completed
                f.write("END\n")
        for i in range(len(machines)):
            print(f"Jobs on Machine {machines[i]}: {job_allocation[machines[i]]}")
            
# main function           
def main():
    args = parse_arguments()
    data_imported = pd.read_csv(args.input_file, sep=",")
    machines = args.machines
    # create a list of machines
    machines = [f"m{i}" for i in range(1,machines+1)]
    l = len(machines)
    print("Printing number of machines: \t",l)
    data_imported = data_imported.sample(n=args.subset_size,random_state=args.subset_num)
    data_imported = data_imported.reset_index(drop=True)
    for i in range(l+1,5,-1): # Run the same sample across all machines
        temp_data = data_imported.iloc[:,0:i]
        # print("Your data is: \n")
        # num_subsets=args.n
        if args.algorithm == 'jobshop' and args.stage == 'stage1':
            print(temp_data)
            flexible_jobshop_stage_1(temp_data,machines)
        elif args.algorithm == 'greedy' and args.stage == 'stage1':
            print(temp_data)
            greedy_stage_1(temp_data,machines)
        elif args.algorithm == 'jobshop' and args.stage == 'stage2':
            print(data_imported)
            flexible_jobshop_stage_2(data_imported,machines)
            pass
        elif args.algorithm == 'greedy2' and args.stage == 'stage1':
            print(data_imported)
            new_greedy_stage_1(temp_data,machines)
            pass
        else:
            print("Invalid arguments, please check the arguments and try again!🥹")
if __name__ == "__main__":
    main()

