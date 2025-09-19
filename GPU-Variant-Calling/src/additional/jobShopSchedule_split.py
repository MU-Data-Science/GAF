import collections
import time
from ortools.sat.python import cp_model

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

def flexible_jobshop():
    """solve a small flexible jobshop problem."""
    # Data part.     
    run = [
   "ERR016294",
    "ERR016314",
    "ERR016316",
    "ERR016317",
    "ERR016320",
    "ERR016326",
    "ERR016327",
    "ERR016338",
    "ERR016344",
    "ERR016350",
    "ERR018197",
    "ERR018198",
    "ERR018204",
    "ERR018395",
    "ERR018416",
    "ERR018423",
    "ERR018429",
    "ERR018435",
    "ERR018436",
    "ERR018442",
    "ERR018448",
    "ERR018454",
    "ERR018460",
    "ERR018463",
    "ERR018469",
    "ERR018475",
    "ERR018476",
    "ERR018482",
    "ERR018491",
    "ERR018492",
    "ERR018501",
    "ERR018507",
    "ERR018513",
    "ERR018517",
    "ERR018538",
    "ERR018544",
    "ERR018550",
    "ERR018558",
    "ERR019481",
    "ERR019482",
    "ERR019488",
    "ERR019492",
    "ERR019498",
    "ERR019897",
    "ERR019903",
    "ERR020157",
    "ERR020229",
    "ERR020235",
    "ERR020241",
    "ERR020247",
    "ERR020253",
    "ERR020257",
    "ERR020263",
    "ERR020264",
    "ERR020270",
    "ERR020276",
    "ERR020282",
    "ERR020288",
    "ERR020289",
    "ERR022370",
    "ERR022393",
    "ERR022429",
    "ERR022450",
    "ERR022456",
    "ERR022457",
    "ERR022463",
    "ERR022469",
    "ERR023204",
    "ERR023210",
    "ERR023211",
    "ERR023220",
    "ERR023221",
    "ERR062924",
    "ERR062930",
    "ERR062934",
    "ERR062940",
    "ERR062941",
    "ERR062947",
    "ERR062953",
    "ERR062959",
    "ERR062966",
    "ERR062967",
    "ERR062973",
    "ERR062979",
    "ERR065364",
    "ERR068392",
    "ERR12713838",
    "SRR111943",
    "SRR793879",
    "SRR794275",
    "SRR794312",
    "SRR794319",
    "SRR794349",
    "SRR794355",
    "SRR794367",
    "SRR794387",
    "SRR794397",
    "SRR794398",
    "SRR799760"
    ]
    #data
    import pandas as pd
    df = pd.read_csv("./split_data.csv")
    df = df.drop(columns=["Sample"])
    data = df.values.tolist()
    jobs = [[[(data[i][j], j//2) for j in range(0, len(data[i]), 2)], [(data[i][j], j//2) for j in range(1, len(data[i]), 2)]] for i in range(len(data))]
    print(jobs)
    num_jobs = len(jobs)
    all_jobs = range(num_jobs)
    num_machines = 5
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

    # Solve model.
    solver = cp_model.CpSolver()
    solution_printer = SolutionPrinter()
    status = solver.Solve(model, solution_printer)
    n=0
    # Print final solution.
    for job_id in all_jobs:
        #print("Job %i:" % job_id)
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
            runner=run[n]
            print("%s_%i_%i starts at %i (alt %i, machine %i, duration %i)" % (runner,job_id, task_id, start_value, selected, machine, duration))

        n=n+1
    print("solve status: %s" % solver.StatusName(status))
    print("Optimal objective value: %i" % solver.ObjectiveValue())
    print("Statistics")
    print("  - conflicts : %i" % solver.NumConflicts())
    print("  - branches  : %i" % solver.NumBranches())
    print("  - wall time : %f s" % solver.WallTime())

flexible_jobshop()
