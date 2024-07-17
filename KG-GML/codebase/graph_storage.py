import subprocess, os, shlex
from constants import (
    DEFAULT_BLAZEGRAPH_INPUT_DIR,
    BLAZEGRAPH_JAR_PATH,
    QUAD_PROPERTIES_PATH,
    TURTLE_PROPERTIES_PATH
)
import gradio as gr
import time
from typing import Union, List
from feature_generator import check_connection
from input_dataclasses import GraphEnrichmentConfig

def stop_blazegraph(pid: int, progress: gr.Progress = None):
    print("Stopping Blazegraph!")
    subprocess.call(shlex.split('kill -9 ' + str(pid)))
    if progress is not None:
        for _ in progress.tqdm(range(20), desc="Waiting for Blazegraph to stop"):   
            time.sleep(1)
    else:
        time.sleep(25)

def start_blazegraph(progress: gr.Progress= None):
    print("Starting Blazegraph in the background!")
    # subprocess.call(shlex.split('java -server -Xmx4g -jar ' + blazegraph_jar_path))
    
    return_value = subprocess.Popen(shlex.split('java -server -Xmx4g -Dbigdata.propertyFile=' + TURTLE_PROPERTIES_PATH + ' -Dbigdata.propertyFile=' + QUAD_PROPERTIES_PATH + ' -jar ' + BLAZEGRAPH_JAR_PATH), close_fds=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT); print("Background process started!")
    print(return_value.pid)
    if progress is not None:
        for _ in progress.tqdm(range(30), desc="Waiting for Blazegraph to load"):   
            time.sleep(1)
    else:
        time.sleep(15)
    return return_value.pid

def store_data_on_blazegraph(
    input_path: Union[str, List] = DEFAULT_BLAZEGRAPH_INPUT_DIR,
    progress: gr.Progress= None,
    config: GraphEnrichmentConfig = None):
    if check_connection(config):
        stop_blazegraph(config.blazegraph_pid)
    if isinstance(input_path, str):
        input_path = [input_path]
    files_to_process = []
    for item in input_path:
        if os.path.isfile(item):
            files_to_process.append(item)
        elif os.path.isdir(item):
            files_to_process.extend([os.path.join(input_path, f) for f in os.listdir(input_path) if f.endswith(".nq") or f.endswith(".ttl")])
    for f in progress.tqdm(files_to_process, desc="Uploading to Blazegraph"):
        print(f)
        if f.endswith('.nq'):
            print("Loading VCF files into Blazegraph..")
            subprocess.call(shlex.split('java -Xmx4g -cp ' + BLAZEGRAPH_JAR_PATH + ' com.bigdata.rdf.store.DataLoader -verbose -namespace kb ' + QUAD_PROPERTIES_PATH + ' ' + f))     
        elif f.endswith('.ttl'):
            subprocess.call(shlex.split('java -Xmx4g -cp ' + BLAZEGRAPH_JAR_PATH + ' com.bigdata.rdf.store.DataLoader -verbose defaultGraph http://sg.org -namespace kb ' + TURTLE_PROPERTIES_PATH + ' ' + f))   
        
    print("Done loading!")
    return start_blazegraph(progress)
