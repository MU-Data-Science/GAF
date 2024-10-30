import subprocess
import os
from ParseTSV import process_cadd_into_n3
from ParseVCF import annotate_vcf
from AnnotateGraphName import annotate_files_with_graph_name
from constants import (
    VCF_FILE_UPLOADS_PATH,
    SNPEFF_JAR_PATH,
    SPARQLING_GEN_PATH
)
import gradio as gr
from typing import List, Union
import concurrent.futures

def process_vcf_file(f, progress):
    try:
        progress(0.05, desc="Annotating VCF")
        print("PROGRESS --------- Annotating VCF")
        annotated_vcf_file = annotate_vcf(f, SNPEFF_JAR_PATH)
        
        progress(0.25, desc="Converting annotated VCF to N3")
        print("PROGRESS --------- Converting annotated VCF to N3")
        n3_output_file = open(annotated_vcf_file.replace('.vcf', '.n3'), 'w')
        subprocess.check_call([SPARQLING_GEN_PATH, "-i", annotated_vcf_file, "-O", "ntriples"], stdout=n3_output_file)
        
        progress(0.50, desc="Converting N3 to NQ")
        print("PROGRESS --------- Converting N3 to NQ")
        nq_output_filepath = annotate_files_with_graph_name(annotated_vcf_file.replace('.vcf', '.n3'))
        return nq_output_filepath
    except Exception as e:
        print(e)
        progress(0.0, desc=str(e))
        return None

def process_tsv_file(f, progress):
    try:
        save_path = process_cadd_into_n3(f, progress)
        return save_path
    except Exception as e:
        print(e)
        progress(0.0, desc=str(e))
        return None

def process_file(f, progress):
    progress(0.0, desc=f"Processing {os.path.basename(f)}")
    if f.endswith('.vcf'):
        return process_vcf_file(f, progress)
    elif f.endswith('.tsv'):
        return process_tsv_file(f, progress)
    return None

def convert_to_nq_or_ttl(
    uploads_path: Union[str, List] = VCF_FILE_UPLOADS_PATH,
    progress: gr.Progress = None) -> List[str]:
    processed_file_paths = []
    try:
        if isinstance(uploads_path, str):
            uploads_path = [uploads_path]
        files_to_process = []
        for item in uploads_path:
            if os.path.isfile(item):
                files_to_process.append(item)
            elif os.path.isdir(item):
                files_to_process.extend([os.path.join(item, f) for f in os.listdir(item) if f[-4:] in [".vcf", ".tsv"]])
        progress(0.0, desc=f"Processing {len(files_to_process)} files")
        print(f"PROGRESS --------- These are the files I am processing: {files_to_process}")
        
        with concurrent.futures.ThreadPoolExecutor() as executor:
            futures = [executor.submit(process_file, f, progress) for f in files_to_process]
            for future in concurrent.futures.as_completed(futures):
                result = future.result()
                if result:
                    processed_file_paths.append(result)
    except Exception as e:
        print(e)
        progress(0.0, desc=str(e))
        return processed_file_paths
    return processed_file_paths
