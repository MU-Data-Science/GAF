'''
Developed by Shivika Prasanna on 02/16/2022.
Last updated on 03/22/2022.
Reads ntriples and annotates unique graph name. 
Working code. 
Run in terminal as: python3 Code/AnnotateGraphName.py -n <annotated directory path>
n3_path = '/path/to/VCF/annotated/N3'
'''

import os, csv
import pandas as pd

def annotate_files_with_graph_name(n3_file):
    head = n3_file.split('.')[-4]
    accession_id = head.split('/')[-1]
    graph_label = " <sg://" + accession_id + "> ."
    print(graph_label)
    with open (n3_file, 'r') as n3_file_in, open(n3_file.replace('.n3', '.nq'), 'w') as n3_file_out:
        for i, line in enumerate(n3_file_in):
            line = line.rstrip('. \n') + graph_label
            print(line, file=n3_file_out)

    print("Completed writing NQ file!")
    return n3_file.replace('.n3', '.nq')
