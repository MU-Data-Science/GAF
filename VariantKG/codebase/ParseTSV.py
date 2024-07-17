'''
Developed by Shivika Prasanna on 02/25/2022.
Last updated on 03/21/2022.
Working code.
Parse all CADD scores and convert to N3 RDF triples.
Run as: python3 ParseTSV.py -i <input path to directory> -o <output path for n3 file>
'''

import os
import pandas as pd
from pathlib import Path

# import argparse
from datetime import datetime

from lxml import etree
from rdflib import Graph, Literal, RDF, URIRef, Namespace
from rdflib.namespace import FOAF , XSD

from tqdm import tqdm
import gradio as gr

def process_cadd_into_n3(input_tsv, progress: gr.Progress): 
    g = Graph()
    b = Namespace('http://sg.org/')

    print("Currently processing: {}".format(input_tsv))
    df_file = pd.read_csv(input_tsv, skiprows=2, names=['Chrom', 'Pos', 'Ref', 'Alt', 'RawScore', 'PHRED'], sep='\t', index_col=None)
    file_id = input_tsv.replace('.tsv', '')
    count = 1
    for k, v in progress.tqdm(df_file.iterrows(), desc="Progressing TSV"):
        if 'Chrom' in v:
            chrom_uri = URIRef('http://sg.org/{}/{}'.format(file_id, v['Chrom']))
            g.add((chrom_uri, RDF.type, b.chromosome)) 
            var_uri = URIRef('http://sg.org/{}/{}/variant{}'.format(file_id, v['Chrom'], count))
            g.add((var_uri, RDF.type, b.variant))
            cadd_uri = URIRef('http://sg.org/{}/{}/variant{}/cadd'.format(file_id, v['Chrom'], count))
            g.add((cadd_uri, RDF.type, b.cadd))
            g.add((var_uri, b.has_cadd_scores, cadd_uri))
            if 'Pos' in v:
                g.add((var_uri, b.has_pos, Literal(v['Pos'])))
            if 'Ref' in v:
                g.add((var_uri, b.has_ref_genome, Literal(v['Ref'])))
            if 'Alt' in v:
                g.add((var_uri, b.has_alt_genome, Literal(v['Alt'])))
            if 'RawScore' in v:
                g.add((cadd_uri, b.has_raw_score, Literal(v['RawScore'], datatype=XSD.long)))
            if 'PHRED' in v:
                g.add((cadd_uri, b.has_phred, Literal(v['PHRED'], datatype=XSD.long)))
        count += 1
    print("Items: ", count)
    suffix = '.ttl'
    progress(0, desc="Serializing to ttl")
    g.serialize(file_id+suffix,format='ttl')
    print("Storing output here: {} as {}{}".format(file_id+suffix, file_id, suffix))
    return file_id + suffix
