import pandas as pd
import numpy as np

import rdflib
from rdflib import Dataset, Literal, URIRef, XSD

graph = Dataset()

csv_path = '../metadata_rnaseq.csv'
input_csv_data = pd.read_csv(csv_path)

for idx, row in input_csv_data.iterrows():
    subject = URIRef(f'https://www.ncbi.nlm.nih.gov/sra/?term={row["Run"]}')
    predicate = URIRef('https://www.wikidata.org/wiki/Q11904283')
    if row["Age"] == 'pediatric' or row["Age"] == 'postnatal day 0 / full term':
        object = Literal("0", datatype=XSD.float)
    elif row["Age"] == '' or pd.isna(row["Age"]):
        continue
    else:
        object = Literal(row["Age"], datatype=XSD.float)
    graph_URI = URIRef(f'sg://{row["Run"]}')
    
    graph.add((subject, predicate, object, graph_URI))

graph.serialize(destination=csv_path.replace('.csv', '.nq'), format='nquads')
    

'''
Columns: 'Run', 'Age', 'Assay Type', 'AvgSpotLen', 'Bases', 'BioProject',
       'BioSample', 'Bytes', 'Center Name', 'disease', 'disease_stage',
       'Experiment', 'Instrument', 'GEO_Accession (exp)', 'LibrarySelection',
       'LibrarySource', 'Organism', 'ReleaseDate',
       'create_date', 'Sample Name', 'sex', 'source_name', 'SRA Study',
       'tissue'.
WikiData links:
'Age' - <https://www.wikidata.org/wiki/Q11904283>
'Assay Type' - <https://www.wikidata.org/wiki/Q41853466>
'AvgSpotLen' - <https://www.wikidata.org/wiki/Q36253>
'Bases' - <https://www.wikidata.org/wiki/Q422960>
'BioProject' - <https://www.wikidata.org/wiki/Q114631141>
'BioSample' - <https://www.wikidata.org/wiki/Q111156345>
'Bytes' - <https://www.wikidata.org/wiki/Q95959608>
'Center Name' - <https://www.wikidata.org/wiki/Q7315155>
'disease' - <https://www.wikidata.org/wiki/Q12136>
'disease_stage' - <https://www.wikidata.org/wiki/Q10960555>
'Experiment' - <https://www.wikidata.org/wiki/Q101965>
'Instrument' - <https://www.wikidata.org/wiki/Q39546>
'GEO_Accession (exp)' - <https://www.wikidata.org/wiki/Q3346344>
'LibrarySelection' - <https://www.wikidata.org/wiki/Q33290074>
'LibrarySource' - <https://www.wikidata.org/wiki/Q31464082>
'Organism' - <https://www.wikidata.org/wiki/Q7239>
'ReleaseDate' - <https://www.wikidata.org/wiki/Q47150325>
'Sample Name' - <https://www.wikidata.org/wiki/Q20997795>
'sex' - <https://www.wikidata.org/wiki/Q290>
'source_name' - <https://www.wikidata.org/wiki/Q31464082>
'SRA Study' - <https://www.wikidata.org/wiki/Q7452459>
'tissue' - <https://www.wikidata.org/wiki/Q40397> 
'''
