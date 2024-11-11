import pandas as pd
import numpy as np
import json, random
from itertools import combinations
from collections import defaultdict
import time

from tqdm import tqdm
from pandas.api.types import CategoricalDtype
from create_src_dest_unmapped import create_dataset_ann_impact
from input_dataclasses import GraphCreationConfig
from local_utils import get_logger
import gradio as gr
from constants import (
    EDGE_TYPE_FULLY_CONNECTED,
    EDGE_TYPE_GENE
)

logger = get_logger("create_graph")

def extract_gene_names(annotations):
    ann_list = annotations.split(',')
    gene_names = [ann.split('|')[3] for ann in ann_list]

    return set(gene_names)

def get_gene_name_map(df):
    gene_to_variantIDs = defaultdict(set)
    for index, row in df.iterrows():
        variantID = row['variant_id']
        gene_names = extract_gene_names(row['ann'])

        for gene in gene_names:
            gene_to_variantIDs[gene].add(variantID)

    gene_name_map = defaultdict(lambda: defaultdict(int))

    for gene, variantIDs in gene_to_variantIDs.items():
        variantID_list = list(variantIDs)
        for i in range(len(variantID_list)):
            for j in range(i + 1, len(variantID_list)):
                var1 = variantID_list[i]
                var2 = variantID_list[j]
                gene_name_map[var1][var2] += 1
                gene_name_map[var2][var1] += 1

    gene_name_map = {k: dict(v) for k, v in gene_name_map.items()}
    
    return gene_name_map

def map_data(config: GraphCreationConfig, progress: gr.Progress):
    variant_df_path, origin_df_path, PATH_TO_SAVE_FILES = create_dataset_ann_impact(config, progress)

    variant_id_feats_df = pd.read_parquet(variant_df_path)
    origin_id_feats_df = pd.read_parquet(origin_df_path)
    
    logger.debug(f'Before mapping: \n {origin_id_feats_df}')
    
    map_dict = {}
    origin_id_feats_df[config.class_column] = origin_id_feats_df[config.class_column].apply(lambda x:config.class_map[x])
    map_dict[config.class_column] = config.class_map

    for col in progress.tqdm(origin_id_feats_df.columns, desc="Mapping features to categorical values"):
        if col not in ['accession_id', 'origin', 'variant_id', 'ann', config.class_column]:
            origin_id_feats_df[col] = origin_id_feats_df[col].astype('category').cat.codes
        elif col in ['origin', 'variant_id']:
            categorical_mapping = origin_id_feats_df[col].astype('category')
            origin_id_feats_df[col] = categorical_mapping.cat.codes
            map_dict[col] = {v:k for k,v in dict(enumerate(categorical_mapping.cat.categories)).items()}
    # import pdb; pdb.set_trace()
    # print(f'DEBUG --------- {map_dict["variant_id"]["None"]}')

    origin_id_feats_df.sort_values('origin', inplace=True)
    origin_id_feats_df['variant_id'] += max(origin_id_feats_df['origin']) + 1
    total_selected_len = max(origin_id_feats_df['origin'])
    # none_variant_id = map_dict['variant_id']['None']+max(origin_id_feats_df['origin']) + 1

    json.dump(map_dict, open(f'{PATH_TO_SAVE_FILES}/map_dict.json', 'w'))

    logger.debug(f'After mapping: \n {origin_id_feats_df}')

    logger.debug("PROGRESS --------- Creating and saving the edge features")

    progress(0, desc="Adding edges between variant ids")

    if config.edge_type == EDGE_TYPE_FULLY_CONNECTED:
        unique_variant_id = origin_id_feats_df.variant_id.unique().tolist()
        start = time.time()
        connected_graph_edge_origins, connected_graph_edge_dests = list(zip(*set(combinations(unique_variant_id, 2))))
        logger.debug(f"PROGRESS --------- Created connected graph edges {time.time() - start}")

        new_edges_df = pd.DataFrame({
            'origin': connected_graph_edge_origins,
            'variant_id': connected_graph_edge_dests
        })
        new_edges_df.drop_duplicates().reset_index(drop=True)

    elif config.edge_type == EDGE_TYPE_GENE:
        unique_edges = set()
        gene_name_map = get_gene_name_map(origin_id_feats_df)
        for k, v in gene_name_map.items():
            destinations = list(v.keys())
            for item in zip([k] * len(destinations), destinations):
                unique_edges.add(tuple(sorted(item)))
        # import pdb; pdb.set_trace()
        #if unique_edges is empty break it and return message
        if not unique_edges:
            print("No edges found for the given edge type")
            print("Please check the vcf file size, some files must not be having enough data")
            print(r"Please run: find /path/to/your/directory -type f -size -1M -exec rm {} \;")
        origin_nodes, dest_nodes = zip(*unique_edges)
        new_edges_df = pd.DataFrame({
            'origin': origin_nodes,
            'variant_id': dest_nodes
        })
        new_edges_df.drop_duplicates().reset_index(drop=True)   
             
    edges_features = origin_id_feats_df[['origin', 'variant_id']]
    updated_edges_features = pd.concat([edges_features, new_edges_df])
    progress(0, desc="Saving edge features")
    edge_feat_path = f'{PATH_TO_SAVE_FILES}/edges_features.parquet'
    updated_edges_features.to_parquet(edge_feat_path, index=False)

    edges_features = None
    
    logger.debug("PROGRESS --------- Creating and saving the node features")
    origin_id_feats_df.drop(['variant_id', 'ann'], axis=1, inplace=True)
    origin_id_feats_df.drop_duplicates(inplace=True)

    for col in progress.tqdm(origin_id_feats_df.columns, desc="Featurizing accessing ids"):
        if col not in config.selected_features:
            origin_id_feats_df[col] = np.where(origin_id_feats_df['accession_id'] == col, 1, 0)

    origin_id_feats_df.insert(loc=2, column='variant_id', value='')
    origin_id_feats_df = pd.concat([origin_id_feats_df, variant_id_feats_df], ignore_index=True)
    origin_id_feats_df['accession_id'] = origin_id_feats_df['accession_id'].astype(str)
    progress(0, desc="Saving node features")
    node_feat_path = f'{PATH_TO_SAVE_FILES}/node_features.parquet'
    origin_id_feats_df.to_parquet(node_feat_path, index=False)
    import pdb; pdb.set_trace()
    return node_feat_path, edge_feat_path, PATH_TO_SAVE_FILES, total_selected_len

# node_feat_path, edge_feat_path, PATH_TO_SAVE_FILES = map_data('/mydata/dgl/general/Code/Data/')
