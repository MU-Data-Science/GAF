# pip3 install torch-geometric

import torch
torch.manual_seed(0)
import random
random.seed(0)

import pandas as pd
import json
import os
import numpy as np
import pandas as pd
import pickle

from mapping_data import map_data

import dgl
from dgl.data.utils import save_graphs
import argparse
from input_dataclasses import GraphCreationConfig
from constants import (
    EDGE_WEIGHT_TYPE_INDEGREE,
    EDGE_WEIGHT_TYPE_UNIFORM
)
from local_utils import get_str_datetime, get_logger
import gradio as gr


logger = get_logger("create_graph")
ap = argparse.ArgumentParser()
ap.add_argument("-i", "--input", required=False, help="input path")
args = vars(ap.parse_args())
feats_path = str(args['input']) 

def save_graph(gr, number_of_classes, graph_output_path):
    logger.debug("PROGRESS --------- Graph creation")
    graph_labels = {"glabel": torch.tensor([0, number_of_classes])}
    save_graphs(graph_output_path, gr, graph_labels)

    return graph_output_path

def get_indices(nodes_df, total_selected_len):
    min_class_samples = min(nodes_df['ann_impact'][:total_selected_len].value_counts())
    selected_indices = []
    for class_name in [0, 1, 2, 3]:
        nodes = nodes_df.loc[nodes_df['ann_impact'] == class_name]
        selected_indices.extend(nodes[:min_class_samples].index)
    return selected_indices

def create_graph(config: GraphCreationConfig, progress: gr.Progress):

    node_feat_path, edge_feat_path, PATH_TO_SAVE_FILES, total_selected_len = map_data(config, progress)
    nodes_data = pd.read_parquet(node_feat_path)

    progress(0, desc="Converting features to tensors")
    node_features_df = nodes_data.drop(['accession_id', 'origin', 'variant_id', 'ann_impact', 'ann'], axis=1)
    node_features = torch.from_numpy(node_features_df.to_numpy())
    node_labels = torch.from_numpy(nodes_data[config.class_column].to_numpy()).to(torch.long)
    
    logger.debug(f'Dataset Information: \n Node columns: {nodes_data.columns} \n 3 node rows: {nodes_data.head(3)} \n Node features: {node_features_df.columns} \n Number of target classes: {config.number_of_classes}')

    edges_data = pd.read_parquet(edge_feat_path)
    
    edges_src = torch.from_numpy(edges_data['origin'].to_numpy()).long()
    edges_dst = torch.from_numpy(edges_data['variant_id'].to_numpy()).long()
    if config.edge_weight_type == EDGE_WEIGHT_TYPE_INDEGREE:
        edge_features = torch.tensor([edges_dst.eq(item).sum() for item in edges_dst])
    elif config.edge_weight_type == EDGE_WEIGHT_TYPE_UNIFORM:
        edge_features = torch.tensor([config.edge_weight_value] * edges_data.shape[0])
    progress(0, desc="Creating the graph")
    graph = dgl.graph((edges_src, edges_dst), num_nodes=nodes_data.shape[0])
    
    graph.ndata['feat'] = node_features
    graph.ndata['label'] = node_labels
    graph.edata['weight'] = edge_features

    # total_selected_len = max(edges_data.origin)
    n_nodes = nodes_data.shape[0]
    n_train = int(total_selected_len * config.train_size)
    n_val = int(total_selected_len * config.val_size)
    
    logger.debug(f"INFO --------- Train:Val:Test \t {n_train, n_val, total_selected_len - (n_train + n_val)}")
    logger.debug(f"INFO --------- {nodes_data[:n_train][config.class_column].value_counts(), nodes_data[n_train:n_train+n_val][config.class_column].value_counts(), nodes_data[n_train+n_val:total_selected_len][config.class_column].value_counts()}")
    
    config.graph_metadata = {'number_of_nodes': graph.number_of_nodes(), "number_of_edges": graph.number_of_edges()}

    nodes_data[:n_train].to_parquet(f'{PATH_TO_SAVE_FILES}/train_set.parquet')
    nodes_data[n_train:n_train+n_val].to_parquet(f'{PATH_TO_SAVE_FILES}/val_set.parquet')
    nodes_data[n_train+n_val:total_selected_len].to_parquet(f'{PATH_TO_SAVE_FILES}/test_set.parquet')

    train_selected_indices = get_indices(nodes_data, n_train)
    train_mask = torch.zeros(n_nodes, dtype=torch.bool)
    val_mask = torch.zeros(n_nodes, dtype=torch.bool)
    test_mask = torch.zeros(n_nodes, dtype=torch.bool)
    # train_mask[:n_train] = True
    train_mask = train_mask.index_fill_(0, torch.tensor(train_selected_indices, dtype=torch.int64), True)
    logger.debug(f"INFO --------- Total training dataset size \t {sum(train_mask.int())}")
    val_mask[n_train:n_train + n_val] = True
    test_mask[n_train + n_val:total_selected_len] = True
    graph.ndata['train_mask'] = train_mask
    graph.ndata['val_mask'] = val_mask
    graph.ndata['test_mask'] = test_mask

    if config.add_bidirectional_edges:
        graph = dgl.to_bidirected(graph, copy_ndata=True)
    # if config.add_self_loop:
    #     graph = dgl.add_self_loop(graph)
    unique_datetime_str = get_str_datetime()
    progress(0, desc="Saving the graph")
    graph_output_path = save_graph(graph, config.number_of_classes, graph_output_path=f'{PATH_TO_SAVE_FILES}/graph_{unique_datetime_str}.bin')
    
    print(config)
    
    json.dump(
        config.model_dump(),
        open(f'{PATH_TO_SAVE_FILES}/graph_{unique_datetime_str}_config.json', 'w')
    )
    logger.debug(f'PROGRESS --------- Graph stored as: {graph_output_path}')
    
    return graph_output_path, config.number_of_classes

if __name__=="__main__":
    # feats_path = '/mydata/dgl/general/Data/new_data_feb_mar_3.parquet'
    config = GraphCreationConfig(input_file_or_dirpath='/mydata/dgl/general/new_feb_mar3_1500.parquet')
    graph_output_path, number_of_classes = create_graph(config)