# pip3 install torch-geometric

import torch
torch.manual_seed(0)
import random
random.seed(0)

import dgl
from dgl.data.utils import load_graphs
from dgl.data import DGLDataset
from dgl.nn import GraphConv
from dgl.nn import SAGEConv

import torch_geometric.transforms as T
import torch.nn as nn
import torch.nn.functional as F
from torch.nn import Linear, Dropout

import pandas as pd
import os
import numpy as np
import pickle
import json
from input_dataclasses import GraphTrainerConfig
from local_utils import get_logger


logger = get_logger("train_model")
dgl.use_libxsmm(False)
logger.debug(dgl.__version__)

class GraphSAGE(nn.Module):
    """GraphSAGE"""
    def __init__(self, in_feats, h_feats, num_classes, num_layers, dropout_rate):
        super(GraphSAGE, self).__init__()
        self.layers = torch.nn.ModuleList()
        
        # Input layer
        self.layers.append(SAGEConv(in_feats, h_feats, "mean"))
        
        # Hidden layers
        for _ in range(num_layers - 2):
            self.layers.append(SAGEConv(h_feats, h_feats, "mean"))
        
        # Output layer
        self.layers.append(SAGEConv(h_feats, num_classes, "mean"))
        
        self.dropout_rate = dropout_rate
        if self.dropout_rate > 0.0:
            self.dropout = nn.Dropout(dropout_rate)

    def forward(self, g, in_feat):
        x = in_feat
        for i, layer in enumerate(self.layers):
            x = F.relu(layer(g, x)) if i != len(self.layers) - 1 else layer(g, x)
            if self.dropout_rate > 0.0 and i != len(self.layers) - 1:
                x = self.dropout(x)
        
        return x

class GCN(nn.Module):
    def __init__(self, in_feats, h_feats, num_classes, num_layers, dropout_rate):
        super(GCN, self).__init__()
        self.layers = torch.nn.ModuleList()
        
        # Input layer
        self.layers.append(GraphConv(in_feats, h_feats))
        
        # Hidden layers
        for _ in range(num_layers - 2):
            self.layers.append(GraphConv(h_feats, h_feats))
        
        # Output layer
        self.layers.append(GraphConv(h_feats, num_classes))
        
        self.dropout_rate = dropout_rate
        if self.dropout_rate > 0.0:
            self.dropout = nn.Dropout(dropout_rate)

    def forward(self, g, in_feat):
        x = in_feat
        for i, layer in enumerate(self.layers):
            x = F.relu(layer(g, x)) if i != len(self.layers) - 1 else layer(g, x)
            if self.dropout_rate > 0.0 and i != len(self.layers) - 1:
                x = self.dropout(x)
        
        return x

def train(g, model, config):
    optimizer = torch.optim.Adam(model.parameters(), lr=config.learning_rate)
    best_val_acc = 0
    best_test_acc = 0

    features = g.ndata['feat'].float()
    labels = g.ndata['label']
    train_mask = g.ndata['train_mask']
    val_mask = g.ndata['val_mask']
    test_mask = g.ndata['test_mask']

    l, freq = labels.unique(return_counts=True)
    output_df = {
        'epoch': [], 
        'train_loss': [], 
        'validation_loss': [], 
        'validation_accuracy': []
    }
    
    config.pickle_file = os.path.join(config.output_dirpath, 'best_preds.pkl')

    # weights = sum(freq[:config.number_of_classes]) / freq[:config.number_of_classes]
    loss_fn = nn.CrossEntropyLoss()
    print(config)
    for e in range(config.number_of_epochs):
        output = model(g, features)
        pred = output.argmax(1)

        loss = loss_fn(output[train_mask], labels[train_mask])
        val_loss = loss_fn(output[val_mask], labels[val_mask])

        # loss = F.cross_entropy(logits[train_mask], labels[train_mask])
        # val_loss = F.cross_entropy(logits[val_mask], labels[val_mask])

        train_acc = (pred[train_mask] == labels[train_mask]).float().mean()
        val_acc = (pred[val_mask] == labels[val_mask]).float().mean()
        test_acc = (pred[test_mask] == labels[test_mask]).float().mean()

        if best_val_acc < val_acc:
            best_val_acc = val_acc
            best_test_acc = test_acc
            torch.save(model.state_dict(), f'{config.output_dirpath}/best_weights_epoch_{e}.pt')
            torch.save(model.state_dict(), f'{config.output_dirpath}/best_weights.pt')
            
            pickle.dump({'gold': labels[test_mask], 'pred': pred[test_mask]}, open(config.pickle_file, 'wb'))

        optimizer.zero_grad()
        loss.backward()
        optimizer.step()

        if e % 1 == 0:
            output_df['epoch'].append(e)
            output_df['train_loss'].append(float(loss))
            output_df['validation_loss'].append(float(val_loss))
            output_df['validation_accuracy'].append(float(val_acc))
            json.dump(output_df, open(config.training_statistics_filename, 'w'))
            logger.debug('In epoch {}, train loss: {:.3f}, train acc: {:.3f}, val loss: {:.3f}, val acc: {:.3f} (best {:.3f})'.format(e, loss, train_acc, val_loss, val_acc, best_val_acc))
            logger.debug(pred[test_mask].unique(return_counts=True))
            
    logger.debug(f'Completed training for hidden layers {config.number_of_hidden_layers} and learning rate {config.learning_rate} over {config.number_of_epochs} epochs')

def run(config: GraphTrainerConfig):
    os.makedirs(config.output_dirpath, exist_ok=True)

    logger.debug("***************Loading Graph***************")
    dataset, graph_labels = load_graphs(config.input_graph_path)
    labels = graph_labels['glabel']
    g = dataset[0]
    logger.debug(g)
    logger.debug(g.is_homogeneous)
    
    if config.model_selection == 'sage':
        # Instantiate SAGEConv model
        model = GraphSAGE(g.ndata['feat'].shape[1], config.number_of_hidden_layers, config.number_of_classes, config.number_of_layers, config.dropout_rate)
    else:
        model = GCN(g.ndata['feat'].shape[1], config.number_of_hidden_layers, config.number_of_classes, config.number_of_layers, config.dropout_rate)
    logger.debug(model)
    # Call model training function
    train(g, model, config)
    