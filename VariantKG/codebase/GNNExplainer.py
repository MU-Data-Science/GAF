
def plot_subgraph_with_exact_degree(g, labels, exact_degree, num_subgraphs):
    # Convert the DGL graph to a NetworkX graph
    
    import dgl
    import networkx as nx
    import matplotlib.pyplot as plt
    nx_g = g.to_networkx().to_undirected()

    # Find all nodes with the exact given degree
    nodes_with_exact_degree = [node for node, degree in nx_g.degree if degree == exact_degree]

    if not nodes_with_exact_degree:
        print(f"No nodes found with degree == {exact_degree}")
        return

    # Limit the number of subgraphs to plot
    nodes_with_exact_degree = nodes_with_exact_degree[:num_subgraphs]
    import matplotlib.pyplot as plt
    for node in nodes_with_exact_degree:
        # Print the node number with the exact degree
        print("Node with the exact degree:", node)

        # Extract the subgraph containing the node and its neighbors
        nodes = [node] + list(nx_g.neighbors(node))
        subg = g.subgraph(nodes)

        # Convert the DGL subgraph to a NetworkX graph
        nx_subg = subg.to_networkx().to_undirected()

        # Debug: Print nodes and edges
        print("Nodes in subgraph:", nx_subg.nodes())
        print("Edges in subgraph:", nx_subg.edges())
    
        # Check if the subgraph is empty
        if len(nx_subg.nodes()) == 0:
            print("The subgraph is empty. Please check the node ID and the graph structure.")
        else:
            # Set up the plot
            plt.figure(figsize=(12, 8))

            # Draw the graph
            pos = nx.spring_layout(nx_subg, k=0.5, iterations=50)  # Adjust layout parameters
            nx.draw(nx_subg, pos, node_color='lightblue', node_size=3000, alpha=0.8)

            # Highlight the node with the exact degree
            nx.draw_networkx_nodes(nx_subg, pos, nodelist=[0], node_color='red', node_size=3000, alpha=0.8)

            # Add node IDs inside the nodes
            node_labels = {i: str(subg.ndata[dgl.NID][i].item()) for i in range(subg.num_nodes())}
            nx.draw_networkx_labels(nx_subg, pos, node_labels, font_size=10, font_weight='bold')

            # Add class labels as external labels
            external_labels = {}
            for i in range(subg.num_nodes()):
                node_id = subg.ndata[dgl.NID][i].item()
                if node_id < len(labels):  # Check if node_id is within the valid range
                    label = labels[node_id].item()
                    external_labels[i] = f"Class: {label}"
                else:
                    external_labels[i] = "Class: N/A"  # Handle out-of-bounds case

            label_pos = {k: (v[0], v[1] + 0.1) for k, v in pos.items()}  # Adjust label positions
            nx.draw_networkx_labels(nx_subg, label_pos, external_labels, font_size=8, font_weight='bold', font_color='black')

            plt.title(f"Node with Degree {exact_degree} and its Neighbors", fontsize=16)
            plt.axis('off')
            plt.tight_layout()
            plt.show()
            plt.savefig(f"subgraph_{node}.png")

# Example usage
exact_degree = 2  # Set your desired exact degree
num_subgraphs = 2 # Set the number of subgraphs to plot

# plot_subgraph_with_exact_degree(g, labels, exact_degree, num_subgraphs)


def main(node_id,exact_degree, num_subgraphs):
    #remove all subgraph*.png files in /mydata/GAF/VariantKG/codebase/ directory
    import os
    for file in os.listdir('/mydata/GAF/VariantKG/codebase/'):
        if file.startswith("subgraph") and file.endswith(".png"):
            os.remove(os.path.join('/mydata/GAF/VariantKG/codebase/', file))
    
    import dgl
    import dgl.function as fn
    import torch
    import torch.nn as nn
    from dgl.nn import GNNExplainer
    import networkx as nx
    import matplotlib.pyplot as plt
    node_id=10
    
    # Load dataset
    g = dgl.load_graphs('/mydata/GAF/VariantKG/MODEL-500-16/graph_new.dgl')[0][0]
    features = g.ndata['feat'].float()  # Ensure features are float
    labels = g.ndata['label'].long()  # Ensure labels are long
    train_mask = g.ndata['train_mask']

    # Define a model
    class Model(nn.Module):
        def __init__(self, in_feats, out_feats):
            super(Model, self).__init__()
            self.linear = nn.Linear(in_feats, out_feats)
        def forward(self, graph, feat, eweight=None):
            with graph.local_scope():
                feat = self.linear(feat)
                graph.ndata['h'] = feat
                if eweight is None:
                    graph.update_all(fn.copy_u('h', 'm'), fn.sum('m', 'h'))
                else:
                    graph.edata['w'] = eweight
                    graph.update_all(fn.u_mul_e('h', 'w', 'm'), fn.sum('m', 'h'))
                return graph.ndata['h']

    # Train the model
    model = Model(features.shape[1], 4)
    criterion = nn.CrossEntropyLoss()
    optimizer = torch.optim.Adam(model.parameters(), lr=1e-2)
    for epoch in range(10):
        logits = model(g, features)
        
        loss = criterion(logits[train_mask], labels[train_mask])
        optimizer.zero_grad()
        loss.backward()
        optimizer.step()

    # Explain the prediction for node 10
    explainer = GNNExplainer(model, num_hops=1)
    new_center, sg, feat_mask, edge_mask = explainer.explain_node(node_id, g, features)
    new_center
    sg.num_edges()
    # Old IDs of the nodes in the subgraph
    print(sg.ndata[dgl.NID])
    # Old IDs of the edges in the subgraph
    print(sg.edata[dgl.EID])
    print(feat_mask)
    print(edge_mask)


    import dgl
    import networkx as nx
    import matplotlib.pyplot as plt
    

    plot_subgraph_with_exact_degree(g, labels, exact_degree, num_subgraphs)
    

    import dgl
    import dgl.function as fn
    import torch
    import torch.nn as nn
    from dgl.data import CoraGraphDataset
    from dgl.nn import GNNExplainer
    import numpy as np
    import matplotlib.pyplot as plt
    import seaborn as sns


    # # Explain the prediction for node 10
    # explainer = GNNExplainer(model, num_hops=1)
    # new_center, sg, feat_mask, edge_mask = explainer.explain_node(node_id, g, features)

    # Define feature names (example)
    feature_names = [f'Feature {i}' for i in range(features.shape[1])]  # Replace with actual feature names if available

    # Extract and display the most important features
    def get_important_features(feat_mask, feature_names, top_k=5):
        feat_mask = feat_mask.detach().numpy()
        important_indices = np.argsort(feat_mask)[-top_k:][::-1]
        important_features = [feature_names[i] for i in important_indices]
        return important_features, feat_mask[important_indices]

    important_features, importance_scores = get_important_features(feat_mask, feature_names, top_k=5)
    print("Most important features and their scores:")
    for feature, score in zip(important_features, importance_scores):
        print(f"{feature}: {score}")

    # Plot the most important features as a heatmap with a blue gradient
    def plot_important_features_heatmap(important_features, importance_scores):
        plt.figure(figsize=(10, 6))
        sns.heatmap([importance_scores], annot=True, cmap='Blues', xticklabels=important_features, yticklabels=['Importance'])
        plt.xlabel('Features')
        plt.title('Top Important Features')
        plt.show()
        plt.savefig("subgraph_heatmap.png")

    plot_important_features_heatmap(important_features, importance_scores)
    
    # Print other details
    print("New center node:", new_center)
    print("Number of edges in subgraph:", sg.num_edges())
    print("Old IDs of the nodes in the subgraph:", sg.ndata[dgl.NID])
    print("Old IDs of the edges in the subgraph:", sg.edata[dgl.EID])
    print("Feature mask:", feat_mask)
    print("Edge mask:", edge_mask)
    
