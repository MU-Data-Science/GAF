from Bio import Phylo
import matplotlib.pyplot as plt

# Load the tree
tree = Phylo.read("./results/tree/output.tree", "newick")

# Plot the tree
fig = plt.figure(figsize=(10, 20), dpi=100)
axes = fig.add_subplot(1, 1, 1)
Phylo.draw(tree, axes=axes)

# Save the plot
plt.savefig("./results/phylogeny/phylogeny.png")
