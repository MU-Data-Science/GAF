import os
import glob
import pandas as pd

print("Executing python script and generating summary file...")
impact_levels=("HIGH", "LOW", "MODIFIER", "MODERATE")

data = {
    'samples': [],
    'missense': [],
    'synonymous': [],
    'HIGH': [],
    'LOW': [],
    'MODERATE': [],
    'MODIFIER': [],
    'Total SNPs': [],
    'Total genes infected': []
}

for vcf in glob.glob('./results/annotated_vcf/*_ann.vcf'):
    base_name = os.path.basename(vcf).replace('.vcf', '')
    data['samples'].append(base_name)

    total_snps = 0
    total_genes = 0

    for impact in impact_levels:
        with open(f'./results/stats/{base_name}_{impact}_impact_variants.txt', 'r') as f:
            lines = f.readlines()
            genes = set()
            snps = set()
            for line in lines[1:]:
                fields = line.strip().split('\t')
                genes.add(fields[5])
                snps.add(fields[2])
            data[impact].append((len(genes), len(snps)))
            total_snps += len(snps)
            total_genes += len(genes)

    for effect in ['missense', 'synonymous']:
        with open(f'./results/stats/{base_name}_{effect}_variant.txt', 'r') as f:
            lines = f.readlines()
            genes = set()
            snps = set()
            for line in lines[1:]:
                fields = line.strip().split('\t')
                genes.add(fields[5])
                snps.add(fields[2])
            data[effect].append((len(genes), len(snps)))
            total_snps += len(snps)
            total_genes += len(genes)

    data['Total SNPs'].append(total_snps)
    data['Total genes infected'].append(total_genes)

df = pd.DataFrame(data)
df.to_csv('./results/summary.csv', index=False)
