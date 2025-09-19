## Extracting data from multiQC reports and process into features
#Functions defines which will be used in data manipulation
def split_data(data):
    nrows = len(data)
    ncols = len(data.columns)
    for i in range(nrows):
        for j in range(1, ncols):
            # Separate by comma
            pivot = str(data.iat[i, j]).split(',')
            # Remove ')' from all elements in pivot
            pivot = [item.replace(')', '') for item in pivot]
            # If pivot[1] is not empty, assign it
            if len(pivot) > 1:
                data.iat[i, j] = pivot[1]
    return data

def set_headers(data):
    nrows = len(data)
    ncols = len(data.columns)
    for i in range(1):
        for j in range(1, ncols):
            # Separate by comma
            pivot = str(data.iat[i, j]).split(',')
            # Remove ')' from all elements in pivot
            pivot = [item.replace('(', '') for item in pivot]
            #update column name j with pivot[0]
            data.columns.values[j] = pivot[0]
    return data

def set_index_drop(data):
    nrows = len(data)
    ncols = len(data.columns)
    for i in range(nrows):
        # print(data.iat[i,0].split('_')[0])
        data.iat[i,0]=data.iat[i,0].split('_')[0]
        # print(data)
    #Drop every second row after keeping mean in first row
    data = data.drop(data.index[1::2])
    #set first columns as index
    data.set_index(data.columns[0], inplace=True)
    return data

path="./results/multiqc_data/"
#Importing and manipulating data
import pandas as pd
import numpy as np
import os
os.chdir(path)
#For loop to extract from multiqc files
#General Stats
general_stats = pd.read_csv('multiqc_general_stats.txt', sep='\t')
general_stats=set_index_drop(general_stats)
print("General stats has been processed successfully.")
#Sequence Counts
sequence_counts = pd.read_csv('fastqc_sequence_counts_plot.txt', sep='\t')
sequence_counts=split_data(sequence_counts)
sequence_counts=set_index_drop(sequence_counts)
print("Sequence counts has been processed successfully")
# #Per Base Sequence Quality
per_base_sequence_quality = pd.read_csv('fastqc_per_base_sequence_quality_plot.txt', sep='\t')
per_base_sequence_quality=split_data(per_base_sequence_quality)
per_base_sequence_quality=set_index_drop(per_base_sequence_quality)
print("Per base sequence quality has been processed successfully")
#Per sequence quality scores
per_sequence_quality_scores = pd.read_csv('fastqc_per_sequence_quality_scores_plot.txt', sep='\t')
per_sequence_quality_scores=split_data(per_sequence_quality_scores)
per_sequence_quality_scores=set_index_drop(per_sequence_quality_scores)
print("Per sequence quality scores has been processed successfully")
#Per sequence GC count
per_sequence_gc_content = pd.read_csv('fastqc_per_sequence_gc_content_plot_Counts.txt', sep='\t')
per_sequence_gc_content=split_data(per_sequence_gc_content)
per_sequence_gc_content=set_index_drop(per_sequence_gc_content)
print("Per sequence GC content (count) has been processed successfully")
#Per sequence GC percentage
per_sequence_gc_content_percentage = pd.read_csv('fastqc_per_sequence_gc_content_plot_Percentages.txt', sep='\t')
per_sequence_gc_content_percentage=split_data(per_sequence_gc_content_percentage)
per_sequence_gc_content_percentage=set_index_drop(per_sequence_gc_content_percentage)
print("Per sequence GC content (%) has been processed successfully")
#Per base N content
per_base_n_content = pd.read_csv('fastqc_per_base_n_content_plot.txt', sep='\t')
per_base_n_content=split_data(per_base_n_content)
per_base_n_content=set_index_drop(per_base_n_content)
print("Per base n content has been processed successfully")
#Sequence Length Distribution
sequence_length_distribution = pd.read_csv('fastqc_sequence_length_distribution_plot.txt', sep='\t')
sequence_length_distribution=split_data(sequence_length_distribution)
sequence_length_distribution=set_index_drop(sequence_length_distribution)
print("Sequence length distribution has been processed successfully")
#Sequence Duplication Levels
sequence_duplication_levels = pd.read_csv('fastqc_sequence_duplication_levels_plot.txt', sep='\t')
sequence_duplication_levels=set_headers(sequence_duplication_levels)
sequence_duplication_levels=split_data(sequence_duplication_levels)
sequence_duplication_levels=set_index_drop(sequence_duplication_levels)
print("Sequence duplication levels has been processed successfully")
#Overrepresented sequences
Overrepresented_sequences = pd.read_csv('fastqc_overrepresented_sequences_plot.txt', sep='\t')
Overrepresented_sequences=set_index_drop(Overrepresented_sequences)
print("Overrepresented sequences has been processed successfully")
# # Adapter Content
# adapter_content = pd.read_csv('fastqc_adapter_content_plot.txt', sep='\t')
# adapter_content=set_headers(adapter_content)
# adapter_content=set_index_drop(adapter_content)
# # adapter_content=split_data(adapter_content)
#fastqc status check heatmap
fastqc_status_check_heatmap = pd.read_csv('fastqc-status-check-heatmap.txt', sep='\t')
fastqc_status_check_heatmap=set_index_drop(fastqc_status_check_heatmap)
print("Fastqc status check heatmap has been processed successfully")


#Renaming columns
#per_sequence_quality_scores
#Add pbsq to all columns in per_base_sequence_quality
per_base_sequence_quality.columns = ['pbsq_'+str(col) for col in per_base_sequence_quality.columns]
#Add pbnc to all columns in per_base_n_content
per_base_n_content.columns = ['pbnc_'+str(col) for col in per_base_n_content.columns]
#Add psgc to all columns in per_sequence_gc_content
per_sequence_gc_content.columns = ['psgc_'+str(col) for col in per_sequence_gc_content.columns]
#Add psgcp to all columns in per_sequence_gc_content_percentage
per_sequence_gc_content_percentage.columns = ['psgcp_'+str(col) for col in per_sequence_gc_content_percentage.columns]
#Add pss to all columns in per_sequence_quality_scores
per_sequence_quality_scores.columns = ['pss_'+str(col) for col in per_sequence_quality_scores.columns]
#Add psl to all columns in sequence_length_distribution
sequence_length_distribution.columns = ['psl_'+str(col) for col in sequence_length_distribution.columns]
#Add sdl to all columns in sequence_duplication_levels
sequence_duplication_levels.columns = ['sdl_'+str(col) for col in sequence_duplication_levels.columns]
#Add os to all columns in Overrepresented_sequences
Overrepresented_sequences.columns = ['os_'+str(col) for col in Overrepresented_sequences.columns]
#Add fsch to all columns in fastqc_status_check_heatmap
fastqc_status_check_heatmap.columns = ['fsch_'+str(col) for col in fastqc_status_check_heatmap.columns]




#Merging all dataframes into one
df = general_stats.join(sequence_counts)
df = df.join(per_base_sequence_quality)
df = df.join(per_base_n_content)
df = df.join(per_sequence_gc_content)
df = df.join(per_sequence_gc_content_percentage)
df = df.join(per_sequence_quality_scores)
df = df.join(sequence_length_distribution)
df = df.join(sequence_duplication_levels)
df = df.join(Overrepresented_sequences)
df = df.join(fastqc_status_check_heatmap)
#Change Sample into Run
df.index.names = ['Run']
print("Saving into ../multiqc_data.csv file in results folder")
df.to_csv('../multiqc_data.csv',index=True)


print("Feature extraction has completed successfully, see the multiqc_data.csv file generated in results directory 😊 ")


#Here merging metadata with execution_times.csv files Run
meta=pd.read_csv("../../metadata/metadata.csv")
execution=pd.read_csv("../execution_times.csv")
execution.columns = execution.columns.str.replace('Sample', 'Run')
execution.columns = execution.columns.str.replace('RealTime', 'Label')
print("Combining metadata with multiqc reports output")
metadata = pd.merge(meta, execution, on='Run', how='inner')
print(metadata.columns)
multiqc=df
merged = pd.merge(metadata, multiqc,on='Run')
print("After merging execution time, metadata and multiqc data, it looks like")
print(merged.head())


#Removing columns with no data
merged = merged.dropna(axis=1, how='all')
#Removing columns with only one unique value
merged = merged.loc[:,merged.apply(pd.Series.nunique) != 1]
#Removing columns with more than 40% missing values
merged = merged.loc[:, merged.isnull().mean() < .4]
#Removing columns with more than 40% zeros
merged = merged.loc[:, (merged == 0).mean() < .4]
#Remove non-numeric columns
merged = merged.select_dtypes(include=[np.number])
#put Label column at the end in merged dataframe
merged = merged[[c for c in merged if c not in ['Label']] + ['Label']]
print("Merged dataframe looks like below, 😊")

print(merged.head())

#Replace nan with mean of that column
merged = merged.fillna(merged.mean())
#Extract features which has 0.5 correlation with Label
correlation = merged.corr()
correlation = correlation['Label'].sort_values(ascending=False)
print("Here we are interested in only features which has correlation more thatn 0.7 with labels")
print("Please change in code here to change the correlation value")
features = correlation.index[abs(correlation)>0.7]
features = features.drop('Label')
print("The most correlated features are:")
print(features)
df=merged[features]
#add labels to the dataframe
df['Label'] = merged['Label']
df.to_csv("../Processed_data.csv")

