import os
import gc
import pandas as pd
# import dask as dd
import numpy as np
from tqdm import tqdm
from local_utils import concat_data, get_logger
from input_dataclasses import GraphCreationConfig
import gradio as gr


logger = get_logger("create_graph")

def create_dataset_ann_impact(config: GraphCreationConfig, progress: gr.Progress):

    PATH_TO_SAVE_FILES = config.output_dirpath
    os.makedirs(PATH_TO_SAVE_FILES, exist_ok=True)

    progress(0, desc="Loading files")
    nodes_data = concat_data(config.input_file_or_dirpath)

    # b = pd.DataFrame(nodes_data.groupby('accession_id')['variant_id'].count())
    # b['a_id'] = b.index
    # b.loc[b.variant_id < 2427]['variant_id']
    
    # import pdb; pdb.set_trace()
    
    nodes_data = nodes_data.sample(frac=min(1, (10000/len(nodes_data))), random_state=42, ignore_index=True, replace=False)
    print("nodesdata", len(nodes_data))
    
    feats_data =  nodes_data[nodes_data.variant_id != 'None']

    logger.debug("PROGRESS --------- Saving the concatenated dataset")
    feats_data.to_parquet(f'{PATH_TO_SAVE_FILES}/feats_data.parquet')

    logger.debug(f"INFO --------- Number of Accession IDs: {len(feats_data.accession_id.unique())} \n Number of unique variant IDs before explosion: {len(feats_data.variant_id.unique())}")

    unique_variant_ids = set()
    unique_variant_ids = list(feats_data['variant_id'].str.split(';').explode('variant_id').unique())

    # Prepare the source origin DF
    logger.debug("PROGRESS --------- Preparing the source dataframe for ORIGIN IDs")
    if len(config.selected_features) == 0:
        config.selected_features = list(feats_data.columns)
    feats_data = feats_data[config.selected_features]
    accession_id_df = pd.DataFrame(0, index=np.arange(len(feats_data)), columns=feats_data['accession_id'].unique(), dtype=np.int8)

    # import dask.dataframe as dd
    # merge_dataframe = dd.from_pandas(pd.concat([feats_data, accession_id_df], axis=1), npartitions=10)

    feats_data['position'] = feats_data.position.astype(np.int32)
    feats_data.reset_index(inplace=True)
    merge_dataframe = pd.concat([feats_data, accession_id_df], axis=1)

    # nodes_data = None
    feats_data = None
    accession_id_df = None
    gc.collect()

    merge_dataframe['variant_id'] = merge_dataframe['variant_id'].str.split(';')
    merge_dataframe = merge_dataframe.explode('variant_id')

    logger.debug(f"INFO --------- Number of unique variant IDs after explosion: {len(merge_dataframe.variant_id.unique())}")

    # Prepare the destination variant_id DF
    logger.debug("PROGRESS --------- Preparing the destination dataframe for VARIANT IDs")
    variant_id_feats_df = pd.DataFrame(index=range(len(unique_variant_ids)), columns=merge_dataframe.columns)

    for col in progress.tqdm(variant_id_feats_df.columns, desc="Updating dataframe"):
        if col not in ['origin', 'variant_id']:
            variant_id_feats_df[col].values[:] = 999999

    variant_id_feats_df['variant_id'] = list(unique_variant_ids)

    logger.debug("PROGRESS --------- Saving the variant ID and origin ID datasets")
    variant_df_path = f'{PATH_TO_SAVE_FILES}/variantdf.parquet'
    variant_id_feats_df.to_parquet(variant_df_path, index=False)

    # for i in range(merge_dataframe.npartitions):
    #     merge_dataframe.partitions[i].compute().to_parquet(f"/mydata/dgl/general/Code/Generated-Files/Origin/file_{i}.parquet", engine='pyarrow')
    origin_df_path = f'{PATH_TO_SAVE_FILES}/origindf.parquet'
    merge_dataframe.to_parquet(origin_df_path, index=False)

    return variant_df_path, origin_df_path, PATH_TO_SAVE_FILES

# create_dataset_ann_impact('/mydata/dgl/general/Code/Data/')
    