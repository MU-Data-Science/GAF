import os
import pandas as pd
from datetime import datetime
import logging
 
# Create and configure logger
def get_logger(stage):
    logging.basicConfig(filename=f"{stage}.log",
                        format='%(asctime)s - %(levelname)s - %(message)s',
                        filemode='w')
    
    # Creating an object
    logger = logging.getLogger()
    
    # Setting the threshold of logger to DEBUG
    logger.setLevel(logging.DEBUG)
    
    return logger

def concat_data(filepath):
    if os.path.isfile(filepath):
        print(f"Reading: {filepath}")
        df = pd.read_parquet(filepath)
    else:
        for root, dirs, files in os.walk(filepath):
            print(f"Reading: {os.path.join(root, files[0])}")
            df = pd.read_parquet(os.path.join(root, files[0]))
            for f in files[1:]:
                print(f"Reading: {os.path.join(root, f)}")
                df_temp = pd.read_parquet(os.path.join(root, f))
                df = pd.concat([df, df_temp], axis=0)

    df = df.drop_duplicates().reset_index(drop=True)
    accession_id_variant_id_freq = df.groupby('accession_id')['variant_id'].count()
    min_variant_id_freq = min(accession_id_variant_id_freq)
    variant_id_counts_df = pd.DataFrame(accession_id_variant_id_freq)
    variant_id_counts_df['accession_id'] = variant_id_counts_df.index
    threshold = int(f"{int(str(min_variant_id_freq)[0]) + 1}{'0' * (len(str(min_variant_id_freq)) - 1)}")
    shortlisted_accession_ids = variant_id_counts_df.loc[variant_id_counts_df.variant_id < threshold]['accession_id'].tolist()
    df = df.loc[df['accession_id'].isin(shortlisted_accession_ids)]
    return df

def get_str_datetime():
    now = datetime.now() # current date and time
    return now.strftime("%m_%d_%Y_%H_%M_%S")

if __name__=="__main__":
    import sys
    concat_data(sys.argv[1])