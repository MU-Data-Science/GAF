import os, gc, time
import json
import pandas as pd
import numpy as np
from pymantic import sparql
import requests
from collections import OrderedDict
from tqdm import tqdm

from input_dataclasses import GraphEnrichmentConfig
import gradio as gr
from local_utils import get_logger
from typing import List

from constants import AGE_GROUP_FILTER_MAPPING

logger = get_logger("enrich_graph")

def check_connection(config: GraphEnrichmentConfig) -> bool:
    url = f'http://{config.blazegraph_ip_address}:9999/blazegraph/sparql'
    
    headers = {
        'Accept': 'application/json',
    }
    
    data_1 = {
        'query': 'select * where { ?s ?p ?o } LIMIT 1',
    }
    
    try:
        result_1 = requests.post(url, headers=headers, data=data_1)
        return len(result_1.json()['results']['bindings']) == 1
    except Exception as e:
        print(e)
    return False

def fetch_accession_by_age_group(config: GraphEnrichmentConfig) -> List[str]:
    age_group_filter_str = " || ".join([AGE_GROUP_FILTER_MAPPING[item] for item in config.age_group])
    
    url = f'http://{config.blazegraph_ip_address}:9999/blazegraph/sparql'
    headers = {
        'Accept': 'application/json',
    }
    
    # (?age < 25) || (?age >= 25 && ?age < 50) || (?age >= 50 && ?age < 75) || (?age >= 75)
    print(age_group_filter_str)
    data = {
        'query': f"PREFIX wdt: <https://www.wikidata.org/wiki/> PREFIX sg: <sg://> SELECT DISTINCT ?accession_id WHERE {{ GRAPH ?accession_id {{ ?sraURL wdt:Q11904283 ?age . }} FILTER ( ({age_group_filter_str}) ) }}",
    }
    
    print(f"PREFIX wdt: <https://www.wikidata.org/wiki/> PREFIX sg: <sg://> SELECT DISTINCT ?accession_id WHERE {{ GRAPH ?accession_id {{ ?sraURL wdt:Q11904283 ?age . }} FILTER ( ({age_group_filter_str}) ) }}")
    
    result = requests.post(url, headers=headers, data=data)
    print(f"Fetching {len(result.json()['results']['bindings'])} accession IDs")
    
    accession_id_set = set()

    for key in result.json()['results']['bindings']:
        accession_id = key['accession_id']['value'].split('//')[-1]
        
        accession_id_set.add(accession_id)
    
    return list(accession_id_set)

def get_unique_accession_ids(config: GraphEnrichmentConfig) -> List[str]:
    print(config.blazegraph_ip_address)
    url = f'http://{config.blazegraph_ip_address}:9999/blazegraph/sparql'
    selected_accessionid_list = config.accession_ids
    print(config)
    
    headers = {
        'Accept': 'application/json',
    }
    
    data = {
        'query': 'PREFIX sg_biohackathon:<http://biohackathon.org/resource/faldo#> SELECT DISTINCT ?accession_id WHERE { GRAPH ?accession_id { ?origin sg_biohackathon:reference ?chromosome . } } LIMIT 51',
    }
    
    result = requests.post(url, headers=headers, data=data)

    print(f"Fetching {len(result.json()['results']['bindings'])} accession IDs")
    
    accession_id_set = set()

    for key in result.json()['results']['bindings']:
        accession_id = key['accession_id']['value'].split('//')[-1]
        
        accession_id_set.add(accession_id)
    return list(accession_id_set)

def create_query_for_chosen_features(config: GraphEnrichmentConfig, progress: gr.Progress):
    
    prefixes = """
    PREFIX sg_biohackathon:<http://biohackathon.org/resource/faldo#>
    PREFIX sg_variant:<sg://0.99.11/vcf2rdf/>
    PREFIX sg_vcf:<sg://0.99.11/vcf2rdf/variant/>
    PREFIX sg_info:<sg://0.99.11/vcf2rdf/info/>
    PREFIX sg_format:<sg://0.99.11/vcf2rdf/format/>
    PREFIX sg_format_gt:<sg://0.99.11/vcf2rdf/format/GT/>
    PREFIX ns1:<http://sg.org/>
    """
    
    base_query = """
    SELECT DISTINCT ?accession_id ?origin (COALESCE(?variant_id, "None") AS ?variant_id) ?chromosome ?position ?ref_genome ?alt_genome ?quality ?ann ?ann_split_1
    """

    select_fields = " ".join([f"?{field}" for field, include in config.optional_fields.items() if include and field != 'accession_id'])

    where_clause = """
    WHERE {  
    GRAPH ?accession_id {    
        OPTIONAL { ?origin sg_variant:variantId ?variant_id . }    
        BIND (COALESCE(?variant_id, "None") AS ?variant_id)    
        ?origin sg_biohackathon:reference ?chromosome .    
        ?origin sg_biohackathon:position ?position .    
        ?origin sg_vcf:REF ?ref_genome .     
        ?origin sg_vcf:ALT ?alt_genome .     
        ?origin sg_vcf:QUAL ?quality .    
        ?origin sg_info:ANN ?ann .    
        BIND (IF(STRLEN(?ann) - STRLEN(REPLACE(?ann, ",", "")) = 0, ?ann, STRBEFORE(?ann, ",")) AS ?ann_split_1)
    """

    if len(config.age_group) > 0:
        config.accession_ids = fetch_accession_by_age_group(config)
        if len(config.accession_ids) == 0:
            progress(1.0, desc="No accession IDs available for this age group!")
            time.sleep(2)
            
    elif len(config.accession_ids) == 0:
        config.accession_ids = get_unique_accession_ids(config)
                        
    print(config.accession_ids)
    for field, include in config.optional_fields.items():
        if include and field != 'accession_id':
            where_clause += f"OPTIONAL {{ ?origin sg_info:{field.upper()} ?{field} . }}\n"

    accession_ids_str = ", ".join([f"<sg://{acc_id}>" for acc_id in config.accession_ids])
    
    where_clause += """
    }   
    ?origin sg_biohackathon:position ?position .  
    OPTIONAL { ?variant <http://sg.org/has_pos> ?position . }  
    ?origin sg_vcf:REF ?ref_genome .  
    OPTIONAL { ?variant <http://sg.org/has_ref_genome> ?ref_genome . }  
    ?origin sg_vcf:ALT ?alt_genome .  
    OPTIONAL { ?variant <http://sg.org/has_alt_genome> ?alt_genome . }  
    OPTIONAL { ?variant <http://sg.org/has_cadd_scores> ?cadd_scores . }  
    OPTIONAL { ?cadd_scores <http://sg.org/has_raw_score> ?raw_score . }  
    OPTIONAL { ?cadd_scores <http://sg.org/has_phred> ?phred_score . }
    FILTER (?accession_id IN (""" + accession_ids_str + """))
    } ORDER BY ?variant_id
    """

    query = prefixes + base_query + select_fields + where_clause
    
    return query

def get_chosen_features(config: GraphEnrichmentConfig, blazegraph_query: str, progress: gr.Progress):
    
    url = f'http://{config.blazegraph_ip_address}:9999/blazegraph/sparql'

    headers = {
        'Accept': 'application/json',
    }

    # data_1 = {
    #     'query': f"PREFIX sg_biohackathon:<http://biohackathon.org/resource/faldo#> PREFIX sg_variant:<sg://0.99.11/vcf2rdf/> PREFIX sg_vcf:<sg://0.99.11/vcf2rdf/variant/> PREFIX sg_info:<sg://0.99.11/vcf2rdf/info/> PREFIX sg_format:<sg://0.99.11/vcf2rdf/format/> PREFIX sg_format_gt:<sg://0.99.11/vcf2rdf/format/GT/> PREFIX ns1:<http://sg.org/> SELECT DISTINCT ?accession_id ?origin (COALESCE(?variant_id, "None") AS ?variant_id) ?chromosome ?position ?ref_genome ?alt_genome ?quality ?ann_split_1 ?"{filter_status}" ?"{ac}" ?"{af}" ?"{an}" ?"{baseqranksum}" ?"{depth}" ?"{excesshet}" ?"{fs}" ?"{mleac}" ?"{mleaf}" ?"{mq}" ?"{qd}" ?"{readposranksum}" ?"{sor}" ?"{dp}" ?"{gq}" ?"{gt}" ?"{raw_score}" ?"{phred_score}" WHERE {{ GRAPH ?accession_id { OPTIONAL { ?origin sg_variant:variantId ?variant_id . } BIND (COALESCE(?variant_id, "None") AS ?variant_id) ?origin sg_biohackathon:reference ?chromosome . ?origin sg_biohackathon:position ?position . ?origin sg_vcf:REF ?ref_genome . ?origin sg_vcf:ALT ?alt_genome . ?origin sg_vcf:QUAL ?quality . ?origin sg_info:ANN ?ann . BIND (IF(STRLEN(?ann) - STRLEN(REPLACE(?ann, ",", "")) = 0, ?ann, STRBEFORE(?ann, ",")) AS ?ann_split_1) OPTIONAL { ?origin sg_vcf:FILTER ?_status . } OPTIONAL { ?origin sg_info:AC ?ac . } OPTIONAL { ?origin sg_info:AF ?af . } OPTIONAL { ?origin sg_info:AN ?an . } OPTIONAL { ?origin sg_info:BaseQRankSum ?baseqranksum . } OPTIONAL { ?origin sg_info:DP ?depth . } OPTIONAL { ?origin sg_info:ExcessHet ?excesshet . } OPTIONAL { ?origin sg_info:FS ?fs . } OPTIONAL { ?origin sg_info:MLEAC ?mleac . } OPTIONAL { ?origin sg_info:MLEAF ?mleaf . } OPTIONAL { ?origin sg_info:MQ ?mq . } OPTIONAL { ?origin sg_info:QD ?qd . } OPTIONAL { ?origin sg_info:ReadPosRankSum ?readposranksum . } OPTIONAL { ?origin sg_info:SOR ?sor . } OPTIONAL { ?origin sg_format:DP ?dp . } OPTIONAL { ?origin sg_format:GQ ?gq . } OPTIONAL { ?origin sg_format_gt:allele_1 ?gt . } } ?origin sg_biohackathon:position ?position . OPTIONAL { ?variant <http://sg.org/has_pos> ?position . } ?origin sg_vcf:REF ?ref_genome . OPTIONAL { ?variant <http://sg.org/has_ref_genome> ?ref_genome . } ?origin sg_vcf:ALT ?alt_genome . OPTIONAL { ?variant <http://sg.org/has_alt_genome> ?alt_genome . } OPTIONAL { ?variant <http://sg.org/has_cadd_scores> ?cadd_scores . } OPTIONAL { ?cadd_scores <http://sg.org/has_raw_score> ?raw_score . } OPTIONAL { ?cadd_scores <http://sg.org/has_phred> ?phred_score . }}",
    # }
    
    progress(0, desc="Querying Blazegraph")
    data = {
        'query': blazegraph_query
    }
    # import pdb; pdb.set_trace()
    # progress(0.1, desc=query.replace("\n", "<br>"))
    result = requests.post(url, headers=headers, data=data)
    logger.debug(f"👉 Finished fetching {len(result.json()['results']['bindings'])}")

    data_list = []
    alternate_value_for_missing_values = np.NaN
    
    for key in progress.tqdm(result.json()['results']['bindings'], desc="Parsing query results"):
        accession_id = key['accession_id']['value'].split('/')[-1]
        origin = key['origin']['value']
        variant_id = key['variant_id']['value']
        chromosome = key['chromosome']['value'].split('/')[-1]
        position = key['position']['value']
        ref_genome = key['ref_genome']['value']
        alt_genome = key['alt_genome']['value']
        quality = key['quality']['value']
        ann = key['ann']['value']
        ann_allele = key['ann_split_1']['value'].split('|')[0] 
        ann_annotation = key['ann_split_1']['value'].split('|')[1]
        ann_impact = key['ann_split_1']['value'].split('|')[2]
        ann_gene_name = key['ann_split_1']['value'].split('|')[3]
        ann_gene_id = key['ann_split_1']['value'].split('|')[4]
        ann_feature_type = key['ann_split_1']['value'].split('|')[5]
        ann_feature_id = key['ann_split_1']['value'].split('|')[6]
        ann_transcript_biotype = key['ann_split_1']['value'].split('|')[7]
        ann_rank = key['ann_split_1']['value'].split('|')[8]
        filter_status = key['filter_status']['value'].split('/')[-1] if 'filter_status' in key else alternate_value_for_missing_values
        allele_count = key['ac']['value'] if 'ac' in key else alternate_value_for_missing_values
        allele_frequency = key['af']['value'] if 'af' in key else alternate_value_for_missing_values
        total_number_of_alleles= key['an']['value'] if 'an' in key else alternate_value_for_missing_values
        baseqranksum = key['baseqranksum']['value'] if 'baseqranksum' in key else alternate_value_for_missing_values
        depth = key['depth']['value'] if 'depth' in key else alternate_value_for_missing_values
        excesshet = key['excesshet']['value'] if 'excesshet' in key else alternate_value_for_missing_values
        fs = key['fs']['value'] if 'fs' in key else alternate_value_for_missing_values
        mleac = key['mleac']['value'] if 'mleac' in key else alternate_value_for_missing_values
        mleaf = key['mleaf']['value'] if 'mleaf' in key else alternate_value_for_missing_values
        RMS_mapping_quality = key['mq']['value'] if 'mq' in key else alternate_value_for_missing_values
        qd = key['qd']['value'] if 'qd' in key else alternate_value_for_missing_values
        readposranksum = key['readposranksum']['value'] if 'readposranksum' in key else alternate_value_for_missing_values
        sor = key['sor']['value'] if 'sor' in key else alternate_value_for_missing_values
        combined_depth = key['dp']['value'] if 'dp' in key else alternate_value_for_missing_values
        conditional_genotype_quality = key['gq']['value'] if 'gq' in key else alternate_value_for_missing_values
        genotype = key['gt']['value'] if 'gt' in key else alternate_value_for_missing_values
        raw_score = key['raw_score']['value'] if 'raw_score' in key else alternate_value_for_missing_values
        phred_score = key['phred_score']['value'] if 'phred_score' in key else alternate_value_for_missing_values
        
        
        data_list.append([accession_id, origin, variant_id, chromosome, position, ref_genome, alt_genome, quality, ann, ann_allele, ann_annotation, ann_impact, ann_gene_name, ann_gene_id, ann_feature_type, ann_feature_id, ann_transcript_biotype, ann_rank, filter_status, allele_count, allele_frequency, total_number_of_alleles, baseqranksum, depth, excesshet, fs, mleac, mleaf, RMS_mapping_quality, qd, readposranksum, sor, combined_depth, conditional_genotype_quality, genotype, raw_score, phred_score])
        
        # if len(data_list) % 1000 == 0:
        #     logger.debug("Wrote: ", len(data_list))
    
    progress(0, desc="Saving the parquet file")
    logger.debug("Completed fetching the selected features!")
    return dump_data(data_list, config.fetched_data_path)

def dump_data(data_list, fetched_data_path):
    node_cols=['accession_id', 'origin', 'variant_id', 'chromosome', 'position', 'ref_genome', 'alt_genome', 'quality', 'ann', 'ann_allele', 'ann_annotation', 'ann_impact', 'ann_gene_name', 'ann_gene_id', 'ann_feature_type', 'ann_feature_id', 'ann_transcript_biotype', 'ann_rank', 'filter_status', 'allele_count', 'allele_frequency', 'total_number_of_alleles', 'baseqranksum', 'depth', 'excesshet', 'fs', 'mleac', 'mleaf', 'RMS_mapping_quality', 'qd', 'readposranksum', 'sor', 'combined_depth', 'conditional_genotype_quality', 'genotype', 'raw_score', 'phred_score']
    
    df = pd.DataFrame(data_list, columns=node_cols)
    df.dropna(axis=1, how='all', inplace=True)
    df.to_parquet(fetched_data_path, index=False)
    return fetched_data_path, list(df.columns)


# get_chosen_features(blazegraph_ip_address='10.10.1.1', config.optional_fields=config.optional_fields, fetched_data_path='/mydata/dgl/general/data.parquet')