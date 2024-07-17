from pydantic import BaseModel, Field, computed_field, conint
from typing import List, Dict
import os
from pandas import DataFrame
from local_utils import get_str_datetime
from constants import DEFAULT_BLAZEGRAPH_SERVER_IP, EDGE_TYPE_GENE

class GraphEnrichmentConfig(BaseModel):
    
    blazegraph_ip_address: str = Field(default=DEFAULT_BLAZEGRAPH_SERVER_IP)
    blazegraph_pid: int = Field(default=-1)
    age_group: List[str] = Field(default=[])
    accession_ids: List[str] = Field(default=[])
    optional_fields: dict = Field(default={
        "accession_id": True,
        "filter_status": False,
        "allele_count": False,
        "allele_frequency": False,
        "total_number_of_alleles": False,
        "baseqranksum": False,
        "depth": False,
        "excesshet": False,
        "fs": False,
        "mleac": False,
        "mleaf": False,
        "RMS_mapping_quality": False,
        "qd": False,
        "readposranksum": False,
        "sor": False,
        "combined_depth": False,
        "conditional_genotype_quality": False,
        "genotype": False,
        "raw_score": False,
        "phred_score": False
    })
    save_base_dirpath: str = Field(default='/mydata/dgl/general/Data/')
    
    def set_optional_fields(self, field_name:str) -> None:
        if field_name in self.optional_fields:
            self.optional_fields[field_name] = True
    
    @computed_field
    @property
    def fetched_data_path(self) -> str:
        os.makedirs(self.save_base_dirpath, exist_ok=True)
        
        return os.path.join(self.save_base_dirpath, f"custom_data_{get_str_datetime()}.parquet")
    
class GraphCreationConfig(BaseModel):
    
    input_file_or_dirpath: str = Field(default="")
    output_dirpath: str = Field(default=os.path.normpath(os.getcwd() + os.sep + os.pardir)+'/GENERATED-FILES')
    selected_features: List[str] = Field(default=['accession_id', 'origin', 'variant_id', 'chromosome', 'ref_genome', 'alt_genome', 'position', 'quality', 'ann_impact', 'ann'])
    class_column: str = Field(default="ann_impact")
    class_map: Dict = Field(default={
        'HIGH': 2, 
        'LOW': 0, 
        'MODERATE': 1, 
        'MODIFIER': 3, 
        999999: 999999
    })
    number_of_classes: int = Field(default=4)
    train_size: float = Field(default=0.8)
    val_size: float = Field(default=0.1)
    # add_self_loop: bool = Field(default=False)
    add_bidirectional_edges: bool = Field(default=True)
    edge_type: str = Field(default=EDGE_TYPE_GENE)
    edge_weight_type: str = Field(default="UNIFORM")
    edge_weight_value: int = Field(default=1)
    
    graph_metadata: Dict = Field(default={})
    
    def to_dataframe(self) -> DataFrame:
        json_version = self.model_dump()
        df_compatible_json = []
        for k, v in json_version.items():
            if k == "graph_metadata":
                continue
            if v == self.selected_features:
                print("Loaded graph info", "*"*100)
                v = list(set(self.selected_features).difference(set([self.class_column])))
            df_compatible_json.append({
                'configuration name': k,
                'value': str(v)
            })
        
        for k, v in self.graph_metadata.items():
            df_compatible_json.append({
                'configuration name': k,
                'value': str(v)
            })
            
        return DataFrame(df_compatible_json)

class GraphTrainerConfig(BaseModel):
    
    input_graph_path: str = Field(default="")
    
    model_selection: str = Field(default='sage') 
    number_of_classes: int = Field(default=4)
    number_of_layers: int = Field(default=2)
    dropout_rate: float = Field(default=0.0)
    number_of_epochs: conint(ge=1) = Field(default=500)
    number_of_hidden_layers: int = Field(default=16)
    learning_rate: float = Field(default=0.01)
    pickle_file: str = Field(default='')
    
    # training_statistics_filename: str = Field(default="training_stats.json")
    
    @computed_field
    @property
    def output_dirpath(self) -> str:
        return os.path.normpath(os.getcwd() + os.sep + os.pardir) + f'/MODEL-{self.number_of_epochs}-{self.number_of_hidden_layers}'
    
    @computed_field
    @property
    def training_statistics_filename(self) -> str:
        os.makedirs(self.output_dirpath, exist_ok=True)
        
        return os.path.normpath(os.getcwd() + os.sep + os.pardir) + f'/MODEL-{self.number_of_epochs}-{self.number_of_hidden_layers}' + "training_stats.json"
        
      