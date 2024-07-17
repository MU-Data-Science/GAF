VCF_FILE_UPLOADS_PATH = "/mydata/GAF/VariantKG/codebase/Uploads"
SNPEFF_JAR_PATH = '/mydata/GAF/VariantKG/codebase/snpEff/snpEff.jar'
SPARQLING_GEN_PATH = '/mydata/GAF/VariantKG/codebase/sparqling-genomics-0.99.11/tools/vcf2rdf/vcf2rdf'

DEFAULT_BLAZEGRAPH_SERVER_IP = '10.10.1.1'
DEFAULT_BLAZEGRAPH_INPUT_DIR = '/mydata/GAF/VariantKG/codebase/Processed'
BLAZEGRAPH_JAR_PATH = '/mydata/GAF/VariantKG/codebase/blazegraph.jar'
QUAD_PROPERTIES_PATH = '/mydata/GAF/VariantKG/codebase/quad.properties'
TURTLE_PROPERTIES_PATH = '/mydata/GAF/VariantKG/codebase/turtle.properties'

EDGE_TYPE_FULLY_CONNECTED = "Fully Connected"
EDGE_TYPE_GENE = "Gene Name"
EDGE_WEIGHT_TYPE_INDEGREE = "INDEGREE"
EDGE_WEIGHT_TYPE_UNIFORM = "UNIFORM"

AGE_GROUP_FILTER_MAPPING = {'Less than 25': '(?age < 25)', '25 - 50': '(?age >= 25 && ?age < 50)', '50 - 75': '(?age >= 50 && ?age < 75)', 'Greater than 75': '(?age >= 75)'}

POSSIBLE_CLASS_LABELS = set(['ann_impact', 'ann_feature_type', 'ann_transcript_biotype', 'mleac', 'mleaf'])