import numpy as np
import gradio as gr
import sys
from input_dataclasses import GraphEnrichmentConfig, GraphCreationConfig, GraphTrainerConfig
import pandas as pd
import os
from time import perf_counter_ns
from constants import (
    VCF_FILE_UPLOADS_PATH,
    DEFAULT_BLAZEGRAPH_SERVER_IP,
    EDGE_TYPE_FULLY_CONNECTED,
    EDGE_TYPE_GENE,
    EDGE_WEIGHT_TYPE_INDEGREE,
    EDGE_WEIGHT_TYPE_UNIFORM,
    AGE_GROUP_FILTER_MAPPING,
    POSSIBLE_CLASS_LABELS
)
import gradio as gr
from PIL import Image
import glob
from create_graph import create_graph
from feature_generator import get_chosen_features, get_unique_accession_ids, check_connection, create_query_for_chosen_features
import json
import logging
import trainer as tr
import psutil
from process_uploads import convert_to_nq_or_ttl
from graph_storage import start_blazegraph, store_data_on_blazegraph
from get_confusion_matrix import draw_confusion_matrix
from string import Template
import warnings
import altair as alt
import pandas as pd
from GNNExplainer import main
warnings.filterwarnings("ignore")
js_func = """
function refresh() {
    const url = new URL(window.location);

    if (url.searchParams.get('__theme') == 'dark') {
        url.searchParams.set('__theme', 'light');
        window.location.href = url.href;
    }
}
"""

blazegraph_template = Template("""
<p style="height: 300px; overflow-y: scroll;">
$content
</p>
""")

# import shutil

# shutil.rmtree(VCF_FILE_UPLOADS_PATH)
# os.remove('/mydata/dgl/general/private-code-cloudlab/codebase/blazegraph.jnl')

config = GraphCreationConfig()
trainer_config = GraphTrainerConfig()
enrichment_config = GraphEnrichmentConfig()

features = []
seconds = []
memory_used = []

create_graph_button_clicked_flag = False
upload_vcf_button_clicked_flag = False

base_file_dir = "/mydata/GAF/VariantKG/codebase/Data/"
log_file_name = "create_graph.log"
graph_output_path = ""
create_graph_input_path = ""
blazegraph_pid = -1
blazegraph_query = None
logging.getLogger('gradio').setLevel(logging.ERROR)

feature_choices_custom = ['accession_id', 'origin', 'variant_id', 'chromosome', 'position', 'ref_genome', 'alt_genome', 'quality', 'ann_allele', 'ann_annotation', 'ann_impact', 'ann_gene_name', 'ann_gene_id', 'ann_feature_type', 'ann_feature_id', 'ann_transcript_biotype', 'ann_rank', 'filter_status', 'allele_count', 'allele_frequency', 'total_number_of_alleles', 'baseqranksum', 'depth', 'excesshet', 'fs', 'mleac', 'mleaf', 'RMS_mapping_quality', 'qd', 'readposranksum', 'sor', 'combined_depth', 'conditional_genotype_quality', 'genotype', 'raw_score', 'phred_score']

def custom_data_button_1_click():
    return gr.Tabs(selected=0), gr.update(visible=True), gr.update(visible=True), gr.update(visible=False), gr.update(visible=True), gr.update(visible=False)

def our_data_button_2_click(progress: gr.Progress = gr.Progress()):
    global enrichment_config
    if not check_connection(enrichment_config):
        enrichment_config.blazegraph_pid = start_blazegraph(progress)
    unique_accession_ids = get_unique_accession_ids(enrichment_config)
    enrichment_config.accession_ids = unique_accession_ids
    return gr.Tabs(selected=1), gr.update(visible=False), gr.update(visible=True), gr.update(visible=True), gr.update(visible=True), gr.update(visible=False), gr.update(visible=True), gr.update(visible=True), gr.update(choices=unique_accession_ids, value=[]), gr.update(visible=True), gr.update(choices=feature_choices_custom, value=feature_choices_custom, visible=True), gr.update(visible=False), gr.update(visible=False), gr.update(value="")

def load_graph_button_3_click():
    return gr.Tabs(selected=2), gr.update(visible=False), gr.update(visible=False), gr.update(visible=True), gr.update(visible=True), gr.update(visible=False)

def update_button_status():
    button_type_map = {True: "secondary", False: "primary"}
    return gr.update(interactive=not upload_vcf_button_clicked_flag, variant=button_type_map[upload_vcf_button_clicked_flag]), gr.update(interactive=not create_graph_button_clicked_flag, variant=button_type_map[create_graph_button_clicked_flag])

def show_blazegraph_query():
    if blazegraph_query is not None:
        return gr.update(value=blazegraph_template.substitute(content=blazegraph_query.replace('<', '&lt;').replace('>', '&gt;').replace("\n", "<br>")), visible=True)
    return gr.update(value="", visible=False)

#Tab 1 functions

def upload_file(files, progress=gr.Progress()):
    global enrichment_config
    total_start_time = perf_counter_ns()
    
    os.makedirs(VCF_FILE_UPLOADS_PATH, exist_ok=True)
    if isinstance(files, str):
        files = [files]
    uploaded_file_paths = []
    for file in files:
        progress(0, desc=f"Uploading {file.split('/')[-1]}")
        uploaded_file_paths.append(os.path.join(VCF_FILE_UPLOADS_PATH, file.split('/')[-1]))
        with open(os.path.join(VCF_FILE_UPLOADS_PATH, file.split('/')[-1]), 'w') as f:
            f.write(open(file).read())
    
    progress(0, desc="Converting uploaded files to N3/TTL")
    processed_output_paths = convert_to_nq_or_ttl(uploads_path=uploaded_file_paths, progress=progress)
    
    progress(0, desc="Transferring to Blazegraph")
    enrichment_config.blazegraph_pid = store_data_on_blazegraph(
        input_path=processed_output_paths,
        progress=progress,
        config=enrichment_config)
    
    unique_accession_ids = get_unique_accession_ids(enrichment_config)
    enrichment_config.accession_ids = unique_accession_ids
    total_stop_time = perf_counter_ns()
    print(f"Total time taken to upload VCFs, perform annotation on vcf, converting into N3 and then converting into NQ After that file uploads into Blazegraph (upload vcf to vcfs in blazegraph): {((total_stop_time-total_start_time)*1e-9)} seconds")
    
    return gr.update(choices=unique_accession_ids, visible=False, value=[]), gr.update(visible=True, interactive=True),  gr.update(choices=feature_choices_custom, value=feature_choices_custom, visible=True), "Finished converting files and adding to Blazegraph!", gr.Tabs(selected=1)

def selection_criteria_toggle_selector(selected_option):
    if selected_option == "Age group":
        enrichment_config.accession_ids = []
        return gr.update(visible=True, interactive=True, value=[]), gr.update(visible=False, value=[])
    else:
        enrichment_config.age_group = []
        return gr.update(visible=False, value=[]), gr.update(visible=True, value=[])

def age_filter_selector_change_handler(age_filter_selector):
    global enrichment_config
    enrichment_config.age_group = age_filter_selector
    
def accession_id_selector_change_handler(accession_id_selector):
    global enrichment_config
    print("in accessionID selector change func")
    enrichment_config.accession_ids = accession_id_selector

def feature_selector_custom_change(feature_selector_custom):
    for field in feature_selector_custom:
      enrichment_config.set_optional_fields(field)

def feature_generator_click():
    global enrichment_config, features, create_graph_input_path, blazegraph_query
    enrichment_config.blazegraph_ip_address = DEFAULT_BLAZEGRAPH_SERVER_IP
    progress = gr.Progress()
    progress(0, desc="Starting the generation")
    blazegraph_query = create_query_for_chosen_features(enrichment_config, progress)
    create_graph_input_path, features = get_chosen_features(enrichment_config, blazegraph_query, progress)
    del(features[features.index('ann')])
    
    blazegraph_query = None
    return gr.update(choices=features, value=features), gr.update(visible=True), gr.update(visible=True), gr.update(visible=False), gr.update(visible=False), gr.update(value="Completed!", lines=1)

#Tab 2 functions
# def update_self_loop(self_loop_checkbox):
#     config.add_self_loop = self_loop_checkbox

def select_edge_type(edge_type):
    config.edge_type = edge_type
    
def update_bidirectional(bidirectional_checkbox):
    config.add_bidirectional_edges = bidirectional_checkbox
    
def file_loader(file_selector):
    global features
    df = pd.read_parquet(os.path.join(base_file_dir, file_selector))
    features = list(df.columns)
    return {
        feature_selector: gr.CheckboxGroup(
            choices=features,
            label="Feature selector",
            info="Select features to extract for graph creation",
            value=features
        )
    }

def feature_selector_change(feature_selector):
    class_choices = list(set(features).difference(set(feature_selector)).intersection(POSSIBLE_CLASS_LABELS))
    return gr.Dropdown(
            choices=class_choices,
            label="Class label selector",
            info="Select the class label to use",
            value=(class_choices[0] if len(class_choices) > 0 else None),
            interactive=True
    )

def get_edge_weight(edge_weight_input):
    if edge_weight_input == "User defined value":
        return gr.update(visible=True)
    else:
        return gr.update(visible=False)

def capture_user_value(user_value):
    return user_value

def train_size_slider_change(train_size_slider, val_size_slider):
    if (train_size_slider + val_size_slider) > 1.0:
        return gr.update(value=1.0-train_size_slider)
    else:
        return gr.update(value=val_size_slider)

def val_size_slider_change(train_size_slider, val_size_slider):
    if (train_size_slider + val_size_slider) > 1.0:
        return gr.update(value=1.0-val_size_slider)
    else:
        return gr.update(value=train_size_slider)

def create_graph_button_click(
    edge_type,
    bidirectional_checkbox,
    feature_selector,
    class_label_dropdown,
    edge_weight_input,
    user_defined_input,
    train_size_slider,
    val_size_slider):
    
    global config, graph_output_path, create_graph_button_clicked_flag
    create_graph_button_clicked_flag = True
    config.input_file_or_dirpath = create_graph_input_path
    config.selected_features = feature_selector + ['ann', class_label_dropdown]
    config.class_column = class_label_dropdown
    config.train_size = train_size_slider
    config.val_size = val_size_slider
    config.add_bidirectional_edges = bidirectional_checkbox
    config.edge_type = edge_type
    #["Default of 1", "Number of incoming edges", "User defined value"]
    if edge_weight_input == "User defined value":
        config.edge_weight_type = EDGE_WEIGHT_TYPE_UNIFORM
        config.edge_weight_value = int(user_defined_input)
    elif edge_weight_input == "Number of incoming edges":
        config.edge_weight_type == EDGE_WEIGHT_TYPE_INDEGREE
    else:
        config.edge_weight_type = EDGE_WEIGHT_TYPE_UNIFORM
        config.edge_weight_value = 1

    progress = gr.Progress()
    progress(0, desc="Starting graph creation")
    import time
    start_time_cg = time.time()
    graph_output_path, _ = create_graph(config, progress)
    print("Time taken to create graph: ", time.time()-start_time_cg)
    loaded_config_json = json.load(open(graph_output_path.replace(".bin", "_config.json")))
    loaded_config = GraphCreationConfig(**loaded_config_json)
    trainer_config.input_graph_path = graph_output_path
    if os.path.exists(trainer_config.training_statistics_filename):
        os.remove(trainer_config.training_statistics_filename)
        
    create_graph_button_clicked_flag = False
    
    return "Completed!", gr.Tabs(selected=3), gr.update(interactive=True), gr.update(value=graph_output_path), gr.update(
        value=loaded_config.to_dataframe(),
        label=f"Selected graph configuration ({load_graph_selector})"), gr.update(), gr.update(), gr.update()

def load_graph_handler(load_graph_selector):
    if len(load_graph_selector) == 0:
        return gr.update()
    loaded_config_json = json.load(open(os.path.join(
            config.output_dirpath, 
            load_graph_selector.replace(".bin", "_config.json"))))
    loaded_config = GraphCreationConfig(**loaded_config_json)
    return gr.update(value=os.path.join(
        config.output_dirpath, 
        load_graph_selector)), gr.update(
        value=loaded_config.to_dataframe(),
        visible=True)

def show_train_tab(load_graph_selector):
    global trainer_config
    loaded_config_json = json.load(open(os.path.join(
            config.output_dirpath, 
            load_graph_selector.replace(".bin", "_config.json"))))
    loaded_config = GraphCreationConfig(**loaded_config_json)
    trainer_config.input_graph_path = os.path.join(
        config.output_dirpath,
        load_graph_selector)
    return gr.Tabs(selected=3), gr.update(interactive=True), gr.update(value=os.path.join(
        config.output_dirpath, 
        load_graph_selector)), gr.update(
        value=loaded_config.to_dataframe(),
        label=f"Selected graph configuration ({load_graph_selector})")

def model_settings_change(number_of_layers, dropout_rate):
    global trainer_config
    trainer_config.number_of_layers = number_of_layers
    trainer_config.dropout_rate = dropout_rate
    

def update_trainer_config(model_selection, epochs, hidden_layers, learning_rate):
    global trainer_config
    trainer_config.model_selection = str(model_selection)
    trainer_config.number_of_epochs = int(epochs)
    trainer_config.number_of_hidden_layers = int(hidden_layers)
    trainer_config.learning_rate = float(learning_rate)

def start_training():
    import time
    start_time_tr = time.time()
    filepath = trainer_config.training_statistics_filename
    if os.path.exists(filepath):
        json.dump({
            'epoch': [],
            'train_loss': [],
            'validation_loss': [],
            'validation_accuracy': []}, open(filepath, 'w'))
    tr.run(trainer_config)
    print("Time taken to train model: ", time.time()-start_time_tr)
    confusion_matrix_path, classification_report_df = draw_confusion_matrix(trainer_config)
    return gr.update(visible=True), gr.update(value=classification_report_df), gr.update(value=confusion_matrix_path, visible=True), gr.update(visible=True)

def show_model_inference():
    return gr.Tabs(selected=4), gr.update(visible=True)


# Function to load images
def load_images():
    image_paths = glob.glob("/mydata/GAF/VariantKG/codebase/subgraph_*.png")
    images = [Image.open(path) for path in image_paths]
    return images

def show_model_interpretation():
    return gr.Tabs(selected=5)

def update_memory_used_plot():
    global seconds, memory_used
    seconds.append(len(seconds))
    memory_used.append(psutil.Process().memory_info().rss/1024**2)
    return gr.update(value=pd.DataFrame({
        'Period': seconds[:-10], 
        'Memory Used': memory_used[:-10]}))
    
def update_graph_plots():
    filepath = trainer_config.training_statistics_filename
    if not os.path.exists(filepath):
        json.dump({
            'epoch': [],
            'train_loss': [],
            'validation_loss': [],
            'validation_accuracy': []}, open(filepath, 'w'))
        return gr.update(), gr.update(), gr.update()

    stats = json.load(open(filepath))
    return gr.update(value=pd.DataFrame({
        'Epoch': stats['epoch'], 
        'Training Loss': stats['train_loss']})), gr.update(value=pd.DataFrame({
        'Epoch': stats['epoch'], 
        'Validation Loss': stats['validation_loss']})), gr.update(value=pd.DataFrame({
        'Epoch': stats['epoch'], 
        'Validation Accuracy': stats['validation_accuracy']}))

def update_file_dropdowns():
    return gr.update(
        choices=os.listdir(base_file_dir)), gr.update(
            choices=[file for file in os.listdir(config.output_dirpath) if file.endswith(".bin")] if os.path.exists(config.output_dirpath) else [])
        
with gr.Blocks(js=js_func) as demo:
    gr.Markdown(
        """
        <div style="text-align: center; padding: 20px; background-color: #f0f0f0; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);">
            <h1 style="color: #333; font-family: 'Helvetica Neue', sans-serif;">VariantKG</h1>
            <p style="font-size: 18px; color: #666;">A Scalable Tool for Analyzing Genomic Variants of Humans Using Knowledge Graphs and Machine Learning</p>
        </div>
        """
    )
    with gr.Row() as options:
        our_data_button_2 = gr.Button("Use pre-built KG")
        custom_data_button_1 = gr.Button("Upload a new VCF")
        load_graph_button_3 = gr.Button("Training history")
    
    # gr.Markdown("Prepare the graph to be used for training")
    with gr.Tabs() as tabs:
        with gr.TabItem("Load File(s)", id=0, visible=False) as tab1:
            with gr.Row():
                upload_button = gr.UploadButton(
                    label="Upload VCF file(s)",
                    file_count='multiple',
                    variant="primary")
            
            progress_status_custom_data = gr.Textbox(
                label="Progress status",
                interactive=False,
                visible=True
            )
            
        with gr.TabItem("Prepare data for ML", id=1, visible=False) as tab2:
            with gr.Group() as feature_creation_group:
                with gr.Row():
                    selection_criteria_selector = gr.Radio(
                        label="Select patients based on",
                        choices=["Age group", "Patient (Accession ID)"],
                        visible=True,
                        value="Age group"
                    )
                with gr.Row():
                    accession_id_selector = gr.CheckboxGroup(
                        label="Patient (accession id) picker",
                        info="Select accession id for fetching data for graph creation",
                        visible=False
                    )
                with gr.Row():
                    age_filter_selector = gr.CheckboxGroup(
                        label="Age group picker",
                        info="Select patients grouped by age",
                        choices=AGE_GROUP_FILTER_MAPPING.keys(),
                        visible=True,
                        interactive=True
                    )
                with gr.Row():
                    feature_selector_custom = gr.CheckboxGroup(
                        label="Feature selector",
                        info="Select features to extract from the knowledge graph",
                        visible=False
                    )
            
            fetch_button = gr.Button("Fetch data from KG", variant="primary")
            
            with gr.Group(visible=False) as graph_creation_group:
                with gr.Row():
                    feature_selector = gr.CheckboxGroup(
                        choices=[],
                        label="Feature selector",
                        info="Select features to extract for graph creation",
                        value=[])

                with gr.Row():
                    class_label_dropdown = gr.Dropdown(
                        label="Class label selector",
                        info="Select the class label to use",
                        interactive=False)
                    
                with gr.Row():
                    edge_type = gr.Dropdown(
                        choices=[EDGE_TYPE_GENE, EDGE_TYPE_FULLY_CONNECTED],
                        multiselect=False,
                        label="Edge type",
                        info="Select edge type",
                        value=EDGE_TYPE_GENE,
                        interactive=True,
                    )
                    edge_choices = ["Default of 1", "Number of incoming edges", "User defined value"]
                    edge_weight_input = gr.Dropdown(
                        choices=edge_choices,
                        multiselect=False,
                        label="Edge weight",
                        info="Select edge weight",
                        value=edge_choices[0],
                        interactive=True
                    )
                with gr.Row():
                    # self_loop_checkbox = gr.Checkbox(label="Self loop", info="Add self loops to all nodes in the graph?")
                    bidirectional_checkbox = gr.Checkbox(label="Bidirectional edges", info="Make all edges in the graph bidirectional?", value=True)
                with gr.Row():
                    user_defined_input = gr.Textbox(
                        label="Enter your value",
                        interactive=True,
                        visible=False
                    )
                    
                with gr.Row():
                    train_size_slider = gr.Slider(
                        minimum=0.1,
                        maximum=1.0,
                        value=0.8,
                        step=0.1,
                        interactive=True,
                        label="Training dataset split ratio",
                    )
                    val_size_slider = gr.Slider(
                        minimum=0.1,
                        maximum=1.0,
                        value=0.1,
                        step=0.1,
                        interactive=True,
                        label="Validation dataset split ratio",
                    )
            submit_button = gr.Button("Create DGL Graph", variant="primary", visible=False)
            
            progress_status = gr.Textbox(
                label="Progress status",
                interactive=False,
                visible=True
            )
            blazegraph_query_display = gr.HTML(label="Blazegraph Query", visible=False)

        with gr.TabItem("Load Saved Graph", id=2, visible=False) as tab3:
            with gr.Row():
                load_graph_selector = gr.Dropdown(
                    choices=[file for file in os.listdir(config.output_dirpath) if file.endswith(".bin")] if os.path.exists(config.output_dirpath) else [],
                    label="Input graph picker",
                    info="Select graph for input data")
                download_graph_load_graph = gr.File(label="Download the selected graph")
            with gr.Row():
                config_display = gr.Dataframe(
                    label="The configuration used to create the graph",
                    wrap=True,
                    visible=False)
            load_button = gr.Button("Set model parameters", variant="primary")
        
        with gr.TabItem("Train Model", interactive=False, id=3, visible=False) as tab4:
            with gr.Row():
                download_graph_train_model = gr.File(label="Download the selected graph")
            with gr.Row():
                config_display_tab4 = gr.Dataframe(
                    label="Selected graph configuration",
                    wrap=True)
            with gr.Row():
                model_selection = gr.Dropdown(
                    label="Select model",
                    choices=['GraphSAGE', 'Graph Convolutional Network','GraphTransformer']
                )
            with gr.Row():
                number_of_layers = gr.Slider(
                    minimum=2,
                    maximum=64,
                    value=2,
                    step=1,
                    interactive=True,
                    label="Number of model layers",
                )
                dropout_rate = gr.Slider(
                    minimum=0.0,
                    maximum=1.0,
                    value=0.0,
                    step=0.1,
                    interactive=True,
                    label="Dropout rate",
                    info="If no dropout, set to 0.0"
                )
            with gr.Row():
                epochs_input = gr.Textbox(
                    label="Number of epochs",
                    value=500,
                )
                hidden_layers_input = gr.Textbox(
                    label="Number of hidden layers",
                    value=16,
                )
                learning_rate_input = gr.Textbox(
                    label="Learning rate",
                    value=0.01,
                )
            with gr.Row():
                train_loss_plot = gr.LinePlot(
                    x="Epoch",
                    y="Training Loss",
                    title="Training loss plot (updates every second)",
                    width=600,
                    height=350,
                )
                val_loss_plot = gr.LinePlot(
                    x="Epoch",
                    y="Validation Loss",
                    title="Validation loss plot (updates every second)",
                    width=600,
                    height=350,
                )
            with gr.Row():
                validation_accuracy_plot = gr.LinePlot(
                    x="Epoch",
                    y="Validation Accuracy",
                    title="Validation accuracy plot (updates every second)",
                    width=600,
                    height=350,
                )
                memory_usage_plot = gr.LinePlot(
                    x="Period",
                    y="Memory Used",
                    title="CPU memory usage plot in MBs (updates every second)",
                    width=600,
                    height=350,
                )
                
            train_button = gr.Button("Start training", variant="primary")
            model_inference_button = gr.Button("Evaluate test set", variant="primary", visible=False)
        
        with gr.TabItem("Model Inference", id=4,visible=False) as tab5:
            with gr.Row():
                metrics_df = gr.Dataframe(label="Test set metric scores")
            with gr.Row():
                confusion_matrix = gr.Image(
                    label="image", 
                    visible=False, 
                    width=600, 
                    height=500,
                )
            model_interpretation_button = gr.Button("Model Interpretation", variant="primary")
            
        with gr.TabItem("Model Interpretation", id=5, visible=False) as tab6:
            hops_slider = gr.Slider(minimum=1, maximum=25, step=1, label="Number of Degree/Hops per Node")
            subgraphs_slider = gr.Slider(minimum=1, maximum=10, step=1, label="Number of Subgraphs to Display")
            node_id_input = gr.Textbox(label="Enter Node ID")
            display_button = gr.Button("Display Subgraphs")
            image_gallery = gr.Gallery(label="Subgraph Images", elem_id="subgraph_gallery")

            # subgraph_output = gr.Plot(label="Subgraph Visualization")

            def display_subgraphs(hops, subgraphs, node_id_input):
                main(node_id_input, hops, subgraphs)
                print("Displaying subgraphs!")
                images = load_images()
                return images
            display_button.click(display_subgraphs, inputs=[hops_slider, subgraphs_slider, node_id_input], outputs=image_gallery)
            
            # # Add image gallery and update button within the same tab
            # image_gallery = gr.Gallery(label="Subgraph Images", elem_id="subgraph_gallery")

            # def update_gallery():
            #     images = load_images()
            #     return images

            # update_button = gr.Button("Load Images")
            # update_button.click(update_gallery, inputs=[], outputs=image_gallery)

    #NOTE ----- select accessionID by default
    upload_button.upload(
        fn=upload_file,
        inputs=upload_button,
        outputs=[accession_id_selector, age_filter_selector, feature_selector_custom, progress_status_custom_data, tabs]
    )
    
    selection_criteria_selector.change(
        fn=selection_criteria_toggle_selector,
        inputs=selection_criteria_selector,
        outputs=[age_filter_selector, accession_id_selector]
    )
    age_filter_selector.change(
        fn=age_filter_selector_change_handler,
        inputs=age_filter_selector,
    )
    accession_id_selector.change(
        fn=accession_id_selector_change_handler,
        inputs=accession_id_selector,
    )
    feature_selector_custom.change(
        fn=feature_selector_custom_change,
        inputs=feature_selector_custom,
    )
    fetch_button.click(
        fn=feature_generator_click,
        outputs=[feature_selector, graph_creation_group, submit_button, feature_creation_group, fetch_button, progress_status]
    )
    # self_loop_checkbox.change(update_self_loop, inputs=self_loop_checkbox)
    edge_type.change(select_edge_type, inputs=edge_type)
    bidirectional_checkbox.change(update_bidirectional, inputs=bidirectional_checkbox)
    train_size_slider.change(
        train_size_slider_change, 
        [train_size_slider, val_size_slider], 
        [val_size_slider]
    )
    val_size_slider.change(
        val_size_slider_change, 
        [train_size_slider, val_size_slider], 
        [train_size_slider]
    )
    feature_selector.change(
        fn=feature_selector_change,
        inputs=feature_selector,
        outputs=class_label_dropdown
    )
    edge_weight_input.change(
        fn=get_edge_weight,
        inputs=edge_weight_input,
        outputs=user_defined_input
    )
    user_defined_input.change(
        fn=capture_user_value, 
        inputs=user_defined_input, 
        outputs=None)
    
    submit_button.click(
        fn=create_graph_button_click,
        inputs=[
            edge_type,
            bidirectional_checkbox,
            feature_selector,
            class_label_dropdown,
            edge_weight_input,
            user_defined_input,
            train_size_slider,
            val_size_slider],
        outputs=[progress_status, tabs, tab4, download_graph_train_model, config_display_tab4, train_loss_plot, val_loss_plot, validation_accuracy_plot])
    
    load_graph_selector.change(
        fn=load_graph_handler, 
        inputs=[load_graph_selector], 
        outputs=[download_graph_load_graph, config_display])
    load_button.click(
        show_train_tab,
        inputs=[load_graph_selector],
        outputs=[tabs, tab4, download_graph_train_model, config_display_tab4]
    )
    
    model_selection.change(update_trainer_config, [model_selection, epochs_input, hidden_layers_input, learning_rate_input])
    number_of_layers.change(
        model_settings_change, 
        [number_of_layers, dropout_rate], 
    )
    dropout_rate.change(
        model_settings_change, 
        [number_of_layers, dropout_rate], 
    )
    epochs_input.change(update_trainer_config, [model_selection, epochs_input, hidden_layers_input, learning_rate_input])
    hidden_layers_input.change(update_trainer_config, [model_selection, epochs_input, hidden_layers_input, learning_rate_input])
    learning_rate_input.change(update_trainer_config, [model_selection, epochs_input, hidden_layers_input, learning_rate_input])
    
    train_button.click(start_training, outputs=[tab5, metrics_df, confusion_matrix, model_inference_button])
    model_inference_button.click(show_model_inference, outputs=[tabs, tab6])
    model_interpretation_button.click(show_model_interpretation, outputs=[tabs])
    demo.load(update_graph_plots, None, [train_loss_plot, val_loss_plot, validation_accuracy_plot], every=1)
    demo.load(update_memory_used_plot, None, [memory_usage_plot], every=1)
    demo.load(update_button_status, None, [upload_button, submit_button], every=1)
    demo.load(show_blazegraph_query, None, [blazegraph_query_display], every=1)

    custom_data_button_1.click(fn=custom_data_button_1_click, outputs=[tabs, tab1, tab2, tab3, tab4, tab5])
    our_data_button_2.click(fn=our_data_button_2_click, outputs=[tabs, tab1, tab2, tab3, tab4, tab5, feature_creation_group, fetch_button, accession_id_selector, age_filter_selector, feature_selector_custom, graph_creation_group, submit_button, progress_status])
    load_graph_button_3.click(fn=load_graph_button_3_click, outputs=[tabs, tab1, tab2, tab3, tab4, tab5])
    
if __name__ == "__main__":
    demo.launch(debug=True, server_port=7865)
    # Append /?__theme=light
