import matplotlib.pyplot as plt
from sklearn import metrics
import pickle
import pandas as pd

from input_dataclasses import GraphTrainerConfig
from local_utils import get_logger

def draw_confusion_matrix(config: GraphTrainerConfig):
    predictions = pickle.load(open(config.pickle_file, 'rb'))
    
    ground = predictions['gold']
    predicted = predictions['pred']
    
    classification_report = metrics.classification_report(
        y_true=ground,
        y_pred=predicted,
        output_dict=True)
    classification_report_list = []
    print(classification_report)
    for k, v in classification_report.items():
        if isinstance(v, dict):
            v['label'] = k
        else:
            v = {
                "label": k,
                "precision": v,
                "recall": "",
                "f1-score": "",
                "support": "",
            }
        classification_report_list.append(v)
    
    classification_report_df = pd.DataFrame(classification_report_list)
    classification_report_df = classification_report_df[["label", "precision", "recall", "f1-score", "support"]]


    confusion_matrix = metrics.confusion_matrix(ground, predicted)
    
    # width_pix = 600
    # height_pix = 500

    dpi = 100  
    
    num_of_classes_list = []
    cm_display = metrics.ConfusionMatrixDisplay(confusion_matrix = confusion_matrix, display_labels = num_of_classes_list.append(range(config.number_of_classes)))
    
    fig, ax = plt.subplots(figsize=(6, 4), dpi=100)
    cm_display.plot(ax=ax, cmap='Blues')
    plt.show()
    img_filename = config.pickle_file.replace('.pkl', '.png') 
    plt.savefig(img_filename)
    
    return img_filename, pd.DataFrame(classification_report_df)

if __name__ == "__main__":
    config = GraphTrainerConfig(pickle_file='/mydata/dgl/general/private-code-cloudlab/MODEL-10-16/best_preds.pkl', number_of_classes=4)
    draw_confusion_matrix(config)