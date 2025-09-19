import pandas as pd
import numpy as np
data = pd.read_csv("./results/Processed_data.csv")

#Model comparison
from sklearn.linear_model import LinearRegression, Lasso, Ridge
from sklearn.svm import SVR
from sklearn.ensemble import RandomForestRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.neural_network import MLPRegressor
from xgboost import XGBRegressor
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score


# Define the models
models = [
    ('Linear Regression', LinearRegression()),
    ('Lasso Regression', Lasso()),
    ('Ridge Regression', Ridge()),
    ('SVM', SVR()),
    ('Random Forest', RandomForestRegressor()),
    ('Decision Tree', DecisionTreeRegressor()),
    ('Neural Network', MLPRegressor(max_iter=50000)),
    ('XGBoost', XGBRegressor())
]

#Defining data here
#X=metadata[['spots', 'bases', 'spots_with_mates', 'avgLength', 'size_MB', 'InsertSize']]
X=data
X = X.loc[:,~X.columns.duplicated()]
X['Label'] = labelencoder.fit_transform(X['Label'])
y=X['Label']
X=X.drop('Label', axis=1)

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

#Scale the data
sc = StandardScaler()
X_train = sc.fit_transform(X_train)
X_test = sc.transform(X_test)

# Define a function to train a model and calculate metrics
def train_and_evaluate(model, X_train, y_train, X_test, y_test):
    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)
    mae = mean_absolute_error(y_test, y_pred)
    mse = mean_squared_error(y_test, y_pred)
    r2 = r2_score(y_test, y_pred)
    try:
        feature_importance = model.feature_importances_
        # print('Feature importance:', feature_importance)

    except AttributeError:
        feature_importance = None
    return mae, mse, r2, feature_importance

# Define a function to display the metrics in a table
def display_results(results):
    print(f"{'Model':<20}{'MAE':<10}{'MSE':<10}{'R2':<10}{'Feature Importance'}")
    for name, metrics in results.items():
        mae, mse, r2, feature_importance = metrics
        print(f"{name:<20}{mae:<10.2f}{mse:<10.2f}{r2:<10.2f}{feature_importance}")

# Train each model, calculate the metrics, and add the results to the table
results = {}
for name, model in models:
    results[name] = train_and_evaluate(model, X_train, y_train, X_test, y_test)

# Display the table
# display_results(results)

#sort the results based on R2 value
results_sorted = dict(sorted(results.items(), key=lambda item: item[1][2], reverse=True))
display_results(results_sorted)
results_sorted.to_csv('ML_results.csv')
