# Data Science Project - Estimating Pure Premiums in Insurance

## 🎯 Objective
This project aims to estimate the pure premium for an automobile insurance portfolio based on historical data. It involves two key steps:
- Predicting the probability of a claim using a classification model (XGBoost).
- Estimating the average claim cost using Ridge regression.

## 🛠️ Environment and Required Libraries
The project is developed in **R**. The following libraries are required:
- tidyverse, caret, MASS, glmnet, broom, klar, e1071, rpart, rpart.plot, car, ggplot2, plotly, zoo, data.table, lubridate, dplyr, corrplot, questionr, parallel, randomForest, tree, xgboost, pROC, ROCR, stats

## 📂 Project Structure
- `VF Data science.R` : Main script that runs the entire workflow.
- `Annexe.R` : Contains all utility functions (data preparation, modeling, and exporting results).
- `dataTrain_12.csv` : Training dataset.
- `test.csv` : Test dataset.
- `first_results.csv` : Results from an initial log-normal regression to enrich the test dataset.
- `premium_12.csv` : Final output file containing ID, claim probability, and estimated average cost.

## ⚙️ Workflow Overview
1. **Data Loading**: Import training and testing datasets.
2. **Data Preparation**:
    - For classification: Create a binary target variable ("sinistre") indicating the presence or absence of a claim.
    - For regression: Clean the dataset and select relevant features.
3. **Modeling**:
    - **Classification**: Train an XGBoost model to predict the probability of a claim.
    - **Regression**: Train a Ridge regression model to estimate the average cost of a claim.
4. **Exporting Results**:
    - Combine the predicted probabilities and average costs into a single output file (`premium_12.csv`).

## 🚀 How to Run
1. Open the project in RStudio.
2. Install all required libraries if they are not already installed.
3. Open and run the script `VF Data science.R`.
4. The output file `premium_12.csv` will be generated automatically.

## 📈 Outputs
The final CSV file `premium_12.csv` includes:
- `id`: Unique identifier for each record.
- `probability`: Predicted probability of a claim.
- `averageCost`: Estimated average cost in the event of a claim.

## 👤 Author
This project was completed as part of the Master 2 Actuarial Science program, Semester 1.
