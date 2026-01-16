# Predicting-Ecommerce-Customer-Reviews

## Overview
This project’s goal is to predict a customers review rating based on other factors of their internet purchasing history.

## Data
The data comes from a kaggle data set on e-commerce customer behavior and sales analysis.

## Methodology
### Step 1: Exploratory Data Analysis
- Using libraries such as tidyverse to sort the data and visualize different trends and correlations between the variables.
- Identified which predictor variables needed transformations to be effective in prediction.
### Step 2: Data Cleaning
- Cleaned and transformed the data using tidyverse
- Performed feature engineering to get variables into new categories for prediction
### Step 3: Model Selection
- Implemented double cross-validation to get a honest measure of model performance.
- Compared linear regression to random forest models to see if linear or tree models performed better based on Mean Absolute Error
### Step 4: Model Interpretation
- Showcased variable importance and partial dependence plots to show predictor weight and importance to the model.
## Results
- The results showed that tree-based models outperformed linear models for predicting customer reviews.
- A final double cross validation MAE of .882 was found for the best random forest model.
- Final Report: https://github.com/mitchellaldridge/Predicting-Ecommerce-Customer-Reviews/blob/main/Final%20Summary.pdf
