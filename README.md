# Diabetes Health Indicators Dataset Analysis

This repository contains an analysis of the Diabetes Health Indicators dataset, focusing on the imbalanced dataset (`df3`) and a balanced dataset (`df2`). The analysis aims to understand the relationship between features and the target variable, using this information to predict patients in the balanced dataset. Four machine learning models—Logistic Regression, Naive Bayes, Decision Tree, and Random Forest—are implemented for prediction.

## Datasets

There are three datasets in this analysis:

1. `df`: Dataset with data from patients without diabetes, pre-diabetes, and diabetes.
2. `df2`: Balanced dataset with equal representation of patients with and without diabetes.
3. `df3`: Imbalanced dataset of patients with and without diabetes.

## Prerequisites

Make sure you have the required R packages installed:

```R
install.packages(c("ggthemes", "ggsignif", "skimr", "caTools", "rpart.plot", "patchwork", "gridExtra", 
                    "rattle", "Boruta", "ROSE", "naivebayes", "glmnet", "tidymodels"))
