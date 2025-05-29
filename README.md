# Student Dropout Prediction in Higher Education

This project is based on my Bachelor's thesis from **Taras Shevchenko National University of Kyiv**.  
The goal is to identify students who are likely to drop out by solving a binary classification task using data analysis methods.

## Contents

- `final code_bachelor.R`: Main R script
- `data.csv`: Dataset used (publicly available)
- `README.md`: Project overview and methodology

## Dataset

The dataset contains student records from the **Polytechnic Institute of Portalegre (Portugal)**.  
It includes academic performance indicators and socio-demographic attributes. The data is publicly available and anonymized.

## Project Description

- **Problem**: Binary classification — _dropout_ vs. _no-dropout_
- **Language & Tools**: R, RStudio, libraries including `caret`, `e1071`, `randomForest`, `pROC`, `ggplot2`, `tidyverse`, `DMwR`, `rpart`, `rpart.plot`, `kernlab`, `mlblench`, `MLmetrics`, `iml`
- **Preprocessing**:
  - Univariate analysis
  - Testing for normality
  - Correlation analysis & multicollinearity reduction
  - One-hot encoding of categorical variables
  - Dataset splitting into training, test, and validation sets
  - Training dataset balancing using SMOTE

## 🧠 Models Applied

1. Logistic Regression  
2. Naive Bayes  
3. Decision Tree  
4. Random Forest  
5. Support Vector Machine (SVM)

## 🔍 Evaluation Metrics

- Confusion Matrix
- Accuracy
- Precision
- Recall
- F1-score
- AUC-ROC

## ⚙️ Optimization & Feature Analysis

- Hyperparameter tuning was applied to **Random Forest** and **SVM** to improve performance.
- Feature importance was evaluated using **permutation importance** to identify the most influential variables.

## Results

- Comparative evaluation showed Random Forest and SVM achieved the best predictive results.
- The final models offer practical value in identifying students at risk of dropping out in early stages.

## License & Data Usage

The dataset used in this project was sourced from a public repository and does not contain personally identifiable information.  
This project is shared for **educational and demonstrational purposes**.
