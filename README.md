# Prediction of Article Popularity Project

## Table of Contents
- [Prediction of Article Popularity Project](#prediction-of-article-popularity-project)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Project Objectives](#project-objectives)
  - [Data Description](#data-description)
    - [Summary Statistics](#summary-statistics)
  - [Methodology](#methodology)
  - [Results and Models](#results-and-models)
    - [Model Comparison](#model-comparison)
  - [Conclusion](#conclusion)
  - [How to Run the Project](#how-to-run-the-project)

## Introduction
This project focuses on predicting the popularity of articles published on the Mashable website. The popularity of an article is a key indicator for web editors, bloggers, and publishers to evaluate the reach and impact of their content. The project explores various machine learning models to determine which method can best predict an article's popularity, measured by the number of shares.

## Project Objectives
- To explore different machine learning models for predicting article popularity.
- To compare linear regression, Ridge, Lasso, Principal Component Analysis (PCA), General Additive Model (GAM), and Random Forest to find the most effective model.
- To evaluate model performance based on Root Mean Square Error (RMSE).

## Data Description
The dataset comes from the OpenML website and contains 39,016 articles published on Mashable, with 61 variables describing various characteristics of the articles. Key variables include:
- Number of words in the title.
- Number of words in the content.
- Number of images and videos.
- Number of shares (target variable).

### Summary Statistics
- **Number of shares**: Mean = 3,406, St.Dev = 11,706, Max = 843,300, Min = 1
- **Number of words in the title**: Mean = 10.38, Max = 20, Min = 2
- **Number of words in content**: Mean = 546.99, Max = 8,474, Min = 0

## Methodology
Several models were applied to predict article popularity:
1. **Linear Regression**: A basic model using forward, backward, and autometrics stepwise selection methods.
2. **Ridge Regression**: A regularization method that introduces a penalty term to prevent overfitting.
3. **Lasso Regression**: Similar to Ridge but also performs variable selection by shrinking some coefficients to zero.
4. **Principal Component Analysis (PCA)**: A dimensionality reduction technique used to create components for a regression model.
5. **General Additive Model (GAM)**: A non-parametric model that captures non-linear relationships between variables.
6. **Random Forest**: A machine learning ensemble model based on decision trees, used for robust predictions.

## Results and Models
- **Linear Regression**: Achieved an RMSE of 0.884 with 41 selected variables.
- **Ridge Regression**: RMSE of 0.939 using a lambda hyper-parameter selected through cross-validation.
- **Lasso Regression**: RMSE of 0.909, reducing the number of variables to 15.
- **PCA**: RMSE of 0.889 with 24 components explaining 94% of variance.
- **GAM**: Achieved the best performance with an RMSE of 0.864.
- **Random Forest**: RMSE of 0.863, comparable to the GAM model.

### Model Comparison
The Random Forest and GAM models performed equally well, both achieving the lowest RMSE scores, making them the best candidates for predicting article popularity. The PCA model also performed surprisingly well, closely matching the performance of the OLS model.

## Conclusion
The study shows that machine learning models, particularly Random Forest and GAM, are effective in predicting article popularity. While all models had their strengths, Random Forest and GAM provided the best predictive power in this case, with minimal differences in performance. 

## How to Run the Project
1. Clone this repository:
   ```bash
   git clone https://github.com/yourusername/article-popularity-prediction.git
