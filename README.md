
# Used Cars Price Prediction

Welcome to the repository for the "Used Cars Price Prediction" project. This project was conducted to predict the price of used cars based on various features and seller information.

## Autor

Sandro Gassner

## Date

26 March 2022

## Table of Contents

- [Introduction](#introduction)
- [Data Sources](#data-sources)
- [Methods and Results](#methods-and-results)
- [Data Collection and Storage](#data-collection-and-storage)
- [Data Cleaning and Preparation](#data-cleaning-and-preparation)
- [Data Analysis and Visualization](#data-analysis-and-visualization)
- [Results](#results)
- [Shiny Application](#shiny-application)
- [Scaling and Cloud Deployment](#scaling-and-cloud-deployment)

## Introduction

This project addresses the following two major questions:
1. What features of a vehicle should be considered to predict the price of a used car?
2. What is the price of a used car given its technical characteristics and seller information?

## Data Sources

We used the following data sources for this project:
- [US Used Cars Dataset from Kaggle](https://www.kaggle.com/datasets/ananaymital/us-used-cars-dataset)
- [US Zip Codes Dataset](https://www.unitedstateszipcodes.org/zip-code-database/)

## Methods and Results

We tested various regression methods including OLS and LASSO to predict the price of used cars. Our findings are summarized as follows:
- The LASSO model using the BigLasso package with variable transformations (log-transformation of the price-variable and adding a squared age-variable) achieved an \(R^2\) of 0.924, but required 16 hours of computation time.
- OLS, with similar transformations, achieved an \(R^2\) of 0.888 within a few minutes, making it a more reasonable choice considering the trade-off between computation time and prediction power.

## Data Collection and Storage

We downloaded the "US used cars dataset" from Kaggle, which contains three million observations and 66 columns. Initial data cleaning involved dropping irrelevant columns to improve loading speed and memory usage. Data was stored locally as CSV files, with multiple versions saved at different steps of the cleaning process.

## Data Cleaning and Preparation

Key steps in data cleaning included:
1. Dropping irrelevant or redundant variables.
2. Converting empty fields and unit values to a consistent format.
3. Transforming zip-codes into corresponding states.
4. Creating dummy variables for categorical data.
5. Implementing the MICE algorithm for imputing missing values.

## Data Analysis and Visualization

We initially used OLS for price prediction but faced computational issues due to high dimensionality. The LASSO model with a 5-fold cross-validation was more suitable for handling the Big-P problem. Variable transformations significantly improved the prediction performance of both models.

## Results

### OLS Out-of-Sample Performance

| Model                          | Without Transformations | With Transformations |
|--------------------------------|-------------------------|----------------------|
| Car models excluded            | \(R^2 = 0.736\)         | \(R^2 = 0.888\)      |
| Car models included            | No reliable results     | No reliable results  |

### LASSO Out-of-Sample Performance

| Model                          | Without Transformations | With Transformations |
|--------------------------------|-------------------------|----------------------|
| Car models included            | \(R^2 = 0.825\)         | \(R^2 = 0.924\)      |
| Number of covariates           | 1,424                   | 1,400                |

## Shiny Application

We developed a Shiny application to illustrate the prediction power of the final LASSO model. The user can estimate the price of a car by entering its characteristics into an input mask. The app can be accessed [here](https://sgassner.shinyapps.io/car_price_prediction_app/).

## Scaling and Cloud Deployment

To address RAM issues and improve computation time, I recommend using cloud solutions like AWS EC2 for R/RStudio. Detailed steps for setting up and running computations in AWS RStudio are provided in the project documentation.

For further details on the code, data processing, and model implementation, please refer to the report and scripts in this repository.
