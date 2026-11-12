# ST558 Final Project

This repository contains my final project for ST 558: Data Science for Statisticians.

The project uses the Melbourne Housing dataset to build and compare predictive models for housing price.

## Modeling

The response variable is `Price`.

The predictors used are:

- Rooms
- Type
- Distance
- Bathroom
- Car
- Landsize
- Regionname

The data was split into 70% training data and 30% test data. Five-fold cross-validation was used on the training set to tune the models.

Two model families were compared:

- Regression Tree
- Random Forest

The random forest had the lower test RMSE and was selected as the final model.

Final random forest settings:

- `mtry = 6`
- `trees = 500`
- `min_n = 5`

## Modeling Report

The rendered modeling report is available here:

https://divyashahh.github.io/ST558-Final-Project/modeling.html

## Prediction API

The project contains a Plumber API with two endpoints.

### `/pred`

Returns a predicted Melbourne housing price based on:

- Rooms
- Type
- Distance
- Bathroom
- Car
- Landsize
- Regionname

Example:

```text
http://localhost:8000/pred?Rooms=3&Type=h&Distance=10&Bathroom=2&Car=2&Landsize=500&Regionname=Northern%20Metropolitan