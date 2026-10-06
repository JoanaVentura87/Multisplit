
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Multisplit

<!-- badges: start -->

<!-- badges: end -->

The goal of **Multisplit** is to provide a method for splitting datasets
with multiple binary outcomes into training and test sets while
preserving, as closely as possible, the distribution of positive cases
across outcomes.

## Installation

You can install the development version of Multisplit from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("JoanaVentura87/Multisplit")
```

## Overview

The `Multisplit` package provides a function for splitting datasets with
multiple binary outcomes into training and test sets.

## Main function

### `split_multioutcome()`

The function performs iterative multilabel stratification for datasets
with multiple binary outcomes. It aims to preserve, as closely as
possible, the distribution of the different outcomes when creating
training and test sets.

## Method

The `split_multioutcome()` function uses an iterative multilabel
stratification strategy based on the general principle proposed by
Sechidis et al. (2011).

The method is designed for datasets with multiple binary outcomes that
can occur simultaneously in the same observation. Its main objective is
to distribute observations across folds while preserving, as closely as
possible, the distribution of positive cases for each outcome.

The resulting folds are then combined to create the training and test
sets. By default, 7 of 10 folds are used for training and 3 folds for
testing, resulting in an approximately 70/30 split.

## Example

The following example illustrates how to use `split_multioutcome()` with
a synthetic dataset containing multiple binary outcomes.

``` r
library(Multisplit)
# Create a synthetic dataset
set.seed(123)

n <- 800

synthetic_data <- data.frame(
  ID = 1:n,
  age = sample(18:95, n, replace = TRUE),
  sex = sample(c("Female", "Male"), n, replace = TRUE),
  BMI = round(rnorm(n, mean = 27, sd = 6), 1),
  smoking = sample(
    c("Never", "Former", "Current"),
    n,
    replace = TRUE,
    prob = c(0.50, 0.30, 0.20)
  ),
  physical_activity = sample(
    c("none/Low", "Moderate", "High"),
    n,
    replace = TRUE,
    prob = c(0.50, 0.40, 0.10)
  ),
  systolic_BP = round(rnorm(n, mean = 135, sd = 25)),
  diastolic_BP = round(rnorm(n, mean = 85, sd = 15)),
  cholesterol = round(rnorm(n, mean = 200, sd = 50)),
  stroke = rbinom(n, 1, 0.15),
  heart_attack = rbinom(n, 1, 0.20),
  dyslipidemia = rbinom(n, 1, 0.45),
  diabetes = rbinom(n, 1, 0.30)
)

# Define the binary outcomes
outcomes <- c(
  "stroke",
  "heart_attack",
  "dyslipidemia",
  "diabetes"
)

# Perform the split
split <- split_multioutcome(
  data = synthetic_data,
  outcomes = outcomes,
  k = 10,
  train_folds = 7,
  seed = 1234
)

# Training and test sets
train_set <- split$train
test_set <- split$test

# Balance information
split$balanced_table
#> # A tibble: 4 × 7
#>   Outcome      Global_N Train_N Train_Pct Test_N Test_Pct Check
#>   <chr>           <int>   <int>     <dbl>  <int>    <dbl> <lgl>
#> 1 stroke            107      75      70.1     32     29.9 TRUE 
#> 2 heart_attack      172     121      70.4     51     29.6 TRUE 
#> 3 dyslipidemia      367     257      70.0    110     30.0 TRUE 
#> 4 diabetes          242     170      70.2     72     29.8 TRUE
```

## Output

The function returns a list containing:

- `train`: the training dataset.
- `test`: the test dataset.
- `balanced_table`: a table summarising the number and percentage of
  positive cases for each outcome in the global, training, and test
  datasets.
