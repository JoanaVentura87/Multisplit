# Create synthetic data
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

outcomes <- c(
  "stroke",
  "heart_attack",
  "dyslipidemia",
  "diabetes"
)

library(testthat)

test_that("split_multioutcome returns valid train, test, and balanced table", {

  result <- split_multioutcome(
    data = synthetic_data,
    outcomes = outcomes,
    k = 10,
    train_folds = 7,
    seed = 1234
  )

  # Check output structure

  expect_type(result, "list")
  expect_named(
    result,
    c("train", "test", "balanced_table")
  )

  # Check total number of observations

  expect_equal(
    nrow(result$train) + nrow(result$test),
    nrow(synthetic_data)
  )

  # Check train/test split

  expect_equal(nrow(result$train), 560)
  expect_equal(nrow(result$test), 240)

  # Check that all variables are retained

  expect_equal(names(result$train), names(synthetic_data))
  expect_equal(names(result$test), names(synthetic_data))

  # Check balanced table

  expect_equal(
    result$balanced_table$Outcome,
    outcomes
  )

  expect_equal(
    result$balanced_table$Global_N,
    sapply(synthetic_data[outcomes], function(x) sum(x == 1))
  )

  expect_equal(
    result$balanced_table$Train_N +
      result$balanced_table$Test_N,
    result$balanced_table$Global_N
  )

  # Check that the balance verification is TRUE

  expect_true(
    all(result$balanced_table$Check)
  )
})

