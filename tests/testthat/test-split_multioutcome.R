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
test_that("split_multioutput returns valid train and test sets", {

  result <- split_multioutput(
    data = synthetic_data,
    outcomes = outcomes,
    k = 10,
    seed = 1234
  )

  expect_type(result, "list")
  expect_named(result, c("train", "test"))

  expect_equal(
    nrow(result$train) + nrow(result$test),
    nrow(synthetic_data)
  )

  expect_equal(nrow(result$train), 560)
  expect_equal(nrow(result$test), 240)

  expect_equal(names(result$train), names(synthetic_data))
  expect_equal(names(result$test), names(synthetic_data))
})
