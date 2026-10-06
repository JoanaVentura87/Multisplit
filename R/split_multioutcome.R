#' Split a dataset into training and test sets for multiple binary outcomes
#'
#' Splits a dataset into training and test sets while attempting to distribute
#' positive cases across multiple binary outcomes as evenly as possible.
#'
#' @param data A data frame containing the dataset to be split.
#' @param outcomes A character vector specifying the names of the binary
#'   outcome variables.
#' @param k An integer specifying the total number of folds used to construct
#'   the split. Default is 10.
#' @param train_folds An integer specifying the number of folds allocated to
#'   the training set. The remaining folds are allocated to the test set.
#'   Default is 7.
#' @param seed An optional integer used to make the random assignment
#'   reproducible. Default is NULL.
#'
#' @return A list containing three elements:
#' \describe{
#'   \item{train}{The training set, containing observations assigned to the
#'   first \code{train_folds} folds.}
#'   \item{test}{The test set, containing observations assigned to the
#'   remaining folds.}
#'   \item{balanced_table}{A summary table showing the number and percentage
#'   of positive cases for each outcome in the global dataset, training set,
#'   and test set. The \code{Check} column verifies that the number of
#'   positive cases in the training and test sets sums to the global total.}
#' }
#'
#' @details
#' The algorithm is greedy and attempts to simultaneously satisfy:
#' \enumerate{
#'   \item fold sizes;
#'   \item the distribution of positive cases for each outcome;
#'   \item the distribution of cases with multiple positive outcomes.
#' }
#'
#' The function converts the specified outcome variables to binary numeric
#' values (0/1) before assigning observations to folds.
#'
#' The number of folds allocated to the training set is controlled by
#' \code{train_folds}, while the remaining folds are used for the test set.
#' For example, with \code{k = 10} and \code{train_folds = 7}, approximately
#' 70% of the observations are allocated to the training set and 30% to
#' the test set.
#'
#' The \code{balanced_table} provides a summary of the distribution of
#' positive cases across the global dataset, training set, and test set.
#'
#' Missing values in the outcome variables are not allowed, and all outcomes
#' must be coded as 0/1.
#'
#' @examples
#' data <- data.frame(
#'   outcome1 = rep(c(0, 1), 10),
#'   outcome2 = rep(c(0, 0, 1, 1), 5)
#' )
#'
#' split <- split_multioutput(
#'   data = data,
#'   outcomes = c("outcome1", "outcome2"),
#'   k = 10,
#'   train_folds = 7,
#'   seed = 1234
#' )
#'
#' train_set <- split$train
#' test_set <- split$test
#' balanced_table <- split$balanced_table
#'
#' @export
split_multioutcome <- function(
    data,
    outcomes,
    k = 10,
    train_folds = 7,
    seed = NULL
) {

  # Extract outcome variables
  labels <- data[, outcomes, drop = FALSE]

  # Convert outcomes to numeric 0/1
  labels[] <- lapply(labels, function(x) {
    if (is.factor(x) || is.ordered(x)) {
      as.numeric(as.character(x))
    } else {
      as.numeric(x)
    }
  })

  labels <- as.matrix(labels)

  num_samples <- nrow(labels)
  num_labels <- ncol(labels)

  # Validate k
  if (k < 2) {
    stop("k must be >= 2.")
  }

  if (k > num_samples) {
    stop("k cannot be greater than the number of observations.")
  }

  # Validate train_folds
  if (train_folds < 1 || train_folds >= k) {
    stop("train_folds must be >= 1 and < k.")
  }

  if (train_folds != as.integer(train_folds)) {
    stop("train_folds must be an integer.")
  }

  # Check for missing values
  if (anyNA(labels)) {
    stop("Missing values (NA) are not allowed in the outcomes.")
  }

  # Check that outcomes are binary
  if (!all(labels %in% c(0, 1))) {
    stop("All outcomes must be coded as 0/1.")
  }

  # Set seed
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Initialise fold assignments
  fold_assignments <- integer(num_samples)

  desired_fold_sizes <- rep(num_samples / k, k)

  label_sums <- colSums(labels)

  desired_label_sizes <- matrix(
    rep(label_sums / k, each = k),
    nrow = k,
    ncol = num_labels
  )

  unassigned_samples <- seq_len(num_samples)

  # Assign observations to folds
  while (length(unassigned_samples) > 0) {

    remaining_label_sums <- colSums(
      labels[unassigned_samples, , drop = FALSE]
    )

    remaining_label_sums[remaining_label_sums == 0] <- Inf

    rarest_label_idx <- which.min(remaining_label_sums)

    subset_indices <- unassigned_samples[
      labels[unassigned_samples, rarest_label_idx] == 1
    ]

    if (length(subset_indices) == 0) {
      subset_indices <- unassigned_samples[1]
    }

    for (sample_idx in subset_indices) {

      if (!(sample_idx %in% unassigned_samples)) {
        next
      }

      sample_labels <- which(labels[sample_idx, ] == 1)

      if (length(sample_labels) > 0) {

        label_demands <- desired_label_sizes[
          ,
          sample_labels,
          drop = FALSE
        ]

        fold_demands <- rowSums(label_demands)

        candidate_folds <- which(
          fold_demands == max(fold_demands)
        )

      } else {

        candidate_folds <- seq_len(k)
      }

      if (length(candidate_folds) > 1) {

        size_demands <- desired_fold_sizes[candidate_folds]

        candidate_folds <- candidate_folds[
          size_demands == max(size_demands)
        ]
      }

      if (length(candidate_folds) == 1) {
        chosen_fold <- candidate_folds
      } else {
        chosen_fold <- sample(candidate_folds, 1)
      }

      fold_assignments[sample_idx] <- chosen_fold

      desired_fold_sizes[chosen_fold] <-
        desired_fold_sizes[chosen_fold] - 1

      if (length(sample_labels) > 0) {

        desired_label_sizes[
          chosen_fold,
          sample_labels
        ] <-
          desired_label_sizes[
            chosen_fold,
            sample_labels
          ] - 1
      }

      unassigned_samples <-
        setdiff(unassigned_samples, sample_idx)
    }
  }

  # Check that all observations were assigned
  if (any(fold_assignments == 0)) {
    stop("Some observations were not assigned to a fold.")
  }

  # Split data into training and test sets
  train_index <- which(
    fold_assignments <= train_folds
  )

  test_index <- which(
    fold_assignments > train_folds
  )

  train_set <- data[
    train_index,
    ,
    drop = FALSE
  ]

  test_set <- data[
    test_index,
    ,
    drop = FALSE
  ]

  # Count positive cases for each outcome
  n_global <- sapply(data[outcomes], function(x) {
    sum(
      as.numeric(as.character(x)) == 1,
      na.rm = TRUE
    )
  })

  n_tr <- sapply(train_set[outcomes], function(x) {
    sum(
      as.numeric(as.character(x)) == 1,
      na.rm = TRUE
    )
  })

  n_te <- sapply(test_set[outcomes], function(x) {
    sum(
      as.numeric(as.character(x)) == 1,
      na.rm = TRUE
    )
  })

  # Create balanced table
  balanced_table <- dplyr::tibble(
    Outcome = outcomes,
    Global_N = n_global,
    Train_N = n_tr,
    Train_Pct = round(
      (n_tr / n_global) * 100,
      2
    ),
    Test_N = n_te,
    Test_Pct = round(
      (n_te / n_global) * 100,
      2
    )
  )

  balanced_table <- dplyr::mutate(
    balanced_table,
    Check = Train_N + Test_N == Global_N
  )

  return(list(
    train = train_set,
    test = test_set,
    balanced_table = balanced_table
  ))
}

