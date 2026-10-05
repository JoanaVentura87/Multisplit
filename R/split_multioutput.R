split_multioutput <- function(data, outcomes, k = 10, seed = NULL) {

  labels <- data[, outcomes, drop = FALSE]

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

  if (k < 2) {
    stop("k must be >= 2.")
  }

  if (k > num_samples) {
    stop("k cannot be greater than the number of observations.")
  }

  if (anyNA(labels)) {
    stop("Missing values (NA) are not allowed in the outcomes.")
  }

  if (!all(labels %in% c(0, 1))) {
    stop("All outcomes must be coded as 0/1.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  fold_assignments <- integer(num_samples)

  desired_fold_sizes <- rep(num_samples / k, k)

  label_sums <- colSums(labels)

  desired_label_sizes <- matrix(
    rep(label_sums / k, each = k),
    nrow = k,
    ncol = num_labels
  )

  unassigned_samples <- seq_len(num_samples)

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

  if (any(fold_assignments == 0)) {
    stop("Some observations were not assigned to a fold.")
  }

  train_index <- which(fold_assignments <= 7)
  test_index  <- which(fold_assignments > 7)

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

  return(list(
    train = train_set,
    test = test_set
  ))
}
