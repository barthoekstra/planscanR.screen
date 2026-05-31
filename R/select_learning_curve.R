# Learning curve for the learned BIOGAIN selection model.
#
# Estimate how the model's held-out F1 / precision / recall improve as the number
# of human training labels grows, so the user can see where adding more labels
# stops helping. This is an HONEST held-out protocol, not in-sample: within each
# repeat we hold out a FIXED test set and grow only the training subsample, so
# the metric across sizes is comparable (the same records are scored every time).

#' Learning curve for the learned selection model.
#'
#' Estimates how the model's held-out F1 / precision / recall improve as the
#' number of human keep/drop labels grows. For each repeat it makes one
#' stratified train/test split (the test set is FIXED across all training sizes
#' within that repeat, so the metric is comparable as the training pool grows),
#' then fits the learner on increasing stratified subsamples of the training pool
#' and scores the held-out test set. Repeating the whole thing `repeats` times and
#' averaging (see [learning_curve_summary()]) smooths out the split noise and
#' shows where the curve flattens.
#'
#' @param records A scored + classified tibble (from [planscanR::get_assessments()],
#'   [planscanR::index_cache()], or the review-app snapshot) carrying the
#'   [selection_features()] columns.
#' @param reviews The review-decision tibble (the app's `reviews.csv`), with
#'   `document_id`, `country`, `decision`, `source`, `reviewed_at`.
#' @param topics,labels The topic and classifier-label vectors naming the
#'   feature columns (required); see [selection_feature_names()].
#' @param learner A [selection_learner]. Defaults to
#'   [selection_learner_logistic()].
#' @param sizes Optional integer vector of training-label counts to evaluate.
#'   `NULL` builds a grid of ~10-12 increasing sizes from 30 up to the maximum
#'   train-pool size (always including that maximum). Supplied sizes larger than
#'   the train pool are dropped, but the pool maximum is always kept.
#' @param test_frac Fraction of the labelled data held out as the fixed test set
#'   (per repeat).
#' @param repeats Number of repeated held-out resamples.
#' @param eval_source Restrict labels to this review `source` (default
#'   `"random"` — the unbiased sample). `NULL` uses every keep/drop label.
#' @param threshold Probability cutoff for the keep decision when scoring.
#' @param seed Optional RNG seed for reproducible splits.
#' @param by_country If `TRUE`, also slice the held-out test set per country and
#'   compute one set of metrics per country at every (size, repeat), plus an
#'   `"all"` row that matches the default output. A `country` column is
#'   prepended. The model is still trained corpus-wide — this only varies the
#'   evaluation slice, so it shows where the shared model performs as the
#'   training pool grows. Slices smaller than 1 record are dropped silently.
#' @return A long tibble with one row per (size, repeat), columns in order:
#'   `size`, `n_train_used`, `rep`, `n_test`, `precision`, `recall`, `f1`. When
#'   `by_country = TRUE`, a leading `country` column is added and there is one
#'   row per (country, size, repeat) plus an `"all"` row per (size, repeat).
#' @seealso [learning_curve_summary()] to aggregate the curve.
#' @export
#' @examples
#' \dontrun{
#' recs <- index_cache(country = "nl")
#' rev <- read.csv(file.path(cache_dir, "reviews.csv"), colClasses = "character")
#' curve <- selection_learning_curve(recs, rev, repeats = 10)
#' learning_curve_summary(curve)
#' }
selection_learning_curve <- function(
  records,
  reviews,
  topics,
  labels,
  learner = selection_learner_logistic(),
  sizes = NULL,
  test_frac = 0.25,
  repeats = 10,
  eval_source = "random",
  threshold = 0.5,
  seed = NULL,
  by_country = FALSE
) {
  if (!inherits(learner, "planscanR_selection_learner")) {
    cli::cli_abort(
      "{.arg learner} must be a planscanR_selection_learner.",
      class = "planscanR_error_bad_input"
    )
  }
  if (missing(topics) || missing(labels)) {
    cli::cli_abort(
      c(
        "{.arg topics} and {.arg labels} are required.",
        i = "Pass the topic and classifier-label vectors (see {.fn selection_feature_names})."
      ),
      class = "planscanR_error_bad_input"
    )
  }
  if (!is.numeric(test_frac) || length(test_frac) != 1L || test_frac <= 0 || test_frac >= 1) {
    cli::cli_abort(
      "{.arg test_frac} must be a single number in (0, 1).",
      class = "planscanR_error_bad_input"
    )
  }
  repeats <- as.integer(repeats)
  if (is.na(repeats) || repeats < 1L) {
    cli::cli_abort(
      "{.arg repeats} must be a positive integer.",
      class = "planscanR_error_bad_input"
    )
  }
  require_tidymodels(learner$engine_pkg)

  dat <- build_training_frame(records, reviews, topics, labels, eval_source = eval_source)
  feature_names <- attr(dat, "feature_names")

  if (!is.null(seed) && !is.na(seed)) {
    set.seed(as.integer(seed))
  }

  n_total <- nrow(dat)
  max_train <- floor((1 - test_frac) * n_total)
  if (max_train < 2L) {
    cli::cli_abort(
      "Too few labelled records ({n_total}) to build a learning curve.",
      class = "planscanR_error_bad_input"
    )
  }
  sizes <- resolve_learning_curve_sizes(sizes, max_train)

  parts <- lapply(seq_len(repeats), function(r) {
    # One fixed held-out test set per repeat: the SAME test records score every
    # training size, so F1 across sizes is comparable within the repeat.
    split <- rsample::initial_split(dat, prop = 1 - test_frac, strata = "decision")
    train <- rsample::training(split)
    test <- rsample::testing(split)

    rows <- lapply(sizes, function(n) {
      sub <- stratified_subsample(train, "decision", n)
      if (is.null(sub)) {
        return(NULL)
      }
      wf <- build_selection_workflow(learner, sub, feature_names)
      fit_i <- parsnip::fit(wf, data = sub)
      preds <- stats::predict(fit_i, new_data = test, type = "prob")[[".pred_keep"]]
      oof <- tibble::tibble(
        .pred_keep = preds,
        truth = test$decision,
        country = test$country
      )
      base <- tibble::tibble(
        size = as.integer(n),
        n_train_used = nrow(sub),
        rep = as.integer(r)
      )
      slice_row <- function(slice_oof, cc) {
        if (nrow(slice_oof) == 0L) {
          return(NULL)
        }
        m <- selection_metrics_from_oof(slice_oof, threshold = threshold)
        out <- base
        if (!is.null(cc)) {
          out$country <- cc
        }
        out$n_test <- nrow(slice_oof)
        out$precision <- m$precision
        out$recall <- m$recall
        out$f1 <- m$f1
        out
      }
      if (!by_country) {
        return(slice_row(oof, NULL))
      }
      groups <- split(seq_len(nrow(oof)), oof$country)
      per_country <- lapply(names(groups), function(cc) {
        slice_row(oof[groups[[cc]], , drop = FALSE], cc)
      })
      all_row <- slice_row(oof, "all")
      dplyr::bind_rows(c(list(all_row), per_country))
    })
    out <- dplyr::bind_rows(rows)
    if (by_country && nrow(out) > 0L) {
      out <- out[, c("country", setdiff(names(out), "country")), drop = FALSE]
    }
    out
  })

  dplyr::bind_rows(parts)
}

#' Aggregate a learning curve to mean +/- sd per training size.
#'
#' @param curve The long tibble returned by [selection_learning_curve()].
#' @return A tibble with one row per `size` (or per (country, size) when the
#'   `curve` carries a `country` column): `size`, `n_train_used`, `n` (number
#'   of repeats contributing), and the mean/sd of `f1`, `precision`, `recall`.
#' @export
#' @examples
#' \dontrun{
#' learning_curve_summary(selection_learning_curve(recs, rev))
#' }
learning_curve_summary <- function(curve) {
  if (is.null(curve) || nrow(curve) == 0L) {
    cli::cli_abort(
      "{.arg curve} is empty; nothing to summarise.",
      class = "planscanR_error_bad_input"
    )
  }
  grouped <- if ("country" %in% names(curve)) {
    dplyr::group_by(curve, .data$country, .data$size)
  } else {
    dplyr::group_by(curve, .data$size)
  }
  out <- dplyr::summarise(
    grouped,
    n_train_used = round(mean(.data$n_train_used)),
    n = dplyr::n(),
    f1_mean = mean(.data$f1, na.rm = TRUE),
    f1_sd = stats::sd(.data$f1, na.rm = TRUE),
    precision_mean = mean(.data$precision, na.rm = TRUE),
    precision_sd = stats::sd(.data$precision, na.rm = TRUE),
    recall_mean = mean(.data$recall, na.rm = TRUE),
    recall_sd = stats::sd(.data$recall, na.rm = TRUE),
    .groups = "drop"
  )
  if ("country" %in% names(out)) {
    dplyr::arrange(out, .data$country, .data$size)
  } else {
    dplyr::arrange(out, .data$size)
  }
}

# Build a grid of ~10-12 increasing integer sizes from 30 up to `max_train`,
# always including `max_train`. If the pool is small, degrade to fewer points.
# A supplied `sizes` is sorted, deduped, dropped above the pool, and the pool max
# re-added.
#' @noRd
resolve_learning_curve_sizes <- function(sizes, max_train) {
  if (!is.null(sizes)) {
    s <- sort(unique(as.integer(sizes)))
    s <- s[s >= 1L & s <= max_train]
    s <- unique(c(s, max_train))
    return(sort(s))
  }
  start <- min(30L, max_train)
  if (start >= max_train) {
    return(max_train)
  }
  grid <- unique(round(seq(start, max_train, length.out = 11L)))
  grid <- as.integer(grid[grid >= 1L])
  sort(unique(c(grid, max_train)))
}

# Draw a stratified subsample of up to `n` rows from `data`, preserving the class
# balance of `strata` and guaranteeing >=1 row of each class. Returns NULL if
# `n` is too small to include both classes.
#' @noRd
stratified_subsample <- function(data, strata, n) {
  n <- min(as.integer(n), nrow(data))
  groups <- split(seq_len(nrow(data)), data[[strata]])
  groups <- groups[lengths(groups) > 0L]
  k <- length(groups)
  if (n < k) {
    return(NULL)
  }
  props <- lengths(groups) / nrow(data)
  take <- pmax(1L, round(props * n))
  # Trim/grow to hit `n` exactly while never dropping a class below 1.
  while (sum(take) > n) {
    i <- which.max(take)
    if (take[i] <= 1L) {
      break
    }
    take[i] <- take[i] - 1L
  }
  while (sum(take) < n) {
    headroom <- lengths(groups) - take
    i <- which.max(headroom)
    if (headroom[i] <= 0L) {
      break
    }
    take[i] <- take[i] + 1L
  }
  idx <- unlist(lapply(seq_along(groups), function(j) {
    g <- groups[[j]]
    sample(g, min(take[j], length(g)))
  }))
  data[idx, , drop = FALSE]
}
