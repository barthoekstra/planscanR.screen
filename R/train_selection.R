# Train / predict / evaluate the learned selection model.
#
# This is the supervised alternative to a hand-tuned threshold rule: instead of
# OR-ing the three signals at fixed cutoffs, it LEARNS the keep/drop decision
# from human review labels over the per-record scores already on the sidecars
# (see selection_features()).
#
# Honest evaluation is the whole point. Metrics reported here are
# OUT-OF-FOLD: every record is scored by a model that did NOT see it in
# training (k-fold CV), so there is no train-on-test inflation and they compare
# directly against any hand-rule baseline on the same review set. By default
# both training and CV run on the unbiased random sample (`source == "random"`).

#' Train the learned selection model from human review labels.
#'
#' @param records A scored + classified tibble (from [planscanR::get_assessments()],
#'   [planscanR::index_cache()], or a review-app snapshot) carrying the
#'   [selection_features()] columns. Only records that also appear in `reviews`
#'   with a keep/drop decision are used for training.
#' @param reviews The review-decision tibble (e.g. a review tool's
#'   `reviews.csv`), with `document_id`, `country`, `decision`, `source`,
#'   `reviewed_at`.
#' @param topics,labels The topic and classifier-label vectors naming the
#'   feature columns (required); see [selection_feature_names()]. Stored on the
#'   returned model so [predict_selection()] rebuilds the same feature frame.
#' @param learner A [selection_learner]. Defaults to
#'   [selection_learner_logistic()].
#' @param eval_source Restrict labels to this review `source` (default
#'   `"random"` — the unbiased sample). `NULL` uses every keep/drop label.
#' @param include Optional extra feature columns; see [selection_features()].
#' @param v,repeats Cross-validation folds and repeats for the out-of-fold
#'   metrics.
#' @param threshold Default probability cutoff for the keep decision.
#' @param seed Optional RNG seed for reproducible folds.
#' @return A `planscanR_selection_model`: the fitted workflow plus provenance
#'   (`learner_name`, `features`, `n_train`, `trained_at`, ...), the out-of-fold
#'   predictions (`oof`), and the CV metrics at `threshold` (`cv`).
#' @export
#' @examples
#' \dontrun{
#' recs <- index_cache(country = "nl")
#' rev <- read.csv(file.path(cache_dir, "reviews.csv"))
#' m <- train_selection_model(recs, rev)
#' m$cv
#' }
train_selection_model <- function(
  records,
  reviews,
  topics,
  labels,
  learner = selection_learner_logistic(),
  eval_source = "random",
  include = character(0),
  v = 5L,
  repeats = 1L,
  threshold = 0.5,
  seed = NULL
) {
  if (!inherits(learner, "planscanR_selection_learner")) {
    cli::cli_abort("{.arg learner} must be a planscanR_selection_learner.")
  }
  if (missing(topics) || missing(labels)) {
    cli::cli_abort(c(
      "{.arg topics} and {.arg labels} are required.",
      i = "Pass the topic and classifier-label vectors used to score and classify the records."
    ))
  }
  require_tidymodels(learner$engine_pkg)

  dat <- build_training_frame(
    records, reviews, topics, labels,
    eval_source = eval_source, include = include
  )
  feature_names <- attr(dat, "feature_names")

  if (!is.null(seed) && !is.na(seed)) {
    set.seed(as.integer(seed))
  }

  # Final model: fit on ALL labelled rows (the model that ships / predicts).
  wf <- build_selection_workflow(learner, dat, feature_names)
  fitted <- parsnip::fit(wf, data = dat)

  # Honest metrics: out-of-fold predictions from stratified k-fold CV.
  oof <- cv_oof_predictions(learner, dat, feature_names, v = v, repeats = repeats)
  cv <- selection_metrics_from_oof(oof, threshold = threshold)

  structure(
    list(
      workflow = fitted,
      learner_name = learner$name,
      features = feature_names,
      topics = topics,
      labels = labels,
      include = include,
      n_train = nrow(dat),
      n_keep = sum(dat$decision == "keep"),
      n_by_country = as.list(table(dat$country)),
      eval_source = eval_source,
      threshold = threshold,
      v = as.integer(v),
      repeats = as.integer(repeats),
      trained_at = Sys.time(),
      oof = oof,
      cv = cv
    ),
    class = "planscanR_selection_model"
  )
}

#' Resolve a review store to one agreed decision per record.
#'
#' Collapses the per-(record, reviewer) review rows into a single ground-truth
#' decision per `(country, document_id)`, in two steps:
#'
#' 1. **Per reviewer:** keep each reviewer's *most-recent* verdict on a record
#'    (so re-deciding overrides an earlier opinion).
#' 2. **Across reviewers:** keep the record only if all its reviewers
#'    **unanimously agree**. Records with conflicting decisions are dropped as
#'    ambiguous ground truth.
#'
#' Records reviewed by a single reviewer trivially pass. This is the rule used
#' for both the training labels ([train_selection_model()]) and the
#' automated-vs-human comparison, so a disagreement never silently becomes a
#' label.
#'
#' @param reviews The review-decision tibble (e.g. a review tool's
#'   `reviews.csv`), with `document_id`, `country`, `decision`, `reviewer`,
#'   `reviewed_at`.
#' @param decisions Decisions treated as a verdict. Default `c("keep", "drop")`
#'   (so `"unsure"` rows are ignored).
#' @return A tibble with one row per agreed record: `document_id`, `country`,
#'   `decision`, `n_reviewers`, `reviewed_at` (the latest among the agreeing
#'   reviewers). Empty if no record has an agreed decision.
#' @export
#' @examples
#' \dontrun{
#' rev <- read.csv("reviews.csv", colClasses = "character")
#' consensus_reviews(rev)
#' }
consensus_reviews <- function(reviews, decisions = c("keep", "drop")) {
  empty <- tibble::tibble(
    document_id = character(0),
    country = character(0),
    decision = character(0),
    n_reviewers = integer(0),
    reviewed_at = as.POSIXct(character(0), tz = "UTC")
  )
  if (is.null(reviews) || nrow(reviews) == 0L) {
    return(empty)
  }
  d <- reviews[reviews$decision %in% decisions, , drop = FALSE]
  if (nrow(d) == 0L) {
    return(empty)
  }
  d$.ra <- parse_reviewed_at_chr(d$reviewed_at)
  rv <- if ("reviewer" %in% names(d)) as.character(d$reviewer) else NA_character_
  rv[is.na(rv)] <- ""
  d$.reviewer <- rv

  # 1. each reviewer's most-recent verdict per record.
  d <- d[order(d$.ra, decreasing = TRUE), , drop = FALSE]
  per_reviewer <- d[
    !duplicated(paste(d$country, d$document_id, d$.reviewer)),
    ,
    drop = FALSE
  ]

  # 2. keep a record only when its reviewers unanimously agree.
  key <- paste(per_reviewer$country, per_reviewer$document_id)
  parts <- lapply(split(seq_len(nrow(per_reviewer)), key), function(ix) {
    rows <- per_reviewer[ix, , drop = FALSE]
    if (length(unique(rows$decision)) != 1L) {
      return(NULL) # conflicting reviews -> drop the record
    }
    tibble::tibble(
      document_id = rows$document_id[1L],
      country = rows$country[1L],
      decision = rows$decision[1L],
      n_reviewers = length(unique(rows$.reviewer)),
      reviewed_at = max(rows$.ra, na.rm = TRUE)
    )
  })
  out <- dplyr::bind_rows(parts)
  if (nrow(out) == 0L) {
    return(empty)
  }
  out
}

# Join records to consensus keep/drop labels (see consensus_reviews()) and
# attach the feature frame. Returns a tibble with a `decision` factor (levels
# keep, drop — keep is the positive/event level) and a `"feature_names"`
# attribute.
#' @noRd
build_training_frame <- function(records, reviews, topics, labels, eval_source = "random", include = character(0)) {
  src <- reviews
  if (!is.null(eval_source)) {
    src <- src[src$source %in% eval_source, , drop = FALSE]
  }
  if (sum(src$decision %in% c("keep", "drop")) == 0L) {
    src_txt <- if (is.null(eval_source)) {
      ""
    } else {
      sprintf(" for source '%s'", paste(eval_source, collapse = ", "))
    }
    cli::cli_abort(
      "No keep/drop labels found{src_txt}.",
      class = "planscanR_error_bad_input"
    )
  }
  # One agreed label per record: a multiply-reviewed record is used only when
  # its reviewers unanimously agree; conflicting records are excluded.
  dec <- consensus_reviews(src)
  if (nrow(dec) == 0L) {
    cli::cli_abort(
      "No agreed keep/drop labels: every multiply-reviewed record has conflicting decisions.",
      class = "planscanR_error_bad_input"
    )
  }

  feats <- selection_features(records, topics, labels, include = include)
  feature_names <- attr(feats, "feature_names")
  key_f <- paste(feats$country, feats$document_id)
  key_d <- paste(dec$country, dec$document_id)
  m <- match(key_d, key_f)
  ok <- !is.na(m)
  if (!any(ok)) {
    cli::cli_abort(
      "None of the labelled records were found in {.arg records}.",
      class = "planscanR_error_bad_input"
    )
  }

  dat <- feats[m[ok], , drop = FALSE]
  dat$decision <- factor(dec$decision[ok], levels = c("keep", "drop"))
  if (nlevels(droplevels(dat$decision)) < 2L) {
    cli::cli_abort(
      "Need both {.val keep} and {.val drop} labels to train; only one class present.",
      class = "planscanR_error_bad_input"
    )
  }
  attr(dat, "feature_names") <- feature_names
  dat
}

# Collect out-of-fold predicted P(keep) for every labelled row via stratified
# k-fold CV. Returns tibble(document_id, country, truth, .pred_keep).
#' @noRd
cv_oof_predictions <- function(learner, dat, feature_names, v = 5L, repeats = 1L) {
  folds <- rsample::vfold_cv(dat, v = v, repeats = repeats, strata = "decision")
  parts <- lapply(seq_len(nrow(folds)), function(i) {
    split <- folds$splits[[i]]
    tr <- rsample::analysis(split)
    te <- rsample::assessment(split)
    wf <- build_selection_workflow(learner, tr, feature_names)
    fit_i <- parsnip::fit(wf, data = tr)
    p <- stats::predict(fit_i, new_data = te, type = "prob")
    tibble::tibble(
      document_id = te$document_id,
      country = te$country,
      truth = te$decision,
      .pred_keep = p[[".pred_keep"]]
    )
  })
  oof <- dplyr::bind_rows(parts)
  # With repeats > 1 a record appears in multiple assessment sets; average its
  # predicted probability so each record contributes one out-of-fold score.
  if (as.integer(repeats) > 1L) {
    oof <- dplyr::summarise(
      dplyr::group_by(oof, .data$document_id, .data$country, .data$truth),
      .pred_keep = mean(.data[[".pred_keep"]]),
      .groups = "drop"
    )
  }
  oof
}

# Confusion counts + precision/recall/F1 of a keep-probability vector against
# the human truth at a given threshold. The one-row tibble shape (n_reviewed,
# tp/fp/fn/tn, precision/recall/f1) is reused by selection_cv_metrics() and the
# learning curve so every metrics row formats identically.
#' @noRd
selection_metrics_from_oof <- function(oof, threshold = 0.5) {
  if (is.null(oof) || nrow(oof) == 0L) {
    return(NULL)
  }
  pred_keep <- oof$.pred_keep >= threshold
  human_keep <- oof$truth == "keep"
  tp <- sum(pred_keep & human_keep)
  fp <- sum(pred_keep & !human_keep)
  fn <- sum(!pred_keep & human_keep)
  tn <- sum(!pred_keep & !human_keep)
  precision <- if ((tp + fp) > 0) tp / (tp + fp) else NA_real_
  recall <- if ((tp + fn) > 0) tp / (tp + fn) else NA_real_
  f1 <- if (!is.na(precision) && !is.na(recall) && (precision + recall) > 0) {
    2 * precision * recall / (precision + recall)
  } else {
    NA_real_
  }
  tibble::tibble(
    n_reviewed = nrow(oof),
    tp = tp,
    fp = fp,
    fn = fn,
    tn = tn,
    precision = precision,
    recall = recall,
    f1 = f1
  )
}

#' Out-of-fold metrics for a trained model at an arbitrary threshold.
#'
#' Recomputes precision/recall/F1 + confusion from the model's stored
#' out-of-fold predictions — no retraining — so a UI can sweep the decision
#' threshold cheaply.
#'
#' @param model A `planscanR_selection_model`.
#' @param threshold Probability cutoff (defaults to the model's own).
#' @param by_country If `TRUE`, return one row per country plus an `"all"` row
#'   (a `country` column is prepended); otherwise a single overall row.
#' @return A metrics tibble (one row, or one per country + `"all"` when
#'   `by_country = TRUE`), or `NULL` if the model has no OOF data.
#' @export
selection_cv_metrics <- function(model, threshold = NULL, by_country = FALSE) {
  if (!inherits(model, "planscanR_selection_model")) {
    cli::cli_abort("{.arg model} must be a planscanR_selection_model.")
  }
  thr <- threshold %||% model$threshold
  oof <- model$oof
  if (!by_country) {
    return(selection_metrics_from_oof(oof, threshold = thr))
  }
  if (is.null(oof) || nrow(oof) == 0L) {
    return(NULL)
  }
  groups <- split(seq_len(nrow(oof)), oof$country)
  rows <- lapply(names(groups), function(cc) {
    m <- selection_metrics_from_oof(oof[groups[[cc]], , drop = FALSE], threshold = thr)
    m$country <- cc
    m
  })
  allm <- selection_metrics_from_oof(oof, threshold = thr)
  allm$country <- "all"
  out <- dplyr::bind_rows(c(list(allm), rows))
  out[, c("country", setdiff(names(out), "country")), drop = FALSE]
}

#' Predict the learned selection decision for records.
#'
#' Adds two columns: `select_prob` (the model's P(keep)) and `selected_model`
#' (logical, `select_prob >= threshold`). Network-free — it reuses the per-record
#' scores already on the sidecars via [selection_features()].
#'
#' @param model A `planscanR_selection_model`.
#' @param records A tibble carrying the [selection_features()] columns.
#' @param threshold Probability cutoff (defaults to the model's own).
#' @return `records` with `select_prob` and `selected_model` added.
#' @export
#' @examples
#' \dontrun{
#' recs <- predict_selection(model, index_cache(country = "de"))
#' table(recs$selected_model)
#' }
predict_selection <- function(model, records, threshold = NULL) {
  if (!inherits(model, "planscanR_selection_model")) {
    cli::cli_abort("{.arg model} must be a planscanR_selection_model.")
  }
  require_tidymodels()
  thr <- threshold %||% model$threshold
  if (nrow(records) == 0L) {
    records$select_prob <- numeric(0)
    records$selected_model <- logical(0)
    return(records)
  }
  feats <- selection_features(
    records, model$topics, model$labels,
    include = model$include
  )
  p <- stats::predict(model$workflow, new_data = feats, type = "prob")
  records$select_prob <- p[[".pred_keep"]]
  records$selected_model <- records$select_prob >= thr
  records
}

#' Persist / restore a trained selection model.
#'
#' Thin wrappers over [saveRDS()] / [readRDS()] with a class check, so the app
#' and the acquisition runbook can share one artifact.
#'
#' @param model A `planscanR_selection_model`.
#' @param path File path (conventionally `selection_model.rds` in the cache
#'   root, alongside `reviews.csv`).
#' @return `save_selection_model()` returns `path` invisibly;
#'   `load_selection_model()` returns the model (or `NULL` if absent).
#' @export
save_selection_model <- function(model, path) {
  if (!inherits(model, "planscanR_selection_model")) {
    cli::cli_abort("{.arg model} must be a planscanR_selection_model.")
  }
  saveRDS(model, path)
  invisible(path)
}

#' @rdname save_selection_model
#' @export
load_selection_model <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  m <- readRDS(path)
  if (!inherits(m, "planscanR_selection_model")) {
    cli::cli_abort("{.file {path}} does not contain a planscanR_selection_model.")
  }
  m
}

#' @export
print.planscanR_selection_model <- function(x, ...) {
  cat(sprintf("<planscanR_selection_model> %s\n", x$learner_name))
  cat(sprintf(
    "  trained on %d labels (%d keep) from source = %s\n",
    x$n_train,
    x$n_keep,
    x$eval_source %||% "all"
  ))
  if (!is.null(x$cv)) {
    cat(sprintf(
      "  CV (%d-fold x%d) @ thr %.2f:  P=%.2f  R=%.2f  F1=%.2f\n",
      x$v,
      x$repeats,
      x$threshold,
      x$cv$precision,
      x$cv$recall,
      x$cv$f1
    ))
  }
  invisible(x)
}

# Parse the review store's ISO-8601 "T" timestamp into POSIXct (UTC).
#' @noRd
parse_reviewed_at_chr <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(x)
  }
  x <- as.character(x)
  ts <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  bad <- is.na(ts) & !is.na(x) & nzchar(x)
  if (any(bad)) {
    ts[bad] <- as.POSIXct(x[bad], tz = "UTC")
  }
  ts
}
