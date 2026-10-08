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
#' @param training_provenance Optional provenance list stored on the returned
#'   model. Used by the curation adapter to pin the review cohort and scored
#'   input identities.
#' @param strict If `TRUE`, require a complete finite scored input and record
#'   exclusion accounting for the curation foundation path. The default keeps
#'   the generic trainer's historical feature behavior.
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
  seed = NULL,
  training_provenance = NULL,
  strict = FALSE
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
    eval_source = eval_source, include = include, strict = strict
  )
  feature_names <- attr(dat, "feature_names")
  training_audit <- attr(dat, "training_audit", exact = TRUE)

  if (isTRUE(strict)) {
    class_counts <- table(dat$decision)
    if (length(class_counts) < 2L || any(class_counts < 2L)) {
      cli::cli_abort(
        "Each training class needs at least two eligible projects for stratified out-of-fold validation.",
        class = "planscanR_error_bad_input"
      )
    }
    if (length(feature_names) == 0L) {
      cli::cli_abort(
        "No scored features are available for model training.",
        class = "planscanR_error_bad_input"
      )
    }
    varying <- vapply(dat[feature_names], function(value) {
      if (is.numeric(value)) {
        return(length(unique(value[is.finite(value)])) > 1L)
      }
      length(unique(value[!is.na(value)])) > 1L
    }, logical(1))
    if (!any(varying)) {
      cli::cli_abort(
        "The selected scored input has no varying features among eligible projects.",
        class = "planscanR_error_bad_input"
      )
    }
    v <- as.integer(v)
    if (length(v) != 1L || is.na(v) || v < 2L || v > min(class_counts)) {
      cli::cli_abort(
        "The requested number of folds must be between 2 and the smallest eligible class count ({min(class_counts)}).",
        class = "planscanR_error_bad_input"
      )
    }
  }

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
      cv_by_country = selection_metrics_from_oof_by_country(oof, threshold = threshold),
      eval_source = eval_source,
      threshold = threshold,
      v = as.integer(v),
      repeats = as.integer(repeats),
      trained_at = Sys.time(),
      oof = oof,
      cv = cv,
      training_audit = training_audit,
      training_provenance = training_provenance %||% list(),
      feature_contract = list(
        strict = isTRUE(strict),
        feature_names = feature_names
      )
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
    !duplicated(paste(d$country, d$document_id, d$.reviewer)), ,
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

# Validate the scored-input contract used by fitting.  `selection_features()`
# intentionally retains its historical zero-fill behaviour for generic
# prediction callers; fitting uses this strict preflight so an incompatible
# snapshot can never turn missing scores into training signal.
#' @noRd
selection_training_feature_contract <- function(records, topics, labels, include = character(0)) {
  if (!is.data.frame(records) || !all(c("country", "document_id") %in% names(records))) {
    cli::cli_abort(
      "The scored input is incompatible with model training: country and document_id are required.",
      class = "planscanR_error_bad_input"
    )
  }
  if (anyNA(records$country) || anyNA(records$document_id) ||
    any(!nzchar(as.character(records$country))) || any(!nzchar(as.character(records$document_id)))) {
    cli::cli_abort(
      "The scored input is incompatible with model training: project keys cannot be missing or empty.",
      class = "planscanR_error_bad_input"
    )
  }
  keys <- paste(as.character(records$country), as.character(records$document_id), sep = "\r")
  if (anyDuplicated(keys)) {
    cli::cli_abort(
      "The scored input is incompatible with model training: each country/document project key must be unique.",
      class = "planscanR_error_bad_input"
    )
  }
  numeric_features <- c(
    paste0("relevance_score_", names(topics)),
    paste0("class_score_", names(labels)),
    "kw_total"
  )
  missing_features <- setdiff(numeric_features, names(records))
  if (length(missing_features) > 0L) {
    cli::cli_abort(
      paste0(
        "The scored input is incompatible with model training: missing feature",
        if (length(missing_features) == 1L) "" else "s",
        " ", paste(missing_features, collapse = ", "), ".",
        " Select a verified snapshot with the configured scores; missing values are not manufactured."
      ),
      class = "planscanR_error_bad_input"
    )
  }
  non_numeric <- numeric_features[!vapply(records[numeric_features], is.numeric, logical(1))]
  if (length(non_numeric) > 0L) {
    cli::cli_abort(
      paste0(
        "The scored input is incompatible with model training: feature",
        if (length(non_numeric) == 1L) "" else "s",
        " must be numeric (", paste(non_numeric, collapse = ", "), ")."
      ),
      class = "planscanR_error_bad_input"
    )
  }
  complete <- rep(TRUE, nrow(records))
  for (feature in numeric_features) {
    complete <- complete & is.finite(records[[feature]])
  }
  list(
    keys = keys,
    numeric_features = numeric_features,
    feature_names = selection_feature_names(topics, labels, include = include),
    complete = complete
  )
}

#' @noRd
selection_training_unresolved_mask <- function(reviews) {
  unresolved <- rep(FALSE, nrow(reviews))
  resolved_current <- rep(FALSE, nrow(reviews))
  if ("mutation_allowed" %in% names(reviews)) {
    allowed <- as.logical(reviews$mutation_allowed)
    unresolved <- unresolved | is.na(allowed) | !allowed
  } else {
    allowed <- rep(NA, nrow(reviews))
  }
  final_linkage <- if ("linkage_status" %in% names(reviews)) {
    tolower(trimws(as.character(reviews$linkage_status)))
  } else if ("current_linkage" %in% names(reviews)) {
    tolower(trimws(as.character(reviews$current_linkage)))
  } else {
    rep(NA_character_, nrow(reviews))
  }
  resolved_current <- !is.na(final_linkage) & final_linkage %in% c("current_unique", "matched")
  if ("mutation_allowed" %in% names(reviews)) {
    resolved_current <- resolved_current & !is.na(allowed) & allowed
  }
  if ("linkage_status" %in% names(reviews)) {
    value <- tolower(trimws(as.character(reviews$linkage_status)))
    unresolved <- unresolved | (!is.na(value) & value %in% c(
      "ambiguous", "pending", "unseen", "unresolved", "unverified", "historical_only"
    ))
  }
  for (column in intersect(c("current_linkage", "historical_linkage"), names(reviews))) {
    value <- tolower(trimws(as.character(reviews[[column]])))
    unresolved <- unresolved | (!resolved_current & !is.na(value) & value %in% c(
      "ambiguous", "pending", "unseen", "unresolved", "unverified", "historical_only"
    ))
  }
  unresolved
}

#' @noRd
selection_training_consensus_audit <- function(reviews) {
  verdicts <- reviews[reviews$decision %in% c("keep", "drop"), , drop = FALSE]
  if (nrow(verdicts) == 0L) {
    return(list(conflict_keys = character(0), consensus_rows = 0L))
  }
  verdicts$.ra <- parse_reviewed_at_chr(verdicts$reviewed_at)
  reviewer <- if ("reviewer" %in% names(verdicts)) {
    as.character(verdicts$reviewer)
  } else if ("reviewer_id" %in% names(verdicts)) {
    as.character(verdicts$reviewer_id)
  } else {
    rep("", nrow(verdicts))
  }
  reviewer[is.na(reviewer)] <- ""
  verdicts$.reviewer <- reviewer
  verdicts <- verdicts[order(verdicts$.ra, decreasing = TRUE), , drop = FALSE]
  verdicts <- verdicts[!duplicated(paste(
    verdicts$country, verdicts$document_id, verdicts$.reviewer
  )), , drop = FALSE]
  key <- paste(verdicts$country, verdicts$document_id, sep = "\r")
  groups <- split(seq_len(nrow(verdicts)), key)
  decisions <- vapply(groups, function(index) length(unique(verdicts$decision[index])), integer(1))
  list(
    conflict_keys = names(decisions)[decisions > 1L],
    consensus_rows = sum(decisions == 1L)
  )
}

# Join records to consensus keep/drop labels (see consensus_reviews()) and
# attach the feature frame. Returns a tibble with a `decision` factor (levels
# keep, drop — keep is the positive/event level) and a `"feature_names"`
# attribute. Strict fitting also carries a compact audit for the model
# artifact; no unavailable score is converted to zero on the fit path.
#' @noRd
build_training_frame <- function(
  records,
  reviews,
  topics,
  labels,
  eval_source = "random",
  include = character(0),
  strict = FALSE
) {
  if (!isTRUE(strict)) {
    # Preserve the generic trainer's historical zero-fill and review semantics.
    # The curation foundation path opts into the stricter branch below so a
    # verified snapshot cannot silently manufacture a training feature.
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
    return(dat)
  }

  if (!is.data.frame(reviews) || !all(c("country", "document_id", "decision", "source") %in% names(reviews))) {
    cli::cli_abort(
      "The review input must include country, document_id, decision, and source columns.",
      class = "planscanR_error_bad_input"
    )
  }
  source_mask <- if (is.null(eval_source)) rep(TRUE, nrow(reviews)) else reviews$source %in% eval_source
  src <- reviews[source_mask, , drop = FALSE]
  source_excluded <- sum(!source_mask)
  if (sum(src$decision %in% c("keep", "drop")) == 0L) {
    src_txt <- if (is.null(eval_source)) "" else sprintf(" for source '%s'", paste(eval_source, collapse = ", "))
    cli::cli_abort(
      "No keep/drop labels found{src_txt}.",
      class = "planscanR_error_bad_input"
    )
  }

  unresolved_mask <- selection_training_unresolved_mask(src)
  unresolved_keys <- unique(paste(
    src$country[unresolved_mask], src$document_id[unresolved_mask],
    sep = "\r"
  ))
  unresolved_keys <- unresolved_keys[nzchar(unresolved_keys) & !is.na(unresolved_keys)]
  src_key <- paste(src$country, src$document_id, sep = "\r")
  src_clean <- src[!unresolved_mask & !(src_key %in% unresolved_keys), , drop = FALSE]
  consensus_audit <- selection_training_consensus_audit(src_clean)

  # One agreed label per project precedes any fold construction. Conflicting
  # records are excluded as ambiguous ground truth; unsure rows are reported
  # separately and never become keep/drop labels.
  dec <- consensus_reviews(src_clean)
  if (nrow(dec) == 0L) {
    cli::cli_abort(
      "No agreed keep/drop labels: every eligible project has an unresolved or conflicting decision.",
      class = "planscanR_error_bad_input"
    )
  }

  contract <- if (isTRUE(strict)) {
    selection_training_feature_contract(records, topics, labels, include = include)
  } else {
    list(
      keys = paste(records$country, records$document_id, sep = "\r"),
      numeric_features = c(paste0("relevance_score_", names(topics)), paste0("class_score_", names(labels)), "kw_total"),
      feature_names = selection_feature_names(topics, labels, include = include),
      complete = rep(TRUE, nrow(records))
    )
  }
  feats <- selection_features(records, topics, labels, include = include)
  feature_names <- attr(feats, "feature_names")
  key_f <- contract$keys
  key_d <- paste(dec$country, dec$document_id, sep = "\r")
  m <- match(key_d, key_f)
  matched <- !is.na(m)
  complete_match <- matched & contract$complete[m]
  if (!any(matched)) {
    cli::cli_abort(
      "None of the labelled records were found in {.arg records}.",
      class = "planscanR_error_bad_input"
    )
  }

  dat <- feats[m[complete_match], , drop = FALSE]
  dat$decision <- factor(dec$decision[complete_match], levels = c("keep", "drop"))
  if (nlevels(droplevels(dat$decision)) < 2L) {
    cli::cli_abort(
      "Need both {.val keep} and {.val drop} labels to train; only one class is present among compatible projects.",
      class = "planscanR_error_bad_input"
    )
  }
  audit <- list(
    consensus_before_folds = TRUE,
    source = list(
      requested = eval_source,
      considered = nrow(src),
      excluded = source_excluded,
      corrected = if ("original_source" %in% names(src)) {
        sum(!is.na(src$original_source) & nzchar(as.character(src$original_source)) &
          as.character(src$original_source) != as.character(src$source))
      } else {
        0L
      }
    ),
    snapshot = list(
      rows = nrow(records),
      countries = sort(unique(as.character(records$country))),
      feature_names = feature_names
    ),
    consensus = list(
      rows = nrow(dec),
      conflicts = length(consensus_audit$conflict_keys),
      projects = unique(paste(dec$country, dec$document_id, sep = "\r"))
    ),
    eligible = list(
      projects = sum(complete_match),
      keep = sum(dec$decision[complete_match] == "keep"),
      drop = sum(dec$decision[complete_match] == "drop"),
      features = sum(complete_match)
    ),
    exclusions = list(
      source = source_excluded,
      unsure = sum(src$decision == "unsure", na.rm = TRUE),
      unresolved = length(unresolved_keys),
      disagreement = length(consensus_audit$conflict_keys),
      unmatched = sum(!matched),
      incompatible = sum(matched & !contract$complete[m])
    )
  )
  attr(dat, "feature_names") <- feature_names
  attr(dat, "training_audit") <- audit
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

#' @noRd
selection_metrics_from_oof_by_country <- function(oof, threshold = 0.5) {
  if (is.null(oof) || nrow(oof) == 0L) {
    return(NULL)
  }
  groups <- split(seq_len(nrow(oof)), oof$country)
  rows <- lapply(names(groups), function(country) {
    metrics <- selection_metrics_from_oof(oof[groups[[country]], , drop = FALSE], threshold = threshold)
    metrics$country <- country
    metrics
  })
  overall <- selection_metrics_from_oof(oof, threshold = threshold)
  overall$country <- "all"
  result <- dplyr::bind_rows(c(list(overall), rows))
  result[, c("country", setdiff(names(result), "country")), drop = FALSE]
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
  selection_metrics_from_oof_by_country(oof, threshold = thr)
}

# Compute learned keep probabilities without applying a decision cutoff.  The
# curation app can then move a threshold over this stable vector without
# invoking the model workflow again.
predict_selection_probabilities <- function(model, records) {
  if (!inherits(model, "planscanR_selection_model")) {
    cli::cli_abort("{.arg model} must be a planscanR_selection_model.")
  }
  require_tidymodels()
  if (nrow(records) == 0L) {
    records$select_prob <- numeric(0)
    return(records)
  }
  feats <- selection_features(
    records, model$topics, model$labels,
    include = model$include
  )
  p <- stats::predict(model$workflow, new_data = feats, type = "prob")
  records$select_prob <- p[[".pred_keep"]]
  records
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
  thr <- threshold %||% model$threshold
  records <- predict_selection_probabilities(model, records)
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
