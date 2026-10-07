# Configuration-aware adapters for the learned selection model.
#
# Curation callers supply a validated configuration and review scope here. The
# learned-model implementation remains in train_selection_model(),
# predict_selection(), and selection_learning_curve(); these functions only
# resolve configuration values and the existing learner registry.

curation_model_abort <- function(message) {
  curation_abort(message, class = "planscanR_error_curation_model")
}

curation_model_check_model <- function(model) {
  if (!inherits(model, "planscanR_selection_model")) {
    curation_model_abort("{.arg model} must be a planscanR_selection_model.")
  }
  invisible(model)
}

curation_model_same_named_vector <- function(x, y) {
  is.character(x) &&
    is.character(y) &&
    identical(names(x), names(y)) &&
    identical(unname(x), unname(y))
}

curation_model_check_config <- function(model, config) {
  curation_model_check_model(model)
  if (is.null(config)) {
    return(invisible(model))
  }
  if (!inherits(config, "planscanR_curation_config")) {
    curation_model_abort("{.arg config} must be a planscanR_curation_config.")
  }
  if (!curation_model_same_named_vector(model$topics, config$topics)) {
    curation_model_abort(
      "The model topics do not match {.arg config$topics}; retrain or select a compatible model."
    )
  }
  if (!curation_model_same_named_vector(model$labels, config$labels)) {
    curation_model_abort(
      "The model labels do not match {.arg config$labels}; retrain or select a compatible model."
    )
  }
  invisible(model)
}

curation_model_resolve_learner <- function(learner = NULL, learner_key = NULL) {
  if (!is.null(learner) && !is.null(learner_key)) {
    curation_model_abort("Supply only one of {.arg learner} and {.arg learner_key}.")
  }
  if (!is.null(learner)) {
    if (!inherits(learner, "planscanR_selection_learner")) {
      curation_model_abort("{.arg learner} must be a planscanR_selection_learner.")
    }
    return(learner)
  }
  if (is.null(learner_key)) {
    return(selection_learner_logistic())
  }
  if (!is.character(learner_key) || length(learner_key) != 1L || is.na(learner_key) || !nzchar(learner_key)) {
    curation_model_abort("{.arg learner_key} must be one non-empty character string.")
  }
  registry <- selection_learners()
  constructor <- registry[[learner_key]]
  if (is.null(constructor)) {
    curation_model_abort(
      "Unknown {.arg learner_key} {.val {learner_key}}; use curation_selection_learner_choices()."
    )
  }
  constructor()
}

curation_model_normalize_reviews <- function(reviews, config = NULL) {
  if (is.null(reviews)) {
    curation_model_abort("{.arg reviews} must be a review data frame.")
  }
  if (!is.data.frame(reviews)) {
    curation_model_abort("{.arg reviews} must be a review data frame.")
  }
  if (!is.null(config) && !inherits(config, "planscanR_curation_config")) {
    curation_model_abort("{.arg config} must be a planscanR_curation_config.")
  }
  scope <- if (is.null(config)) NULL else config$identity
  out <- tryCatch(
    curation_review_scope_rows(reviews, scope),
    planscanR_error_curation_sampling = function(e) {
      curation_model_abort(conditionMessage(e))
    }
  )
  required <- c("country", "document_id", "decision", "source", "reviewed_at", "reviewer_id")
  missing <- setdiff(required, names(out))
  if (length(missing) > 0L) {
    curation_model_abort(
      paste0("{.arg reviews} is missing required column", if (length(missing) > 1L) "s" else "", ": ", paste(missing, collapse = ", "), ".")
    )
  }
  # consensus_reviews() predates the canonical reviewer_id repository field;
  # provide its legacy spelling without changing the authored row identity.
  out$reviewer <- as.character(out$reviewer_id)
  out$country <- as.character(out$country)
  out$document_id <- as.character(out$document_id)
  out$decision <- as.character(out$decision)
  out$source <- as.character(out$source)
  out
}

curation_model_training_provenance <- function(
  config,
  records,
  reviews,
  input_provenance = NULL,
  cohort_receipt = NULL
) {
  source_counts <- if ("source" %in% names(reviews)) {
    as.list(table(as.character(reviews$source), useNA = "ifany"))
  } else {
    list()
  }
  list(
    config = list(
      project_id = config$project_id,
      corpus_id = config$corpus_id,
      config_id = config$config_id,
      fingerprint = curation_config_fingerprint(config)
    ),
    review = list(
      fingerprint = curation_artifact_content_fingerprint(reviews),
      rows = nrow(reviews),
      source_counts = source_counts
    ),
    input = input_provenance %||% list(),
    cohort_receipt = cohort_receipt %||% list(),
    record_rows = nrow(records)
  )
}

#' Return the configured learner choices available to a curation UI.
#'
#' The registry and availability check are owned by the selection framework;
#' this adapter only supplies the stable display label used by curation callers.
#'
#' @return A named character vector mapping display labels to learner keys.
#' @export
curation_selection_learner_choices <- function() {
  choices <- c("Logistic regression" = "logistic")
  available <- names(selection_learners(available_only = TRUE))
  choices[choices %in% available]
}

#' Resolve one configured learner from the existing learner registry.
#'
#' @param learner_key A key returned by [selection_learners()].
#' @return A `planscanR_selection_learner`.
#' @export
curation_selection_learner <- function(learner_key = "logistic") {
  curation_model_resolve_learner(learner_key = learner_key)
}

#' Train a curation selection model using explicit configuration.
#'
#' `eval_source = "random"` keeps model fitting and out-of-fold evaluation on
#' the unbiased random-review sample. The existing trainer performs unanimous
#' reviewer consensus and retains the established `planscanR_selection_model`
#' fields and class.
#'
#' @param records A scored and classified record data frame.
#' @param reviews Review rows with country, document_id, reviewer_id (or
#'   reviewer), decision, source, and reviewed_at.
#' @param config A `planscanR_curation_config` supplying topics and labels.
#' @param learner Optional existing `planscanR_selection_learner`.
#' @param learner_key Optional key from [selection_learners()].
#' @param eval_source Review source(s) used for training and evaluation;
#'   defaults to `"random"`, or `NULL` for all sources.
#' @param include Optional extra feature columns accepted by
#'   [selection_features()].
#' @param v,repeats Cross-validation folds and repeats.
#' @param threshold Probability cutoff stored on the model.
#' @param seed Optional seed passed to the existing trainer.
#' @param input_provenance Optional identity for the explicitly selected
#'   scored input snapshot.
#' @param cohort_receipt Optional immutable historical-cohort receipt identity.
#' @return A `planscanR_selection_model`.
#' @export
curation_train_model <- function(
  records,
  reviews,
  config,
  learner = NULL,
  learner_key = NULL,
  eval_source = "random",
  include = character(0),
  v = 5L,
  repeats = 1L,
  threshold = 0.5,
  seed = NULL,
  input_provenance = NULL,
  cohort_receipt = NULL
) {
  curation_check_config(config)
  learner <- curation_model_resolve_learner(learner, learner_key)
  reviews <- curation_model_normalize_reviews(reviews, config)
  training_provenance <- curation_model_training_provenance(
    config = config,
    records = records,
    reviews = reviews,
    input_provenance = input_provenance,
    cohort_receipt = cohort_receipt
  )
  train_selection_model(
    records = records,
    reviews = reviews,
    topics = config$topics,
    labels = config$labels,
    learner = learner,
    eval_source = eval_source,
    include = include,
    v = v,
    repeats = repeats,
    threshold = threshold,
    seed = seed,
    training_provenance = training_provenance,
    strict = TRUE
  )
}

#' Predict with a curation selection model after validating configuration.
#'
#' @param model A `planscanR_selection_model`.
#' @param records A scored and classified record data frame.
#' @param config Optional configuration whose topic and label vectors must
#'   match the model.
#' @param threshold Optional probability cutoff.
#' @return `records` with `select_prob` and `selected_model` columns.
#' @export
curation_predict_model <- function(model, records, config = NULL, threshold = NULL) {
  curation_model_check_config(model, config)
  if (nrow(records) == 0L) {
    records$select_prob <- numeric(0)
    records$selected_model <- logical(0)
    return(records)
  }

  feature_names <- model$features
  if (is.null(feature_names) || length(feature_names) == 0L) {
    feature_contract <- model$feature_contract %||% list()
    feature_names <- feature_contract$feature_names %||% character(0)
  }
  if (length(feature_names) == 0L) {
    feature_names <- selection_feature_names(
      model$topics,
      model$labels,
      include = model$include %||% character(0)
    )
  }
  numeric_features <- feature_names[grepl(
    "^(relevance_score_|class_score_)|^kw_total$",
    feature_names
  )]
  complete <- rep(length(numeric_features) > 0L, nrow(records))
  for (feature in numeric_features) {
    if (!feature %in% names(records) || !is.numeric(records[[feature]])) {
      complete[] <- FALSE
      break
    }
    complete <- complete & is.finite(records[[feature]])
  }

  records$select_prob <- rep(NA_real_, nrow(records))
  records$selected_model <- rep(FALSE, nrow(records))
  if (!any(complete)) {
    return(records)
  }
  predicted <- predict_selection(
    model,
    records[complete, , drop = FALSE],
    threshold = threshold
  )
  records$select_prob[complete] <- predicted$select_prob
  records$selected_model[complete] <- predicted$selected_model
  records
}

#' Save a curation model through the existing selection-model serializer.
#'
#' @param model A `planscanR_selection_model`.
#' @param path RDS destination.
#' @param config Optional compatible curation configuration.
#' @return `path`, invisibly.
#' @export
curation_save_model <- function(model, path, config = NULL) {
  curation_model_check_config(model, config)
  save_selection_model(model, path)
}

#' Load a legacy-compatible curation model artifact.
#'
#' @param path RDS source; a missing path returns `NULL`.
#' @param config Optional compatible curation configuration.
#' @return A `planscanR_selection_model`, or `NULL` when `path` is absent.
#' @export
curation_load_model <- function(path, config = NULL) {
  model <- load_selection_model(path)
  if (is.null(model)) {
    return(NULL)
  }
  curation_model_check_config(model, config)
  model
}

#' Compute stored out-of-fold model metrics through the existing API.
#'
#' @param model A `planscanR_selection_model`.
#' @param config Optional compatible curation configuration.
#' @param threshold Optional probability cutoff.
#' @param by_country If `TRUE`, include an `all` row and country rows.
#' @return A metrics tibble, or `NULL` when no out-of-fold data is stored.
#' @export
curation_model_metrics <- function(model, config = NULL, threshold = NULL, by_country = FALSE) {
  curation_model_check_config(model, config)
  selection_cv_metrics(model, threshold = threshold, by_country = by_country)
}

#' Compute a held-out learning curve with explicit curation configuration.
#'
#' The fixed held-out split and learner implementation remain in
#' [selection_learning_curve()].
#'
#' @param records A scored and classified record data frame.
#' @param reviews Review rows accepted by [curation_train_model()].
#' @param config A `planscanR_curation_config`.
#' @param learner Optional existing learner object.
#' @param learner_key Optional learner-registry key.
#' @param sizes Optional training sizes.
#' @param test_frac Fraction held out per repeat.
#' @param repeats Number of held-out repeats.
#' @param eval_source Review source(s), defaulting to `"random"`.
#' @param threshold Probability cutoff.
#' @param seed Optional seed.
#' @param by_country If `TRUE`, include held-out country slices.
#' @return The long learning-curve tibble from [selection_learning_curve()].
#' @export
curation_learning_curve <- function(
  records,
  reviews,
  config,
  learner = NULL,
  learner_key = NULL,
  sizes = NULL,
  test_frac = 0.25,
  repeats = 10L,
  eval_source = "random",
  threshold = 0.5,
  seed = NULL,
  by_country = FALSE
) {
  curation_check_config(config)
  learner <- curation_model_resolve_learner(learner, learner_key)
  reviews <- curation_model_normalize_reviews(reviews, config)
  selection_learning_curve(
    records = records,
    reviews = reviews,
    topics = config$topics,
    labels = config$labels,
    learner = learner,
    sizes = sizes,
    test_frac = test_frac,
    repeats = repeats,
    eval_source = eval_source,
    threshold = threshold,
    seed = seed,
    by_country = by_country
  )
}

#' @rdname curation_learning_curve
#' @export
curation_compute_learning_curve <- curation_learning_curve

#' Write and atomically publish a model through an injected C3 artifact store.
#'
#' The write callback must return a model identifier (unless `model_id` is
#' supplied); publication is attempted only after a successful write. Callback
#' errors and explicit `ok = FALSE` results propagate to the caller.
#'
#' @param store A `planscanR_curation_artifact_store`.
#' @param scope A `planscanR_curation_scope`.
#' @param model A `planscanR_selection_model`.
#' @param provenance Training and configuration provenance.
#' @param actor A trusted `planscanR_curation_actor`.
#' @param expected_revision Expected immutable-model revision, default `0`.
#' @param expected_active_revision Expected active-reference revision, default
#'   `0`.
#' @param model_id Optional model identifier when the write callback does not
#'   return one.
#' @param config Optional compatible curation configuration.
#' @return The result returned by the publication callback.
#' @export
curation_publish_model <- function(
  store,
  scope,
  model,
  provenance,
  actor,
  expected_revision = 0L,
  expected_active_revision = 0L,
  model_id = NULL,
  config = NULL
) {
  curation_model_check_config(model, config)
  written <- curation_artifact_write_model(
    store = store,
    scope = scope,
    model = model,
    provenance = provenance,
    actor = actor,
    expected_revision = expected_revision
  )
  if (is.list(written) && "ok" %in% names(written) && isFALSE(written$ok)) {
    curation_model_abort("The curation model artifact write was rejected.")
  }
  resolved_id <- model_id %||% if (is.list(written)) written$model_id else NULL
  if (is.null(resolved_id)) {
    curation_model_abort("The curation model write result must include model_id.")
  }
  published <- curation_artifact_publish_model(
    store = store,
    scope = scope,
    model_id = resolved_id,
    actor = actor,
    expected_active_revision = expected_active_revision
  )
  if (is.list(published) && "ok" %in% names(published) && isFALSE(published$ok)) {
    curation_model_abort("The curation model publication was rejected.")
  }
  published
}
