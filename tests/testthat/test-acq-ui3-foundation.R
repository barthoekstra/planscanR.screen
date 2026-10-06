# ACQ-UI3: confirmed historical random labels must enter the existing learner
# through one consensus frame, with bounded exclusion accounting.

acq_ui3_config <- function(root = withr::local_tempdir()) {
  curation_config(
    project_id = "acq-ui3-project",
    corpus_id = "acq-ui3-corpus",
    config_id = "acq-ui3.v1",
    topics = c(topic = "topic"),
    labels = c(keep = "keep", drop = "drop"),
    lexicon = list(topic = "topic"),
    select_fn = function(records, controls) records,
    selection_controls = list(),
    funnel_stages = list(indexed = list(
      key = "indexed",
      label = "Indexed",
      fn = function(records, context) rep(TRUE, nrow(records))
    )),
    branding = list(title = "ACQ-UI3 fixture", primary = "#123456"),
    artifact_paths = list(root = root),
    translator = NULL
  )
}

acq_ui3_records <- function() {
  tibble::tibble(
    country = rep(c("nl", "de"), 5L),
    document_id = paste0("ui3-record-", seq_len(10L)),
    relevance_score_topic = c(.9, .1, .8, .2, .85, .15, .75, .25, .7, .3),
    class_score_keep = c(.9, .1, .8, .2, .85, .15, .75, .25, .7, .3),
    class_score_drop = c(.1, .9, .2, .8, .15, .85, .25, .75, .3, .7),
    kw_total = c(3L, 0L, 2L, 0L, 3L, 0L, 2L, 0L, 1L, 0L)
  )
}

test_that("historical random labels fit through consensus with bounded accounting", {
  for (pkg in c("parsnip", "recipes", "rsample", "workflows")) {
    skip_if_not_installed(pkg)
  }
  records <- acq_ui3_records()
  reviews <- tibble::tibble(
    country = c("nl", "nl", "nl", "nl", "de", "de", "nl", "de", "nl", "de", "xx"),
    document_id = c(
      "ui3-record-1", "ui3-record-1", "ui3-record-3", "ui3-record-3",
      "ui3-record-4", "ui3-record-6", "ui3-record-5", "ui3-record-6",
      "ui3-record-7", "ui3-record-8", "missing"
    ),
    reviewer_id = c("a", "b", "a", "b", "b", "a", "a", "a", "a", "a", "a"),
    decision = c("keep", "keep", "drop", "keep", "drop", "drop", "unsure", "keep", "keep", "drop", "keep"),
    source = c("random", "random", "random", "random", "random", "browse", "random", "random", "random", "random", "random"),
    original_source = c("browse", "browse", rep("random", 9L)),
    reviewed_at = as.POSIXct("2026-10-01", tz = "UTC") + seq_along(document_id)
  )
  # Explicitly unresolved historical linkage removes the whole project from
  # the training consensus even when another reviewer supplied a verdict.
  reviews$historical_linkage <- "unverified"
  reviews$current_linkage <- "current_unique"
  reviews$mutation_allowed <- TRUE
  reviews$linkage_status <- "current_unique"
  reviews$linkage_status[reviews$document_id == "ui3-record-6"] <- "ambiguous"

  model <- curation_train_model(
    records,
    reviews,
    acq_ui3_config(),
    eval_source = "random",
    v = 2L,
    seed = 42L,
    input_provenance = list(snapshot_id = "snapshot-ui3", version = "verified-v1"),
    cohort_receipt = list(id = "receipt-ui3", source_checksum = "sha256:fixture")
  )

  expect_s3_class(model, "planscanR_selection_model")
  expect_true(model$training_audit$consensus_before_folds)
  expect_equal(model$training_audit$source$corrected, 2L)
  expect_equal(model$training_audit$exclusions$unsure, 1L)
  expect_equal(model$training_audit$exclusions$disagreement, 1L)
  expect_equal(model$training_audit$exclusions$unresolved, 1L)
  expect_equal(model$training_audit$exclusions$unmatched, 1L)
  expect_equal(model$training_audit$eligible$keep + model$training_audit$eligible$drop, model$n_train)
  expect_equal(model$training_audit$eligible$features, model$n_train)
  expect_setequal(model$cv_by_country$country, c("all", "de", "nl"))
  expect_identical(model$training_provenance$cohort_receipt$id, "receipt-ui3")
  expect_identical(model$training_provenance$input$snapshot_id, "snapshot-ui3")
})

test_that("training rejects an incompatible scored snapshot instead of manufacturing features", {
  for (pkg in c("parsnip", "recipes", "rsample", "workflows")) {
    skip_if_not_installed(pkg)
  }
  records <- acq_ui3_records()
  records$class_score_drop <- NULL
  reviews <- tibble::tibble(
    country = records$country,
    document_id = records$document_id,
    reviewer_id = "a",
    decision = rep(c("keep", "drop"), 5L),
    source = "random",
    reviewed_at = Sys.time()
  )
  expect_error(
    curation_train_model(records, reviews, acq_ui3_config(), v = 2L),
    class = "planscanR_error_bad_input",
    regexp = "incompatible|missing.*class_score_drop"
  )
})

test_that("provenance pins receipt, review, config, and selected input identities", {
  config <- acq_ui3_config()
  records <- acq_ui3_records()
  actor <- curation_actor("trainer-ui3", roles = "reviewer")
  provenance <- curation_app_model_provenance(
    scope = config$identity,
    config = config,
    records = records,
    reviews = data.frame(
      country = records$country,
      document_id = records$document_id,
      reviewer_id = actor$id,
      decision = rep(c("keep", "drop"), 5L),
      source = "random",
      stringsAsFactors = FALSE
    ),
    actor = actor,
    snapshot_version = "verified-v1",
    snapshot_id = "snapshot-ui3",
    learner_key = "logistic",
    eval_source = "random",
    threshold = 0.5,
    input_provenance = list(snapshot_id = "snapshot-ui3", checksum = "sha256:fixture"),
    cohort_receipt = list(id = "receipt-ui3")
  )
  expect_identical(provenance$input$snapshot_id, "snapshot-ui3")
  expect_identical(provenance$cohort_receipt$id, "receipt-ui3")
  expect_true(is.character(provenance$review$fingerprint))
  expect_identical(provenance$config$config_id, config$config_id)
})

test_that("import receipt linkage is attached to repository reviews before training", {
  reviews <- data.frame(
    country = "nl",
    document_id = "ui3-record-1",
    reviewer_id = "stable-a",
    decision = "keep",
    source = "random",
    import_row_id = "ui3-row-1",
    stringsAsFactors = FALSE
  )
  receipt_rows <- data.frame(
    import_row_id = "ui3-row-1",
    country = "nl",
    document_id = "ui3-record-1",
    original_source = "browse",
    imported_source = "random",
    linkage_status = "current_unique",
    current_linkage = "current_unique",
    historical_linkage = "matched",
    current_match_count = 1L,
    mutation_allowed = TRUE,
    stringsAsFactors = FALSE
  )
  enriched <- curation_app_historical_review_rows(reviews, receipt_rows)
  expect_identical(enriched$original_source, "browse")
  expect_identical(enriched$source, "random")
  expect_identical(enriched$linkage_status, "current_unique")
  expect_true(isTRUE(enriched$historical_mutation_allowed[[1L]]))
})

test_that("explicit scored-input provenance blocks a mismatched provider frame", {
  records <- acq_ui3_records()
  fingerprint <- curation_artifact_content_fingerprint(records)
  expect_true(curation_app_training_input_matches(
    records,
    list(record_fingerprint = fingerprint)
  ))
  changed <- records
  changed$kw_total[[1L]] <- 100L
  expect_false(curation_app_training_input_matches(
    changed,
    list(record_fingerprint = fingerprint)
  ))
})

test_that("verified snapshots preserve scored values while generic enrichment recomputes", {
  config <- acq_ui3_config()
  records <- acq_ui3_records()
  records$title <- rep("fixture", nrow(records))
  records$summary <- rep("", nrow(records))
  records$kw_total[[1L]] <- NA_real_
  read_fn <- function(corpus_id, countries = NULL) {
    if (is.null(countries)) {
      return(records)
    }
    records[records$country %in% countries, , drop = FALSE]
  }
  provider <- curation_records_provider(read_fn, read_fn)
  feature_names <- selection_feature_names(config$topics, config$labels)
  provenance <- list(
    record_fingerprint = curation_artifact_content_fingerprint(records),
    feature_names = feature_names,
    scored_feature_fingerprint = curation_scored_feature_fingerprint(records, feature_names),
    preserve_scored_features = TRUE
  )
  prepared <- curation_records_snapshot(
    provider,
    config,
    preserve_scored_features = TRUE
  )
  metadata <- attr(prepared, "curation_snapshot_metadata", exact = TRUE)
  expect_identical(prepared$kw_total, records$kw_total)
  expect_true(is.na(prepared$kw_total[[1L]]))
  expect_identical(metadata$scored_feature_fingerprint, provenance$scored_feature_fingerprint)
  expect_true(curation_app_training_input_matches(prepared, provenance, metadata))
  changed <- prepared
  changed$kw_total[[1L]] <- 100L
  expect_false(curation_app_training_input_matches(changed, provenance, metadata))

  generic <- curation_records_snapshot(provider, config)
  expect_true(all(generic$kw_total == 0L))

  missing <- records
  missing$kw_total <- NULL
  missing_provider <- curation_records_provider(
    function(corpus_id, countries = NULL) missing,
    function(corpus_id, countries = NULL) missing
  )
  expect_error(
    curation_records_snapshot(missing_provider, config, preserve_scored_features = TRUE),
    class = "planscanR_error_curation_records",
    regexp = "missing configured feature"
  )
  non_numeric <- records
  non_numeric$kw_total <- as.character(non_numeric$kw_total)
  non_numeric_provider <- curation_records_provider(
    function(corpus_id, countries = NULL) non_numeric,
    function(corpus_id, countries = NULL) non_numeric
  )
  expect_error(
    curation_records_snapshot(non_numeric_provider, config, preserve_scored_features = TRUE),
    class = "planscanR_error_curation_records",
    regexp = "non-numeric configured feature"
  )
})
