# ACQ-C1d: thin configuration/model/evaluation adapters.

curation_model_test_config <- function() {
  labels <- c(keep = "keep", drop = "drop", other = "other")
  attr(labels, "relevant") <- "keep"
  curation_config(
    project_id = "project-model",
    corpus_id = "corpus-model",
    config_id = "model.v1",
    topics = c(signal = "signal topic", noise = "noise topic"),
    labels = labels,
    lexicon = list(signal = "signal"),
    select_fn = function(records, controls) {
      records$selected <- records$kw_total > 0L
      records
    },
    selection_controls = list(),
    funnel_stages = list(
      indexed = list(
        key = "indexed",
        label = "Indexed",
        fn = function(records, context) rep(TRUE, nrow(records))
      )
    ),
    branding = list(title = "Model fixture", primary = "#123456"),
    artifact_paths = list(root = "/tmp/model-fixture")
  )
}

curation_model_test_records <- function(n = 60L) {
  i <- seq_len(n)
  keep <- i %% 2L == 0L
  records <- tibble::tibble(
    country = rep(c("nl", "de", "at"), length.out = n),
    document_id = paste0("model-", i),
    title = ifelse(keep, "signal record", "noise record"),
    summary = "synthetic model fixture",
    relevance_score_signal = ifelse(keep, 0.8, 0.1),
    relevance_score_noise = ifelse(keep, 0.1, 0.2),
    class_score_keep = ifelse(keep, 0.8, 0.1),
    class_score_drop = ifelse(keep, 0.1, 0.8),
    class_score_other = 0.05,
    kw_total = ifelse(keep, 2L, 0L)
  )
  records
}

curation_model_test_reviews <- function(records) {
  i <- seq_len(nrow(records))
  tibble::tibble(
    country = records$country,
    document_id = records$document_id,
    decision = ifelse(records$kw_total > 0L, "keep", "drop"),
    source = ifelse(i <= 2L, "browse", "random"),
    reviewer_id = "reviewer-1",
    reviewed_at = "2026-09-01T10:00:00Z"
  )
}

curation_model_test_artifact_store <- function(calls, write_fn, publish_fn) {
  curation_artifact_store(
    paths = list(root = "/tmp/model-fixture"),
    snapshot_read_fn = function(scope, version, actor) NULL,
    snapshot_write_fn = function(scope, snapshot, actor, expected_revision) {
      list(ok = TRUE, revision = expected_revision + 1L)
    },
    sample_list_fn = function(scope, reviewer_id, actor) list(),
    sample_read_fn = function(scope, sample_id, actor) NULL,
    sample_write_fn = function(scope, sample, reviewer_id, actor, expected_revision) {
      list(ok = TRUE, revision = expected_revision + 1L)
    },
    model_list_fn = function(scope, actor) list(),
    model_read_fn = function(scope, model_id, actor) NULL,
    model_write_fn = write_fn,
    model_publish_fn = publish_fn,
    active_model_fn = function(scope, actor) NULL
  )
}

test_that("curation model adapter passes config and random source to existing trainer", {
  for (pkg in c("parsnip", "recipes", "rsample", "workflows")) {
    skip_if_not_installed(pkg)
  }
  config <- curation_model_test_config()
  records <- curation_model_test_records()
  reviews <- curation_model_test_reviews(records)

  model <- curation_train_model(
    records,
    reviews,
    config,
    eval_source = "random",
    v = 3L,
    seed = 42L
  )

  expect_s3_class(model, "planscanR_selection_model")
  expect_identical(model$topics, config$topics)
  expect_identical(model$labels, config$labels)
  expect_identical(model$eval_source, "random")
  expect_equal(model$n_train, sum(reviews$source == "random"))
  expect_equal(model$cv$n_reviewed, model$n_train)

  before <- predict_selection(model, records)
  after <- curation_predict_model(model, records, config = config)
  expect_equal(after$select_prob, before$select_prob, tolerance = 1e-12)
  expect_identical(after$selected_model, before$selected_model)
})

test_that("curation evaluation preserves consensus and random-only metrics", {
  config <- curation_model_test_config()
  records <- curation_model_test_records(6L)
  records$selected <- c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE)
  reviews <- tibble::tibble(
    country = c("nl", "nl", "de", "de", "at", "at", "at"),
    document_id = c("model-1", "model-1", "model-2", "model-2", "model-3", "model-3", "model-3"),
    decision = c("keep", "keep", "drop", "drop", "keep", "drop", "keep"),
    source = c("random", "random", "random", "browse", "random", "random", "random"),
    reviewer_id = c("a", "b", "a", "b", "a", "b", "c"),
    reviewed_at = "2026-09-01T10:00:00Z"
  )

  overall <- curation_selection_vs_human(records, reviews, eval_source = "random")
  expect_equal(overall[, c("n_reviewed", "tp", "fp", "fn", "tn")], tibble::tibble(
    n_reviewed = 2L, tp = 1L, fp = 0L, fn = 0L, tn = 1L
  ))
  by_country <- curation_selection_vs_human(
    records,
    reviews,
    eval_source = "random",
    by_country = TRUE
  )
  expect_equal(by_country$country, c("all", "de", "nl"))
  expect_false("at" %in% by_country$country)

  agreement <- curation_inter_reviewer_summary(reviews)
  expect_equal(agreement$n_multi, 3L)
  expect_equal(agreement$n_agree, 2L)
})

test_that("legacy-format model artifacts load and preserve predictions", {
  for (pkg in c("parsnip", "recipes", "rsample", "workflows")) {
    skip_if_not_installed(pkg)
  }
  config <- curation_model_test_config()
  records <- curation_model_test_records()
  reviews <- curation_model_test_reviews(records)
  # This is a real legacy-format artifact: it is serialized by the existing
  # selection trainer and loaded without BIOGAIN or a model-runtime seam.
  legacy <- train_selection_model(
    records,
    reviews,
    topics = config$topics,
    labels = config$labels,
    eval_source = "random",
    v = 3L,
    seed = 42L
  )
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(legacy, path)
  restored <- curation_load_model(path, config = config)
  expect_s3_class(restored, "planscanR_selection_model")
  expect_identical(restored$topics, config$topics)
  expect_identical(restored$labels, config$labels)
  before <- predict_selection(legacy, records)
  after <- curation_predict_model(restored, records, config = config)
  expect_equal(after$select_prob, before$select_prob, tolerance = 1e-12)
  expect_identical(after$selected_model, before$selected_model)
  expect_null(curation_load_model(withr::local_tempfile(fileext = ".rds")))
})

test_that("model publication delegates writes and active publication, including failures", {
  scope <- curation_scope("project-model", "corpus-model", "model.v1")
  actor <- curation_actor("reviewer-1", roles = "reviewer")
  calls <- new.env(parent = emptyenv())
  calls$published <- FALSE
  model <- structure(list(), class = "planscanR_selection_model")

  store <- curation_model_test_artifact_store(
    calls,
    write_fn = function(scope, model, provenance, actor, expected_revision) {
      calls$written <- list(scope = scope, actor = actor, provenance = provenance)
      list(ok = TRUE, model_id = "model-1", revision = expected_revision + 1L)
    },
    publish_fn = function(scope, model_id, actor, expected_active_revision) {
      calls$published <- TRUE
      list(ok = TRUE, active_model_id = model_id, active_revision = expected_active_revision + 1L)
    }
  )
  result <- curation_publish_model(
    store,
    scope,
    model,
    provenance = list(config_id = "model.v1"),
    actor = actor,
    expected_revision = 0L,
    expected_active_revision = 0L
  )
  expect_true(calls$published)
  expect_identical(result$active_model_id, "model-1")

  failing_write <- curation_model_test_artifact_store(
    calls,
    write_fn = function(scope, model, provenance, actor, expected_revision) stop("write fixture failure"),
    publish_fn = function(scope, model_id, actor, expected_active_revision) stop("must not publish")
  )
  expect_error(
    curation_publish_model(failing_write, scope, model, list(), actor, expected_revision = 0L),
    "write fixture failure"
  )

  failing_publish <- curation_model_test_artifact_store(
    calls,
    write_fn = function(scope, model, provenance, actor, expected_revision) {
      list(ok = TRUE, model_id = "model-2", revision = expected_revision + 1L)
    },
    publish_fn = function(scope, model_id, actor, expected_active_revision) stop("publish fixture failure")
  )
  expect_error(
    curation_publish_model(failing_publish, scope, model, list(), actor, expected_revision = 0L),
    "publish fixture failure"
  )
})

test_that("model adapter rejects a mismatched configuration", {
  model <- structure(
    list(topics = c(a = "a"), labels = c(keep = "keep", drop = "drop")),
    class = "planscanR_selection_model"
  )
  expect_error(
    curation_predict_model(model, tibble::tibble(), config = curation_model_test_config()),
    class = "planscanR_error_curation_model"
  )
})
