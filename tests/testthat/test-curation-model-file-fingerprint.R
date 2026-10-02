# ACQ-C4 model-file integrity and fitted-workflow persistence.

acq_c4_model_file_skip <- function() {
  for (package in c(
    "DBI", "RSQLite", "openssl", "parsnip", "recipes", "rsample", "workflows", "pkgload"
  )) {
    skip_if_not_installed(package)
  }
}

acq_c4_model_file_config <- function(root) {
  curation_config(
    project_id = "acq-c4-file-project",
    corpus_id = "acq-c4-file-corpus",
    config_id = "acq-c4-file.v1",
    topics = c(topic = "topic records"),
    labels = c(keep = "keep", drop = "drop"),
    lexicon = list(topic = "topic"),
    select_fn = function(records, controls) {
      records$selected <- rep(TRUE, nrow(records))
      records
    },
    selection_controls = list(),
    funnel_stages = list(indexed = list(
      key = "indexed",
      label = "Indexed",
      fn = function(records, context) rep(TRUE, nrow(records))
    )),
    branding = list(title = "ACQ-C4 file fixture", primary = "#123456"),
    artifact_paths = list(root = file.path(root, "config-artifacts")),
    translator = NULL
  )
}

acq_c4_model_file_records <- function() {
  tibble::tibble(
    country = rep(c("nl", "de"), 4L),
    document_id = paste0("file-record-", seq_len(8L)),
    title = paste("Synthetic file record", seq_len(8L)),
    summary = paste("Synthetic topic records for model-file persistence", seq_len(8L)),
    relevance_score_topic = c(.95, .08, .88, .12, .91, .1, .84, .15),
    class_score_keep = c(.9, .1, .85, .15, .88, .12, .82, .18),
    class_score_drop = c(.1, .9, .15, .85, .12, .88, .18, .82),
    kw_total = c(4, 0, 3, 0, 4, 0, 3, 0)
  )
}

acq_c4_model_file_reviews <- function(records, actor) {
  data.frame(
    country = records$country,
    document_id = records$document_id,
    reviewer_id = actor$id,
    decision = rep(c("keep", "drop"), 4L),
    source = rep("random", nrow(records)),
    note = NA_character_,
    reviewed_at = as.POSIXct("2026-10-02 00:00:00", tz = "UTC") + seq_len(nrow(records)),
    stringsAsFactors = FALSE
  )
}

acq_c4_model_file_provenance <- function(scope, config, records, reviews, actor) {
  curation_app_model_provenance(
    scope = scope,
    config = config,
    records = records,
    reviews = reviews,
    actor = actor,
    snapshot_version = NULL,
    learner_key = "logistic",
    eval_source = "random",
    threshold = 0.5
  )
}

acq_c4_model_file_train <- function(records, reviews, config, seed = 42L) {
  train_selection_model(
    records = records,
    reviews = reviews,
    topics = config$topics,
    labels = config$labels,
    learner = selection_learner_logistic(),
    eval_source = "random",
    v = 2L,
    repeats = 1L,
    threshold = 0.5,
    seed = seed
  )
}

acq_c4_model_file_scope_setup <- function(root) {
  actor <- curation_actor("alice", roles = "reviewer", display_name = "Alice")
  config <- acq_c4_model_file_config(root)
  list(
    actor = actor,
    config = config,
    scope = config$identity,
    records = acq_c4_model_file_records(),
    reviews = acq_c4_model_file_reviews(acq_c4_model_file_records(), actor)
  )
}

test_that("a fitted GLM model file publishes, reloads, and predicts in a fresh process", {
  acq_c4_model_file_skip()
  root <- withr::local_tempdir()
  setup <- acq_c4_model_file_scope_setup(root)
  store <- curation_artifact_store_sqlite(list(root = file.path(root, "artifacts")))
  on.exit(store$close(), add = TRUE)

  model <- acq_c4_model_file_train(
    setup$records,
    setup$reviews,
    setup$config
  )
  provenance <- acq_c4_model_file_provenance(
    setup$scope,
    setup$config,
    setup$records,
    setup$reviews,
    setup$actor
  )
  written <- curation_artifact_write_model(
    store,
    setup$scope,
    model,
    provenance,
    setup$actor,
    expected_revision = 0L
  )
  expect_true(isTRUE(written$ok))
  fingerprint <- DBI::dbGetQuery(
    store$connection,
    "SELECT model_fingerprint FROM curation_artifact_models WHERE model_id = ?",
    params = list(written$model_id)
  )$model_fingerprint[[1L]]
  expect_match(fingerprint, "^rds-file-sha256-v1:[0-9a-f]{64}$")
  published <- curation_artifact_publish_model(
    store,
    setup$scope,
    written$model_id,
    setup$actor,
    expected_active_revision = 0L
  )
  expect_true(isTRUE(published$ok))
  loaded <- curation_artifact_read_model(
    store,
    setup$scope,
    written$model_id,
    setup$actor
  )
  expect_s3_class(loaded$payload, "planscanR_selection_model")
  expect_identical(loaded$payload$learner_name, "logistic_glm")
  expect_identical(loaded$payload$n_train, 8L)
  served <- curation_predict_model(loaded$payload, setup$records, setup$config)
  expect_true(all(c("select_prob", "selected_model") %in% names(served)))
  expect_equal(nrow(served), nrow(setup$records))
  expect_true(all(is.finite(served$select_prob)))
  expect_true(all(served$select_prob >= 0 & served$select_prob <= 1))
  expect_type(served$selected_model, "logical")

  store$close()
  store <- curation_artifact_store_sqlite(list(root = file.path(root, "artifacts")))
  on.exit(store$close(), add = TRUE)
  reopened <- curation_artifact_read_model(
    store,
    setup$scope,
    written$model_id,
    setup$actor
  )
  expect_s3_class(reopened$payload, "planscanR_selection_model")
  expect_equal(
    curation_predict_model(reopened$payload, setup$records, setup$config)$selected_model,
    served$selected_model
  )
  store$close()

  script <- withr::local_tempfile(fileext = ".R")
  output <- withr::local_tempfile(fileext = ".out")
  package_root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "package_root <- args[[1L]]; artifact_root <- args[[2L]]; output <- args[[3L]]",
    "pkgload::load_all(package_root, quiet = TRUE)",
    "store <- curation_artifact_store_sqlite(list(root = artifact_root))",
    "on.exit(store$close(), add = TRUE)",
    "scope <- curation_scope('acq-c4-file-project', 'acq-c4-file-corpus', 'acq-c4-file.v1')",
    "actor <- curation_actor('alice', roles = 'reviewer')",
    "active <- curation_artifact_active_model(store, scope, actor)",
    "loaded <- curation_artifact_read_model(store, scope, active$active_model_id, actor)$payload",
    "records <- tibble::tibble(country = rep(c('nl', 'de'), 4L), document_id = paste0('file-record-', seq_len(8L)), class_score_keep = c(.9, .1, .85, .15, .88, .12, .82, .18), relevance_score_topic = c(.95, .08, .88, .12, .91, .1, .84, .15), class_score_drop = c(.1, .9, .15, .85, .12, .88, .18, .82), kw_total = c(4, 0, 3, 0, 4, 0, 3, 0))",
    "served <- curation_predict_model(loaded, records)",
    "writeLines(if (all(c('select_prob', 'selected_model') %in% names(served))) 'ok' else 'bad', output)"
  ), script)
  result <- system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", script, package_root, file.path(root, "artifacts"), output),
    stdout = FALSE,
    stderr = FALSE
  )
  expect_identical(result, 0L)
  expect_identical(readLines(output, warn = FALSE), "ok")
})

test_that("file tampering fails before publication and preserves the old active model", {
  acq_c4_model_file_skip()
  root <- withr::local_tempdir()
  setup <- acq_c4_model_file_scope_setup(root)
  store <- curation_artifact_store_sqlite(list(root = file.path(root, "artifacts")))
  on.exit(store$close(), add = TRUE)
  model <- acq_c4_model_file_train(setup$records, setup$reviews, setup$config)
  provenance <- acq_c4_model_file_provenance(
    setup$scope,
    setup$config,
    setup$records,
    setup$reviews,
    setup$actor
  )
  first <- curation_artifact_write_model(
    store, setup$scope, model, provenance, setup$actor,
    expected_revision = 0L
  )
  curation_artifact_publish_model(
    store, setup$scope, first$model_id, setup$actor,
    expected_active_revision = 0L
  )
  second_model <- model
  second_model$learner_name <- "second"
  second <- curation_artifact_write_model(
    store,
    setup$scope,
    second_model,
    provenance,
    setup$actor,
    expected_revision = 0L
  )
  second_row <- DBI::dbGetQuery(
    store$connection,
    "SELECT path FROM curation_artifact_models WHERE model_id = ?",
    params = list(second$model_id)
  )
  original <- readBin(second_row$path[[1L]], what = "raw", n = file.info(second_row$path[[1L]])$size)
  saveRDS(structure(second_model, tamper = TRUE), second_row$path[[1L]])
  expect_error(
    curation_artifact_read_model(store, setup$scope, second$model_id, setup$actor),
    "file fingerprint"
  )
  expect_error(
    curation_artifact_publish_model(
      store, setup$scope, second$model_id, setup$actor,
      expected_active_revision = 1L
    ),
    "file fingerprint"
  )
  expect_identical(
    curation_artifact_active_model(store, setup$scope, setup$actor)$active_model_id,
    first$model_id
  )

  writeBin(original, second_row$path[[1L]])
  corrupted <- original
  corrupted[[1L]] <- as.raw(bitwXor(as.integer(corrupted[[1L]]), 1L))
  writeBin(corrupted, second_row$path[[1L]])
  expect_error(
    curation_artifact_read_model(store, setup$scope, second$model_id, setup$actor),
    "file fingerprint"
  )
  expect_error(
    curation_artifact_publish_model(
      store, setup$scope, second$model_id, setup$actor,
      expected_active_revision = 1L
    ),
    "file fingerprint"
  )
  active <- curation_artifact_active_model(store, setup$scope, setup$actor)
  expect_identical(active$active_model_id, first$model_id)
  expect_identical(active$active_revision, 1L)
  expect_identical(
    curation_artifact_read_model(store, setup$scope, first$model_id, setup$actor)$payload$learner_name,
    "logistic_glm"
  )
})

test_that("legacy fingerprints remain readable and tagged metadata fails closed", {
  acq_c4_model_file_skip()
  root <- withr::local_tempdir()
  setup <- acq_c4_model_file_scope_setup(root)
  store <- curation_artifact_store_sqlite(list(root = file.path(root, "artifacts")))
  on.exit(store$close(), add = TRUE)
  model <- structure(
    list(
      workflow = NULL,
      learner_name = "legacy",
      topics = setup$config$topics,
      labels = setup$config$labels,
      fixture_tag = "legacy"
    ),
    class = "planscanR_selection_model"
  )
  provenance <- acq_c4_model_file_provenance(
    setup$scope,
    setup$config,
    setup$records,
    setup$reviews,
    setup$actor
  )
  written <- curation_artifact_write_model(
    store, setup$scope, model, provenance, setup$actor,
    expected_revision = 0L
  )
  legacy_fingerprint <- curation_artifact_content_fingerprint(model)
  DBI::dbExecute(
    store$connection,
    "UPDATE curation_artifact_models SET model_fingerprint = ? WHERE model_id = ?",
    params = list(legacy_fingerprint, written$model_id)
  )
  before <- DBI::dbGetQuery(
    store$connection,
    "SELECT * FROM curation_artifact_models WHERE model_id = ?",
    params = list(written$model_id)
  )
  expect_s3_class(
    curation_artifact_read_model(store, setup$scope, written$model_id, setup$actor)$payload,
    "planscanR_selection_model"
  )
  saveRDS(structure(model, fixture_tag = "tampered"), before$path[[1L]])
  expect_error(
    curation_artifact_read_model(store, setup$scope, written$model_id, setup$actor),
    "fingerprint does not match its immutable metadata"
  )
  after_tamper <- DBI::dbGetQuery(
    store$connection,
    "SELECT * FROM curation_artifact_models WHERE model_id = ?",
    params = list(written$model_id)
  )
  expect_identical(after_tamper, before)

  DBI::dbExecute(
    store$connection,
    "UPDATE curation_artifact_models SET model_fingerprint = ? WHERE model_id = ?",
    params = list(
      paste0("rds-file-sha512-v1:", strrep("0", 64L)),
      written$model_id
    )
  )
  unknown <- DBI::dbGetQuery(
    store$connection,
    "SELECT * FROM curation_artifact_models WHERE model_id = ?",
    params = list(written$model_id)
  )
  writeLines("not an RDS", before$path[[1L]])
  expect_error(
    curation_artifact_read_model(store, setup$scope, written$model_id, setup$actor),
    "unknown tag"
  )
  expect_identical(
    DBI::dbGetQuery(
      store$connection,
      "SELECT * FROM curation_artifact_models WHERE model_id = ?",
      params = list(written$model_id)
    ),
    unknown
  )

  DBI::dbExecute(
    store$connection,
    "UPDATE curation_artifact_models SET model_fingerprint = ? WHERE model_id = ?",
    params = list("rds-file-sha256-v1:not-a-digest", written$model_id)
  )
  malformed <- DBI::dbGetQuery(
    store$connection,
    "SELECT * FROM curation_artifact_models WHERE model_id = ?",
    params = list(written$model_id)
  )
  expect_error(
    curation_artifact_read_model(store, setup$scope, written$model_id, setup$actor),
    "malformed"
  )
  expect_identical(
    DBI::dbGetQuery(
      store$connection,
      "SELECT * FROM curation_artifact_models WHERE model_id = ?",
      params = list(written$model_id)
    ),
    malformed
  )
})
