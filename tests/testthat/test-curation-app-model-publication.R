# ACQ-C4 model publication integration.
#
# These tests drive the real Shiny app handler against the real SQLite review
# and C3 artifact stores. Records and reviews are synthetic; the trainer mock
# returns a deterministic prediction-capable selection-model fixture, so no
# Python or network dependency is involved.

acq_c4_model_publication_skip <- function() {
  for (package in c("shiny", "bslib", "reactable", "plotly", "htmltools", "DBI", "RSQLite", "parsnip", "recipes", "rsample", "workflows")) {
    skip_if_not_installed(package)
  }
  registerS3method(
    "predict",
    "acq_c4_fixture_workflow",
    predict.acq_c4_fixture_workflow,
    envir = asNamespace("stats")
  )
}

acq_c4_model_publication_records <- function() {
  tibble::tibble(
    country = rep(c("nl", "de"), 4L),
    document_id = paste0("model-record-", seq_len(8L)),
    title = paste("Synthetic model record", seq_len(8L)),
    summary = paste("Synthetic topic records for model publication", seq_len(8L)),
    relevance_score_topic = c(.95, .08, .88, .12, .91, .1, .84, .15),
    class_score_keep = c(.9, .1, .85, .15, .88, .12, .82, .18),
    class_score_drop = c(.1, .9, .15, .85, .12, .88, .18, .82),
    kw_total = c(4, 0, 3, 0, 4, 0, 3, 0)
  )
}

acq_c4_model_publication_config <- function(root) {
  curation_config(
    project_id = "acq-c4-model-project",
    corpus_id = "acq-c4-model-corpus",
    config_id = "acq-c4-model.v1",
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
    branding = list(title = "ACQ-C4 model fixture", primary = "#123456"),
    artifact_paths = list(root = file.path(root, "config-artifacts")),
    translator = NULL
  )
}

acq_c4_model_publication_reviews <- function(records, actor) {
  data.frame(
    country = records$country,
    document_id = records$document_id,
    reviewer_id = actor$id,
    decision = rep(c("keep", "drop"), 4L),
    source = rep("random", nrow(records)),
    note = NA_character_,
    reviewed_at = as.POSIXct(
      "2026-10-02 00:00:00",
      tz = "UTC"
    ) + seq_len(nrow(records)),
    stringsAsFactors = FALSE
  )
}

acq_c4_model_publication_provider <- function(records) {
  curation_records_provider(
    read_fn = function(corpus_id, countries) {
      if (is.null(countries)) {
        return(records)
      }
      records[records$country %in% countries, , drop = FALSE]
    },
    refresh_fn = function(corpus_id, countries) {
      if (is.null(countries)) {
        return(records)
      }
      records[records$country %in% countries, , drop = FALSE]
    }
  )
}

acq_c4_model_publication_write_reviews <- function(repository, scope, reviews, actor) {
  for (i in seq_len(nrow(reviews))) {
    curation_review_upsert(
      repository,
      scope,
      reviews[i, , drop = FALSE],
      actor,
      expected_revision = 0L
    )
  }
  invisible(TRUE)
}

acq_c4_model_publication_context <- function(config, provider, repository, store, actor) {
  list(
    config = config,
    records_provider = provider,
    review_repository = repository,
    artifact_store = store,
    actor_resolver = curation_actor_resolver(function(request) actor),
    request = NULL
  )
}

acq_c4_model_publication_pulse <- function(session, input_id, value) {
  do.call(session$setInputs, stats::setNames(list(value), input_id))
  session$flushReact()
  invisible(value)
}

acq_c4_model_publication_session <- function() {
  session <- shiny::MockShinySession$new()
  session$setInputs(train_model = 0L)
  session
}

predict.acq_c4_fixture_workflow <- function(object, new_data, type = NULL, ...) {
  if (!identical(type, "prob")) {
    stop("the ACQ-C4 fixture workflow only predicts class probabilities")
  }
  keep <- as.numeric(new_data$class_score_keep)
  tibble::tibble(
    .pred_keep = keep,
    .pred_drop = 1 - keep
  )
}

acq_c4_model_publication_train <- function(records, reviews, config, learner_name = NULL) {
  decisions <- as.character(reviews$decision)
  structure(
    list(
      workflow = structure(list(), class = "acq_c4_fixture_workflow"),
      learner_name = learner_name %||% "logistic_glm",
      features = c(
        "relevance_score_topic", "class_score_keep", "class_score_drop", "kw_total"
      ),
      topics = config$topics,
      labels = config$labels,
      include = character(0),
      n_train = nrow(reviews),
      n_keep = sum(decisions == "keep"),
      n_by_country = as.list(table(reviews$country)),
      eval_source = "random",
      threshold = 0.5,
      v = 2L,
      repeats = 1L,
      trained_at = as.POSIXct("2026-10-02 00:00:00", tz = "UTC"),
      oof = NULL,
      cv = NULL
    ),
    class = "planscanR_selection_model"
  )
}

test_that("the actual app trains, writes, publishes, serves, and reloads a model", {
  acq_c4_model_publication_skip()
  root <- withr::local_tempdir()
  review_db <- file.path(root, "reviews.sqlite")
  repository <- curation_review_repository_sqlite(review_db)
  store <- curation_artifact_store_sqlite(list(root = file.path(root, "artifacts")))
  on.exit(repository$close(), add = TRUE)
  on.exit(store$close(), add = TRUE)

  actor <- curation_actor("alice", roles = "reviewer", display_name = "Alice")
  config <- acq_c4_model_publication_config(root)
  scope <- config$identity
  records <- acq_c4_model_publication_records()
  reviews <- acq_c4_model_publication_reviews(records, actor)
  acq_c4_model_publication_write_reviews(repository, scope, reviews, actor)
  provider <- acq_c4_model_publication_provider(records)
  context <- acq_c4_model_publication_context(config, provider, repository, store, actor)
  train_calls <- 0L

  testthat::local_mocked_bindings(
    curation_train_model = function(records, reviews, config, ...) {
      train_calls <<- train_calls + 1L
      acq_c4_model_publication_train(records, reviews, config)
    },
    .package = "planscanR.screen"
  )

  shiny::testServer(
    function(input, output, session) {
      curation_app_server(input, output, session, context)
    },
    {
      acq_c4_model_publication_pulse(session, "train_model", 1L)
      expect_identical(train_calls, 1L)
      expect_true(any(grepl("logistic_glm", as.character(output$model_info), fixed = TRUE)))
    },
    session = acq_c4_model_publication_session()
  )

  active <- curation_artifact_active_model(store, scope, actor)
  expect_true(is.character(active$active_model_id))
  expect_identical(active$active_revision, 1L)
  persisted <- curation_artifact_read_model(store, scope, active$active_model_id, actor)
  expect_true(isTRUE(persisted$ok))
  expect_s3_class(persisted$payload, "planscanR_selection_model")
  expect_identical(persisted$payload$learner_name, "logistic_glm")
  expect_identical(persisted$provenance$creator, actor$id)
  expect_identical(persisted$provenance$project_id, scope$project_id)
  expect_identical(persisted$provenance$corpus_snapshot$version, persisted$provenance$snapshot_version)
  served <- curation_predict_model(persisted$payload, records, config)
  expect_true(all(c("select_prob", "selected_model") %in% names(served)))
  expect_equal(nrow(served), nrow(records))

  repository$close()
  store$close()
  repository <- curation_review_repository_sqlite(review_db)
  store <- curation_artifact_store_sqlite(list(root = file.path(root, "artifacts")))
  on.exit(repository$close(), add = TRUE)
  on.exit(store$close(), add = TRUE)
  restarted_context <- acq_c4_model_publication_context(
    config,
    provider,
    repository,
    store,
    actor
  )
  restarted <- curation_app_read_model(store, scope, actor, config)
  expect_s3_class(restarted, "planscanR_selection_model")
  expect_identical(restarted$learner_name, "logistic_glm")
  shiny::testServer(
    function(input, output, session) {
      curation_app_server(input, output, session, restarted_context)
    },
    {
      expect_true(any(grepl("logistic_glm", as.character(output$model_info), fixed = TRUE)))
    },
    session = acq_c4_model_publication_session()
  )
})

test_that("an active-model CAS conflict leaves the old active model and app model", {
  acq_c4_model_publication_skip()
  root <- withr::local_tempdir()
  review_db <- file.path(root, "reviews.sqlite")
  repository <- curation_review_repository_sqlite(review_db)
  store <- curation_artifact_store_sqlite(list(root = file.path(root, "artifacts")))
  on.exit(repository$close(), add = TRUE)
  on.exit(store$close(), add = TRUE)

  actor <- curation_actor("alice", roles = "reviewer", display_name = "Alice")
  config <- acq_c4_model_publication_config(root)
  scope <- config$identity
  records <- acq_c4_model_publication_records()
  reviews <- acq_c4_model_publication_reviews(records, actor)
  acq_c4_model_publication_write_reviews(repository, scope, reviews, actor)
  provider <- acq_c4_model_publication_provider(records)
  first <- acq_c4_model_publication_train(records, reviews, config, learner_name = "first")
  first_provenance <- curation_app_model_provenance(
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
  first_published <- curation_publish_model(
    store,
    scope,
    first,
    provenance = first_provenance,
    actor = actor,
    expected_revision = 0L,
    expected_active_revision = 0L,
    config = config
  )
  expect_true(isTRUE(first_published$ok))
  first_id <- first_published$active_model_id %||% first_published$model_id
  real_active <- get("curation_artifact_active_model", envir = asNamespace("planscanR.screen"))
  train_calls <- 0L
  testthat::local_mocked_bindings(
    curation_train_model = function(records, reviews, config, ...) {
      train_calls <<- train_calls + 1L
      acq_c4_model_publication_train(records, reviews, config, learner_name = "second")
    },
    curation_artifact_active_model = function(store, scope, actor) {
      active <- real_active(store, scope, actor)
      if (!is.null(active)) active$active_revision <- 0L
      active
    },
    .package = "planscanR.screen"
  )
  context <- acq_c4_model_publication_context(config, provider, repository, store, actor)

  shiny::testServer(
    function(input, output, session) {
      curation_app_server(input, output, session, context)
    },
    {
      expect_true(any(grepl("first", as.character(output$model_info), fixed = TRUE)))
      acq_c4_model_publication_pulse(session, "train_model", 1L)
      expect_identical(train_calls, 1L)
      expect_true(any(grepl("first", as.character(output$model_info), fixed = TRUE)))
    },
    session = acq_c4_model_publication_session()
  )

  active <- real_active(store, scope, actor)
  expect_identical(active$active_model_id, first_id)
  expect_identical(active$active_revision, 1L)
  current <- curation_artifact_read_model(store, scope, first_id, actor)
  expect_identical(current$payload$learner_name, "first")
  expect_equal(
    nrow(curation_artifact_list_models(store, scope, actor)),
    2L
  )
})
