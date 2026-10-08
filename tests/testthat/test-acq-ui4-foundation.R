# ACQ-UI4: immutable model/campaign preview and authenticated submission.

acq_ui4_catalog <- function(root = tempfile("acq-ui4-catalog-")) {
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  artifact <- file.path(root, "foundation-model.rds")
  saveRDS(list(model = "fixture"), artifact)
  catalog <- acquisition_catalog_open(file.path(root, "acquisition.sqlite"))
  model <- acquisition_catalog_register_model(
    catalog,
    model_id = "model-ui4-foundation",
    artifact_path = artifact,
    config_id = "biogain.v1",
    input_snapshot_id = "snapshot-ui4",
    metadata = list(scope = "fixture"),
    created_by = "trainer-ui4"
  )
  source_a <- acquisition_catalog_register_source(
    catalog, "nl", "portal-a", "project-1",
    source_url = "https://portal-a/project-1"
  )
  source_b <- acquisition_catalog_register_source(
    catalog, "de", "portal-b", "project-2",
    source_url = "https://portal-b/project-2"
  )
  list(
    catalog = catalog,
    model = model,
    sources = list(source_a, source_b)
  )
}

acq_ui4_selection <- function(sources) {
  data.frame(
    source_ref_id = vapply(sources, `[[`, character(1), "source_ref_id"),
    country = c("nl", "de"),
    portal = c("portal-a", "portal-b"),
    native_project_id = c("project-1", "project-2"),
    probability = c(0.91, 0.31),
    selected = c(TRUE, FALSE),
    energy_tags = I(list("wind", "solar")),
    stringsAsFactors = FALSE
  )
}

acq_ui4_app_config <- function(root) {
  curation_config(
    project_id = "ui4-app-project",
    corpus_id = "ui4-app-corpus",
    config_id = "biogain.v1",
    topics = c(topic = "topic"),
    labels = c(keep = "keep", drop = "drop"),
    lexicon = list(topic = "topic"),
    select_fn = function(records, controls) {
      records$selected <- rep(TRUE, nrow(records))
      records
    },
    selection_controls = list(),
    funnel_stages = list(list(
      key = "indexed",
      label = "Indexed",
      fn = function(records, context) rep(TRUE, nrow(records))
    )),
    branding = list(title = "UI4 app fixture", primary = "#123456"),
    artifact_paths = list(root = root),
    translator = NULL
  )
}

acq_ui4_app_artifacts <- function(model, model_path, provenance) {
  curation_artifact_store(
    paths = list(root = dirname(model_path)),
    snapshot_read_fn = function(scope, version, actor) NULL,
    snapshot_write_fn = function(scope, snapshot, actor, expected_revision) {
      list(ok = TRUE, snapshot_id = "snapshot-ui4-app", version = "version-ui4-app", revision = 1L)
    },
    sample_list_fn = function(scope, reviewer_id, actor) {
      data.frame(sample_id = character(0), reviewer_id = character(0), revision = integer(0), stringsAsFactors = FALSE)
    },
    sample_read_fn = function(scope, sample_id, actor) NULL,
    sample_write_fn = function(scope, sample, reviewer_id, actor, expected_revision) {
      list(ok = TRUE, sample_id = sample$sample_id, revision = expected_revision + 1L)
    },
    model_list_fn = function(scope, actor) {
      data.frame(model_id = "model-ui4-app", revision = 1L, stringsAsFactors = FALSE)
    },
    model_read_fn = function(scope, model_id, actor) {
      list(ok = TRUE, model_id = model_id, payload = model, path = model_path, provenance = provenance)
    },
    model_write_fn = function(scope, model, provenance, actor, expected_revision) {
      list(ok = TRUE, model_id = "model-ui4-app", revision = expected_revision + 1L)
    },
    model_publish_fn = function(scope, model_id, actor, expected_active_revision) {
      list(ok = TRUE, active_model_id = model_id, active_revision = expected_active_revision + 1L, provenance = provenance)
    },
    active_model_fn = function(scope, actor) {
      list(ok = TRUE, active_model_id = "model-ui4-app", active_revision = 1L, provenance = provenance)
    }
  )
}

testthat::test_that("preview reports immutable provenance and performs no queue writes", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("openssl")
  fixture <- acq_ui4_catalog()
  on.exit(acquisition_catalog_close(fixture$catalog), add = TRUE)

  before_jobs <- acquisition_queue_jobs(fixture$catalog)
  preview <- acquisition_campaign_preview(
    fixture$catalog,
    selection = acq_ui4_selection(fixture$sources),
    model_id = fixture$model$model_id,
    input_snapshot_id = "snapshot-ui4",
    threshold = 0.5,
    policy_version = "biogain-policy.v1",
    priority_rules = list(
      list(rule_id = "wind-first", countries = "nl", energy = "wind", priority = 90),
      list(rule_id = "fallback", priority = 10)
    )
  )

  testthat::expect_identical(preview$model_id, fixture$model$model_id)
  testthat::expect_identical(preview$input_snapshot_id, "snapshot-ui4")
  testthat::expect_identical(preview$threshold, 0.5)
  testthat::expect_identical(preview$artifact_sha256, fixture$model$artifact_sha256)
  testthat::expect_identical(preview$policy_version, "biogain-policy.v1")
  testthat::expect_identical(preview$counts$projects, 2L)
  testthat::expect_identical(preview$counts$selected_projects, 1L)
  testthat::expect_identical(preview$counts$jobs, 0L)
  testthat::expect_true(isTRUE(preview$attachments$unknown))
  testthat::expect_match(preview$attachments$reason, "discovery")
  testthat::expect_true(all(c("probability", "country", "portal", "native_project_id") %in% names(preview$items)))
  testthat::expect_equal(nrow(acquisition_queue_jobs(fixture$catalog)), nrow(before_jobs))
})

testthat::test_that("a published C3 model is preserved through prediction and campaign preview", {
  for (pkg in c("DBI", "RSQLite", "openssl", "parsnip", "recipes", "rsample", "workflows")) {
    testthat::skip_if_not_installed(pkg)
  }
  fixture <- acq_ui4_catalog()
  on.exit(acquisition_catalog_close(fixture$catalog), add = TRUE)
  artifact_store <- curation_artifact_store_sqlite(list(
    root = file.path(withr::local_tempdir(), "ui4-c3-artifacts")
  ))
  on.exit(artifact_store$close(), add = TRUE)
  actor <- curation_actor("trainer-ui4", roles = "admin")
  scope <- curation_scope("ui4-project", "ui4-corpus", "biogain.v1")
  records <- data.frame(
    country = rep(c("nl", "de"), 4L),
    document_id = paste0("model-record-", seq_len(8L)),
    relevance_score_topic = c(.9, .1, .8, .2, .85, .15, .75, .25),
    class_score_keep = c(.9, .1, .8, .2, .85, .15, .75, .25),
    class_score_drop = c(.1, .9, .2, .8, .15, .85, .25, .75),
    kw_total = c(3, 0, 2, 0, 3, 0, 2, 0),
    stringsAsFactors = FALSE
  )
  snapshot <- records
  attr(snapshot, "curation_snapshot_metadata") <- list(
    config_fingerprint = "config-ui4-real",
    corpus_fingerprint = "corpus-ui4-real",
    filter_coverage = list(
      countries = sort(unique(records$country)),
      n_by_country = as.integer(table(records$country)),
      n = nrow(records)
    )
  )
  saved_snapshot <- curation_artifact_write_snapshot(
    artifact_store,
    scope,
    snapshot,
    actor,
    expected_revision = 0L
  )
  reviews <- data.frame(
    country = records$country,
    document_id = records$document_id,
    reviewer_id = "reviewer-ui4",
    decision = rep(c("keep", "drop"), 4L),
    source = "random",
    reviewed_at = Sys.time() + seq_len(nrow(records)),
    stringsAsFactors = FALSE
  )
  model <- train_selection_model(
    records,
    reviews,
    topics = c(topic = "topic"),
    labels = c(keep = "keep", drop = "drop"),
    v = 2L,
    seed = 42L
  )
  provenance <- list(
    review_fingerprint = curation_artifact_content_fingerprint(reviews),
    snapshot_version = saved_snapshot$version,
    config_fingerprint = "config-ui4-real",
    corpus_snapshot = list(
      version = saved_snapshot$version,
      snapshot_id = saved_snapshot$snapshot_id
    ),
    input = list(
      snapshot_id = saved_snapshot$snapshot_id,
      snapshot_version = saved_snapshot$version
    ),
    training_parameters = list(learner = "logistic", eval_source = "random", v = 2L),
    creator = actor$id
  )
  published <- curation_publish_model(
    artifact_store,
    scope,
    model,
    provenance = provenance,
    actor = actor,
    expected_revision = 0L,
    expected_active_revision = 0L
  )
  active <- curation_artifact_active_model(artifact_store, scope, actor)
  testthat::expect_true(isTRUE(published$ok))
  testthat::expect_identical(active$active_model_id, published$active_model_id)
  registered <- acquisition_catalog_register_curation_model(
    fixture$catalog,
    artifact_store = artifact_store,
    scope = scope,
    model_id = active$active_model_id,
    actor = actor
  )
  persisted <- curation_artifact_read_model(
    artifact_store,
    scope,
    active$active_model_id,
    actor
  )
  predicted <- curation_predict_model(persisted$payload, records)
  selection <- acq_ui4_selection(fixture$sources)
  selection$probability <- predicted$select_prob[seq_len(nrow(selection))]
  selection$selected <- predicted$selected_model[seq_len(nrow(selection))]
  preview <- acquisition_campaign_preview(
    fixture$catalog,
    selection,
    registered$model_id,
    saved_snapshot$snapshot_id,
    threshold = 0.5,
    policy_version = "biogain-policy.v1",
    priority_rules = list(list(rule_id = "fallback", priority = 1))
  )
  testthat::expect_true(inherits(model, "planscanR_selection_model"))
  testthat::expect_true(file.exists(registered$artifact_path))
  testthat::expect_true(file.exists(persisted$path))
  testthat::expect_true(normalizePath(registered$artifact_path) != normalizePath(persisted$path))
  testthat::expect_identical(readRDS(registered$artifact_path)$learner_name, model$learner_name)
  testthat::expect_identical(preview$input_snapshot_id, saved_snapshot$snapshot_id)
  testthat::expect_true(is.character(preview$artifact_sha256))
  testthat::expect_true(nchar(preview$artifact_sha256) >= 32L)
  testthat::expect_true(all(is.finite(preview$items$probability) | is.na(preview$items$probability)))
  testthat::expect_true(isTRUE(preview$model_registered))
  testthat::expect_identical(preview$counts$jobs, 0L)
  submitted <- acquisition_campaign_submit_authenticated(
    catalog = fixture$catalog,
    selection = selection,
    model_id = registered$model_id,
    input_snapshot_id = saved_snapshot$snapshot_id,
    threshold = 0.5,
    policy_version = "biogain-policy.v1",
    priority_rules = list(list(rule_id = "fallback", priority = 1)),
    actor_resolver = curation_actor_resolver(function(request) actor),
    campaign_id = "campaign-ui4-c3"
  )
  testthat::expect_identical(submitted$status, "inserted")
  testthat::expect_identical(submitted$submitted_by, actor$id)
  testthat::expect_equal(
    sum(acquisition_queue_jobs(fixture$catalog)$campaign_id == "campaign-ui4-c3"),
    sum(selection$selected)
  )
})

testthat::test_that("portal-aware identity rejects a mismatched composite instead of merging", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("openssl")
  fixture <- acq_ui4_catalog()
  on.exit(acquisition_catalog_close(fixture$catalog), add = TRUE)
  selection <- acq_ui4_selection(fixture$sources)
  selection$portal[[1L]] <- "portal-b"
  testthat::expect_error(
    acquisition_campaign_preview(
      fixture$catalog, selection, fixture$model$model_id, "snapshot-ui4", 0.5,
      "biogain-policy.v1", list(list(rule_id = "fallback", priority = 1))
    ),
    class = "planscanR_error_acquisition_identity",
    regexp = "portal|identity|source"
  )
  acquisition_catalog_register_source(fixture$catalog, "nl", "portal-collision-a", "project-collision")
  acquisition_catalog_register_source(fixture$catalog, "nl", "portal-collision-b", "project-collision")
  ambiguous <- data.frame(
    country = "nl",
    document_id = "project-collision",
    select_prob = 0.9,
    stringsAsFactors = FALSE
  )
  testthat::expect_error(
    acquisition_campaign_selection_from_records(fixture$catalog, ambiguous, 0.5),
    class = "planscanR_error_acquisition_identity",
    regexp = "ambiguous|portal|identity"
  )
  explicit <- ambiguous
  explicit$portal <- "portal-collision-b"
  resolved <- acquisition_campaign_selection_from_records(fixture$catalog, explicit, 0.5)
  testthat::expect_identical(resolved$portal, "portal-collision-b")
})

testthat::test_that("authenticated submission requires the current admin and pins campaign provenance", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("openssl")
  fixture <- acq_ui4_catalog()
  on.exit(acquisition_catalog_close(fixture$catalog), add = TRUE)
  selection <- acq_ui4_selection(fixture$sources)
  admin <- curation_actor_resolver(function(request) curation_actor("admin-ui4", roles = "admin"))
  reviewer <- curation_actor_resolver(function(request) curation_actor("reviewer-ui4", roles = "reviewer"))
  args <- list(
    catalog = fixture$catalog,
    selection = selection,
    model_id = fixture$model$model_id,
    input_snapshot_id = "snapshot-ui4",
    threshold = 0.5,
    policy_version = "biogain-policy.v1",
    priority_rules = list(list(rule_id = "fallback", priority = 1)),
    campaign_id = "campaign-ui4",
    actor_resolver = admin,
    prepared_snapshot_id = "prepared-ui4",
    prepared_snapshot_version = "prepared-version-ui4",
    prepared_snapshot_fingerprint = "prepared-fingerprint-ui4"
  )
  submitted <- do.call(acquisition_campaign_submit_authenticated, args)
  testthat::expect_identical(submitted$status, "inserted")
  testthat::expect_identical(submitted$submitted_by, "admin-ui4")
  campaign <- acquisition_campaigns(fixture$catalog, campaign_id = "campaign-ui4")
  testthat::expect_identical(campaign$model_id, "model-ui4-foundation")
  testthat::expect_identical(campaign$input_snapshot_id, "prepared-ui4")
  testthat::expect_identical(campaign$submitted_by, "admin-ui4")
  campaign_metadata <- acquisition_campaign_unblob(campaign$metadata_blob[[1L]])
  testthat::expect_identical(campaign_metadata$model_input_snapshot_id, "snapshot-ui4")
  testthat::expect_identical(campaign_metadata$prepared_snapshot_version, "prepared-version-ui4")
  testthat::expect_error(
    do.call(acquisition_campaign_submit_authenticated, utils::modifyList(args, list(
      actor_resolver = reviewer,
      campaign_id = "campaign-ui4-reviewer"
    ))),
    class = "planscanR_error_acquisition_forbidden"
  )
  testthat::expect_error(
    acquisition_campaign_submit_authenticated(
      catalog = fixture$catalog,
      selection = selection,
      model_id = fixture$model$model_id,
      input_snapshot_id = "snapshot-ui4",
      threshold = 0.7,
      policy_version = "biogain-policy.v1",
      priority_rules = list(list(rule_id = "fallback", priority = 1)),
      campaign_id = "campaign-ui4",
      actor_resolver = admin
    ),
    class = "planscanR_error_acquisition_immutable"
  )
  tampered <- tempfile("acq-ui4-tampered-")
  writeLines("changed model bytes", tampered)
  testthat::expect_error(
    acquisition_campaign_submit_authenticated(
      catalog = fixture$catalog,
      selection = selection,
      model_id = fixture$model$model_id,
      input_snapshot_id = "snapshot-ui4",
      threshold = 0.5,
      policy_version = "biogain-policy.v1",
      priority_rules = list(list(rule_id = "fallback", priority = 1)),
      campaign_id = "campaign-ui4-tampered",
      actor_resolver = admin,
      model_artifact_path = tampered
    ),
    class = "planscanR_error_acquisition_immutable"
  )
  unregistered_artifact <- tempfile("acq-ui4-preview-artifact-")
  saveRDS(list(model = "preview-before-submit"), unregistered_artifact)
  unregistered_preview <- acquisition_campaign_preview(
    catalog = fixture$catalog,
    selection = selection,
    model_id = "model-ui4-unregistered",
    input_snapshot_id = "snapshot-ui4",
    threshold = 0.5,
    policy_version = "biogain-policy.v1",
    priority_rules = list(list(rule_id = "fallback", priority = 1)),
    artifact_path = unregistered_artifact,
    config_id = "biogain.v1"
  )
  writeLines("replacement after preview", unregistered_artifact)
  testthat::expect_error(
    acquisition_campaign_submit_authenticated(
      catalog = fixture$catalog,
      selection = selection,
      model_id = "model-ui4-unregistered",
      input_snapshot_id = "snapshot-ui4",
      threshold = 0.5,
      policy_version = "biogain-policy.v1",
      priority_rules = list(list(rule_id = "fallback", priority = 1)),
      actor_resolver = admin,
      model_artifact_path = unregistered_artifact,
      model_config_id = "biogain.v1",
      expected_artifact_sha256 = unregistered_preview$artifact_sha256,
      campaign_id = "campaign-ui4-replaced"
    ),
    class = "planscanR_error_acquisition_immutable"
  )
})

testthat::test_that("the optional campaign panel names preview, unknown totals and explicit submit", {
  testthat::skip_if_not_installed("shiny")
  html <- htmltools::renderTags(acquisition_campaign_panel_ui("ui4"))$html
  testthat::expect_match(html, "Acquisition preview")
  testthat::expect_match(html, "unknown until discovery")
  testthat::expect_match(html, "Submit acquisition campaign")
})

testthat::test_that("the campaign panel submits the displayed artifact digest", {
  for (pkg in c("DBI", "RSQLite", "openssl", "shiny")) {
    testthat::skip_if_not_installed(pkg)
  }
  fixture <- acq_ui4_catalog()
  on.exit(acquisition_catalog_close(fixture$catalog), add = TRUE)
  artifact <- tempfile("acq-ui4-static-preview-")
  saveRDS(list(model = "static-preview"), artifact)
  admin <- curation_actor_resolver(function(request) curation_actor("admin-ui4", roles = "admin"))
  preview <- list(
    selection = acq_ui4_selection(fixture$sources),
    model_id = "model-ui4-static-preview",
    input_snapshot_id = "snapshot-ui4",
    threshold = 0.5,
    policy_version = "biogain-policy.v1",
    priority_rules = list(list(rule_id = "fallback", priority = 1)),
    artifact_path = artifact,
    config_id = "biogain.v1"
  )
  shiny::testServer(
    acquisition_campaign_panel_server,
    args = list(
      id = "campaign",
      catalog = fixture$catalog,
      preview = preview,
      actor_resolver = admin
    ),
    {
      session$setInputs(`campaign-threshold` = 0.5)
      session$flushReact()
      expect_false(is.null(output$`campaign-status`))
      writeLines("replacement after displayed preview", artifact)
      session$setInputs(`campaign-submit` = 1L)
      session$flushReact()
      expect_match(as.character(output$`campaign-submit-result`), "changed after preview")
    }
  )
})

testthat::test_that("the curation app wires its published model into the acquisition panel", {
  for (pkg in c("DBI", "RSQLite", "openssl", "shiny", "bslib", "reactable", "plotly", "htmltools")) {
    testthat::skip_if_not_installed(pkg)
  }
  root <- withr::local_tempdir()
  config <- acq_ui4_app_config(root)
  records <- data.frame(
    country = c("nl", "de"),
    document_id = c("project-1", "project-2"),
    title = c("First project", "Second project"),
    summary = c("First topic", "Second topic"),
    stringsAsFactors = FALSE
  )
  review_repository <- curation_review_repository_sqlite(file.path(root, "reviews.sqlite"))
  on.exit(review_repository$close(), add = TRUE)
  model <- structure(
    list(topics = config$topics, labels = config$labels, threshold = 0.5),
    class = "planscanR_selection_model"
  )
  model_path <- file.path(root, "published-model.rds")
  saveRDS(model, model_path)
  provenance <- list(input = list(snapshot_id = "snapshot-ui4-app"), creator = "admin-ui4")
  artifact_store <- acq_ui4_app_artifacts(model, model_path, provenance)
  catalog <- acquisition_catalog_open(file.path(root, "acquisition.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  model_ref <- acquisition_catalog_register_model(
    catalog,
    model_id = "model-ui4-app",
    artifact_path = model_path,
    config_id = "biogain.v1",
    input_snapshot_id = "snapshot-ui4-app",
    created_by = "admin-ui4"
  )
  acquisition_catalog_register_source(catalog, "nl", "portal-a", "project-1")
  acquisition_catalog_register_source(catalog, "de", "portal-b", "project-2")
  actor_resolver <- curation_actor_resolver(function(request) curation_actor("admin-ui4", roles = "admin"))
  context <- list(
    config = config,
    records_provider = curation_records_provider(
      read_fn = function(corpus_id, countries) records,
      refresh_fn = function(corpus_id, countries) records
    ),
    review_repository = review_repository,
    artifact_store = artifact_store,
    actor_resolver = actor_resolver,
    request = NULL,
    acquisition = list(
      catalog = catalog,
      available = TRUE,
      policy_version = "biogain-policy.v1",
      priority_rules = list(list(rule_id = "fallback", priority = 1L))
    ),
    app_state = new.env(parent = emptyenv())
  )
  seen <- new.env(parent = emptyenv())
  model_calls <- 0L
  testthat::local_mocked_bindings(
    curation_predict_model = function(model, records, config = NULL, threshold = NULL) {
      model_calls <<- model_calls + 1L
      records$select_prob <- c(0.9, 0.2)
      records$selected_model <- records$select_prob >= (threshold %||% 0.5)
      records
    },
    acquisition_campaign_preview_from_curation = function(...) {
      seen$arguments <- list(...)
      list(
        model_id = "model-ui4-app",
        input_snapshot_id = "snapshot-ui4-app",
        artifact_sha256 = model_ref$artifact_sha256,
        threshold = 0.5,
        policy_version = "biogain-policy.v1",
        counts = list(projects = 2L, selected_projects = 1L, jobs = 0L),
        attachments = list(unknown = TRUE, reason = "unknown until discovery"),
        items = data.frame(source_ref_id = "source-fixture", selected = TRUE),
        model_registered = TRUE
      )
    },
    .package = "planscanR.screen"
  )
  app <- function(input, output, session) curation_app_server(input, output, session, context)
  shiny::testServer(app, {
    session$flushReact()
    testthat::expect_true(is.list(seen$arguments))
    testthat::expect_identical(seen$arguments$model_reference$model_id, "model-ui4-app")
    testthat::expect_identical(seen$arguments$policy_version, "biogain-policy.v1")
    calls_after_initial_preview <- model_calls
    testthat::expect_gt(calls_after_initial_preview, 0L)
    session$setInputs(model_threshold = 0.8)
    session$flushReact()
    testthat::expect_identical(model_calls, calls_after_initial_preview)
  })
  testthat::expect_identical(model_ref$model_id, "model-ui4-app")
})
