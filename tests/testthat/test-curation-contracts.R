test_that("curation configs validate and keep unrelated configurations isolated", {
  stages <- list(
    list(
      key = "indexed",
      label = "Indexed",
      fn = function(records, context) rep(TRUE, nrow(records))
    )
  )
  select_fn <- function(records, controls) {
    records$selected <- records$score >= controls$cutoff
    records
  }

  cfg_a <- curation_config(
    project_id = "project-a",
    corpus_id = "corpus-a",
    config_id = "fixture-a.v1",
    topics = c(alpha = "alpha topic"),
    labels = c(positive = "positive label", negative = "negative label"),
    lexicon = list(alpha = c("alpha", "first")),
    select_fn = select_fn,
    selection_controls = list(cutoff = 0.5),
    funnel_stages = stages,
    branding = list(
      title = "Fixture A",
      primary = "#123456",
      secondary = "#654321",
      success = "#246813"
    ),
    artifact_paths = list(
      root = "/tmp/fixture-a",
      snapshots = "/tmp/fixture-a/snapshots",
      samples = "/tmp/fixture-a/samples",
      models = "/tmp/fixture-a/models",
      exports = "/tmp/fixture-a/exports"
    )
  )
  cfg_b <- curation_config(
    project_id = "project-b",
    corpus_id = "corpus-b",
    config_id = "fixture-b.v1",
    topics = c(beta = "beta topic"),
    labels = c(keep = "keep label", drop = "drop label"),
    lexicon = list(beta = c("beta", "second")),
    select_fn = select_fn,
    selection_controls = list(cutoff = 0.8),
    funnel_stages = stages,
    branding = list(
      title = "Fixture B",
      primary = "#abcdef",
      secondary = "#fedcba",
      success = "#135790"
    ),
    artifact_paths = list(
      root = "/tmp/fixture-b",
      snapshots = "/tmp/fixture-b/snapshots",
      samples = "/tmp/fixture-b/samples",
      models = "/tmp/fixture-b/models",
      exports = "/tmp/fixture-b/exports"
    )
  )

  expect_s3_class(cfg_a, "planscanR_curation_config")
  expect_s3_class(cfg_b, "planscanR_curation_config")
  expect_identical(cfg_a$topics, c(alpha = "alpha topic"))
  expect_identical(cfg_b$topics, c(beta = "beta topic"))
  expect_identical(cfg_a$identity$project_id, "project-a")
  expect_identical(cfg_b$identity$config_id, "fixture-b.v1")
  expect_false(any(grepl("wind|solar|fossil|renewable", names(cfg_a$topics))))
  expect_false(any(grepl("wind|solar|fossil|renewable", names(cfg_b$topics))))

  old_option <- getOption("planscanR.cache_dir")
  on.exit(options(planscanR.cache_dir = old_option), add = TRUE)
  options(planscanR.cache_dir = "/tmp/should-not-be-read")
  expect_identical(cfg_a$artifact_paths$root, "/tmp/fixture-a")
  expect_identical(cfg_b$artifact_paths$root, "/tmp/fixture-b")
  expect_false(identical(cfg_a$identity, cfg_b$identity))
})

test_that("curation config rejects malformed project inputs", {
  common <- list(
    project_id = "project",
    corpus_id = "corpus",
    config_id = "config.v1",
    topics = c(topic = "topic phrase"),
    labels = c(keep = "keep", drop = "drop"),
    lexicon = list(topic = "term"),
    select_fn = function(records, controls) records,
    selection_controls = list(),
    funnel_stages = list(list(
      key = "indexed",
      label = "Indexed",
      fn = function(records, context) rep(TRUE, nrow(records))
    )),
    branding = list(title = "Fixture", primary = "#123456"),
    artifact_paths = list(root = "/tmp/fixture")
  )

  expect_error(
    do.call(curation_config, modifyList(common, list(project_id = ""))),
    class = "planscanR_error_curation_config"
  )
  expect_error(
    do.call(curation_config, modifyList(common, list(topics = c("unnamed")))),
    class = "planscanR_error_curation_config"
  )
  expect_error(
    do.call(curation_config, modifyList(common, list(
      labels = c(keep = "keep"),
      selection_controls = list()
    ))),
    class = "planscanR_error_curation_config"
  )
  bad_labels <- c(keep = "keep", drop = "drop")
  attr(bad_labels, "relevant") <- "missing"
  expect_error(
    do.call(curation_config, modifyList(common, list(labels = bad_labels))),
    class = "planscanR_error_curation_config"
  )
  malformed_lexicon <- common
  malformed_lexicon$lexicon <- list()
  expect_error(
    do.call(curation_config, malformed_lexicon),
    class = "planscanR_error_curation_config"
  )
  expect_error(
    do.call(curation_config, modifyList(common, list(select_fn = identity))),
    class = "planscanR_error_curation_config"
  )
})

test_that("selection and funnel contracts receive scoped values explicitly", {
  seen <- new.env(parent = emptyenv())
  seen$controls <- NULL
  stages <- list(
    list(
      key = "indexed",
      label = "Indexed",
      fn = function(records, context) rep(TRUE, nrow(records))
    ),
    list(
      key = "selected",
      label = "Selected",
      fn = function(records, context) records$selected %in% TRUE
    )
  )
  cfg <- curation_config(
    project_id = "project",
    corpus_id = "corpus",
    config_id = "fixture.v1",
    topics = c(topic = "unrelated topic"),
    labels = c(keep = "unrelated keep", drop = "unrelated drop"),
    lexicon = list(topic = "unrelated"),
    select_fn = function(records, controls) {
      seen$controls <- controls
      records$selected <- records$score >= controls$cutoff
      records
    },
    selection_controls = list(cutoff = 0.5),
    funnel_stages = stages,
    branding = list(title = "Fixture", primary = "#123456"),
    artifact_paths = list(root = "/tmp/fixture")
  )
  records <- data.frame(
    country = c("aa", "aa"),
    document_id = c("1", "2"),
    score = c(0.4, 0.9),
    stringsAsFactors = FALSE
  )

  selected <- curation_apply_selection(cfg, records, controls = list(cutoff = 0.8))
  expect_identical(seen$controls$cutoff, 0.8)
  expect_identical(selected$selected, c(FALSE, TRUE))
  funnel <- curation_compute_funnel(cfg, selected)
  expect_identical(funnel$key, c("indexed", "selected"))
  expect_identical(funnel$n, c(2L, 1L))
})

test_that("trusted actors and repository writes carry mandatory revisions", {
  actor <- curation_actor(
    id = "user-1",
    roles = "reviewer",
    display_name = "Fixture User"
  )
  expect_s3_class(actor, "planscanR_curation_actor")
  resolver <- curation_actor_resolver(function(request) actor)
  expect_identical(curation_resolve_actor(resolver, list(session = "fixture")), actor)

  scope <- curation_scope("project", "corpus", "config.v1")
  calls <- new.env(parent = emptyenv())
  calls$upsert <- NULL
  calls$clear <- NULL
  calls$bulk <- NULL
  calls$import <- NULL
  calls$import_rows <- NULL
  repo <- curation_review_repository(
    read_fn = function(scope, key, actor) key,
    list_fn = function(scope, filter, actor, limit, offset) data.frame(),
    upsert_fn = function(scope, decision, actor, expected_revision) {
      calls$upsert <- expected_revision
      decision
    },
    clear_fn = function(scope, key, actor, expected_revision) {
      calls$clear <- expected_revision
      key
    },
    bulk_upsert_fn = function(scope, decisions, actor, expected_revision) {
      calls$bulk <- expected_revision
      decisions
    },
    import_fn = function(scope, rows, actor, expected_revision) {
      calls$import <- expected_revision
      calls$import_rows <- rows
      rows
    },
    export_fn = function(scope, actor, format) data.frame()
  )
  decision <- data.frame(
    country = "aa",
    document_id = "1",
    reviewer_id = actor$id,
    decision = "keep",
    source = "browse",
    import_source = "legacy-fixture",
    import_row_id = "row-1",
    import_fingerprint = "fingerprint-1",
    reviewed_at = "2026-09-30T12:34:56Z",
    stringsAsFactors = FALSE
  )
  key <- list(country = "aa", document_id = "1", reviewer_id = actor$id)

  curation_review_upsert(repo, scope, decision, actor, expected_revision = 0L)
  curation_review_clear(repo, scope, key, actor, expected_revision = 1L)
  curation_review_bulk_upsert(
    repo,
    scope,
    rbind(decision, decision),
    actor,
    expected_revision = c(0L, 1L)
  )
  missing_import_mapping <- decision
  missing_import_mapping$import_source <- NULL
  expect_error(
    curation_review_import(
      repo, scope, missing_import_mapping, actor,
      expected_revision = 0L
    ),
    class = "planscanR_error_curation_repository"
  )
  curation_review_import(repo, scope, decision, actor, expected_revision = 0L)
  expect_identical(calls$upsert, 0L)
  expect_identical(calls$clear, 1L)
  expect_identical(calls$bulk, c(0L, 1L))
  expect_identical(calls$import, 0L)
  expect_identical(
    names(calls$import_rows),
    c(
      "country", "document_id", "reviewer_id", "decision", "source", "note",
      "reviewed_at", "sidecar_path", "revision", "deleted", "import_source",
      "import_row_id", "import_fingerprint"
    )
  )
  expect_identical(
    format(calls$import_rows$reviewed_at, "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    "2026-09-30T12:34:56"
  )
  timezone_less_row <- decision
  timezone_less_row$reviewed_at <- "2026-09-30 12:34:56"
  curation_review_import(repo, scope, timezone_less_row, actor, expected_revision = 1L)
  expect_identical(
    format(calls$import_rows$reviewed_at, "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    "2026-09-30T12:34:56"
  )
  timezone_less_row$reviewed_at <- "2026-09-30T12:34:56"
  curation_review_import(repo, scope, timezone_less_row, actor, expected_revision = 2L)
  expect_identical(
    format(calls$import_rows$reviewed_at, "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    "2026-09-30T12:34:56"
  )
  offset_row <- decision
  offset_row$reviewed_at <- "2026-09-30T12:34:56+02:00"
  curation_review_import(repo, scope, offset_row, actor, expected_revision = 1L)
  expect_identical(
    format(calls$import_rows$reviewed_at, "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    "2026-09-30T10:34:56"
  )
  offset_row$reviewed_at <- "2026-09-30T12:34:56-05:30"
  curation_review_import(repo, scope, offset_row, actor, expected_revision = 2L)
  expect_identical(
    format(calls$import_rows$reviewed_at, "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    "2026-09-30T18:04:56"
  )
  invalid_row <- offset_row
  invalid_row$reviewed_at <- "2026-09-30T25:34:56Z"
  expect_error(
    curation_review_import(repo, scope, invalid_row, actor, expected_revision = 3L),
    class = "planscanR_error_curation_repository"
  )
  invalid_row$reviewed_at <- "2026-09-30T12:34:56Ztrailing"
  expect_error(
    curation_review_import(repo, scope, invalid_row, actor, expected_revision = 3L),
    class = "planscanR_error_curation_repository"
  )
  expect_error(
    curation_review_upsert(repo, scope, decision, actor),
    class = "planscanR_error_curation_revision"
  )
  expect_error(
    curation_review_bulk_upsert(repo, scope, decision, actor, expected_revision = NULL),
    class = "planscanR_error_curation_revision"
  )
  expect_error(
    curation_review_upsert(repo, scope, decision, actor, expected_revision = Inf),
    class = "planscanR_error_curation_revision"
  )
  expect_error(
    curation_review_list(repo, scope, actor = actor, limit = Inf),
    class = "planscanR_error_curation_repository"
  )
})

test_that("artifact store methods preserve config identity and revisions", {
  scope <- curation_scope("project", "corpus", "fixture.v2")
  actor <- curation_actor("user-1", roles = "reviewer")
  seen <- new.env(parent = emptyenv())
  seen$scope <- NULL
  seen$revision <- NULL
  store <- curation_artifact_store(
    paths = list(
      root = "/tmp/fixture", snapshots = "/tmp/fixture/snapshots",
      samples = "/tmp/fixture/samples", models = "/tmp/fixture/models",
      exports = "/tmp/fixture/exports"
    ),
    snapshot_read_fn = function(scope, version, actor) scope,
    snapshot_write_fn = function(scope, snapshot, actor, expected_revision) {
      seen$scope <- scope
      seen$revision <- expected_revision
      snapshot
    },
    sample_list_fn = function(scope, reviewer_id, actor) list(),
    sample_read_fn = function(scope, sample_id, actor) NULL,
    sample_write_fn = function(scope, sample, reviewer_id, actor, expected_revision) sample,
    model_list_fn = function(scope, actor) list(),
    model_read_fn = function(scope, model_id, actor) NULL,
    model_write_fn = function(scope, model, provenance, actor, expected_revision) model,
    model_publish_fn = function(scope, model_id, actor, expected_active_revision) model_id,
    active_model_fn = function(scope, actor) NULL
  )

  curation_artifact_write_snapshot(
    store, scope, list(records = 1L), actor,
    expected_revision = 0L
  )
  expect_identical(seen$scope$config_id, "fixture.v2")
  expect_identical(seen$revision, 0L)
  expect_error(
    curation_artifact_write_snapshot(store, scope, list(), actor),
    class = "planscanR_error_curation_revision"
  )
})

test_that("record providers use explicit corpus and exported cache seams", {
  calls <- new.env(parent = emptyenv())
  calls$corpus <- NULL
  provider <- curation_records_provider_cache(
    cache_dir = "/tmp/fixture-cache",
    index_fn = function(cache_dir, country) {
      calls$corpus <- cache_dir
      if (is.null(country)) {
        return(data.frame(
          country = "aa", document_id = "1", title = "Fixture",
          stringsAsFactors = FALSE
        ))
      }
      data.frame(
        country = country, document_id = paste0(country, "-1"),
        title = "Fixture", stringsAsFactors = FALSE
      )
    }
  )
  records <- curation_records_read(provider, "corpus-a", countries = c("aa", "bb"))
  expect_identical(calls$corpus, "/tmp/fixture-cache")
  expect_identical(records$country, c("aa", "bb"))
  expect_identical(records$document_id, c("aa-1", "bb-1"))
})

test_that("translation is optional and writes require an explicit serialized gate", {
  record <- data.frame(
    country = "aa",
    document_id = "1",
    title = "Original title",
    summary = "Original summary",
    stringsAsFactors = FALSE
  )
  disabled <- curation_translate(
    NULL,
    record,
    key = list(country = "aa", document_id = "1"),
    source_language = "xx",
    target_language = "en"
  )
  expect_identical(disabled$status, "disabled")
  expect_identical(disabled$title, "Original title")

  expect_error(
    curation_translation_provider(
      translate_fn = function(record, source_language, target_language) list(),
      service_id = "fixture",
      write_policy = "serialized"
    ),
    class = "planscanR_error_curation_translation"
  )
  provider <- curation_translation_provider(
    translate_fn = function(record, source_language, target_language) {
      list(title_en = "Translated title", summary_en = "Translated summary")
    },
    service_id = "fixture-v1",
    cache_write_fn = function(key, value) invisible(value),
    write_gate_fn = function(key, write_fn) write_fn(),
    write_policy = "serialized"
  )
  translated <- curation_translate(
    provider,
    record,
    key = list(country = "aa", document_id = "1"),
    source_language = "xx",
    target_language = "en",
    write = TRUE
  )
  expect_identical(translated$status, "translated")
  expect_identical(translated$service, "fixture-v1")
  expect_identical(translated$title_en, "Translated title")

  failing <- curation_translation_provider(
    translate_fn = function(record, source_language, target_language) {
      stop("fixture worker failure")
    },
    service_id = "fixture-error"
  )
  failed <- curation_translate(
    failing,
    record,
    key = list(country = "aa", document_id = "1"),
    source_language = "xx",
    target_language = "en"
  )
  expect_identical(failed$status, "error")
  expect_identical(failed$title, "Original title")
  expect_match(failed$error, "fixture worker failure")
})
