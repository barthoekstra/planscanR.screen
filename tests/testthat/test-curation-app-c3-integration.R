# ACQ-C4 integration coverage for the generic app's concrete C3 seams.
#
# These tests use synthetic records and temporary SQLite/RDS stores only.  They
# exercise the same payload shapes used by the Shiny server, including restart
# reads and optimistic-concurrency refreshes.

curation_app_c3_skip <- function() {
  for (package in c("shiny", "bslib", "reactable", "plotly", "htmltools", "DBI", "RSQLite")) {
    skip_if_not_installed(package)
  }
}

curation_app_c3_records <- function() {
  tibble::tibble(
    country = c("nl", "de"),
    document_id = c("c3-one", "c3-two"),
    title = c("First topic record", "Second topic record"),
    summary = c("A first synthetic record.", "A second synthetic record.")
  )
}

curation_app_c3_config <- function(root) {
  curation_config(
    project_id = "c3-app-project",
    corpus_id = "c3-app-corpus",
    config_id = "c3-app.v1",
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
    branding = list(title = "C3 app fixture", primary = "#123456"),
    artifact_paths = list(root = root),
    translator = NULL
  )
}

curation_app_c3_pulse <- function(session, input_id, value) {
  do.call(session$setInputs, stats::setNames(list(value), input_id))
  session$flushReact()
  invisible(value)
}

curation_app_c3_prime <- function(session, input_id, value) {
  curation_app_c3_pulse(session, input_id, value)
}

test_that("the app rebuild persists a content-derived snapshot version", {
  curation_app_c3_skip()
  root <- withr::local_tempdir()
  db <- file.path(root, "reviews.sqlite")
  review_repository <- curation_review_repository_sqlite(db)
  artifact_store <- curation_artifact_store_sqlite(list(root = file.path(root, "artifacts")))
  on.exit(review_repository$close(), add = TRUE)
  on.exit(artifact_store$close(), add = TRUE)

  config <- curation_app_c3_config(root)
  scope <- config$identity
  actor <- curation_actor("alice", roles = "reviewer")
  provider <- curation_records_provider(
    read_fn = function(corpus_id, countries) curation_app_c3_records(),
    refresh_fn = function(corpus_id, countries) curation_app_c3_records()
  )
  context <- list(
    config = config,
    records_provider = provider,
    review_repository = review_repository,
    artifact_store = artifact_store,
    actor_resolver = curation_actor_resolver(function(request) actor),
    request = NULL
  )

  shiny::testServer(
    function(input, output, session) {
      curation_app_server(input, output, session, context)
    },
    {
      curation_app_c3_prime(session, "rebuild", 0L)
      curation_app_c3_pulse(session, "rebuild", 1L)
    }
  )

  live <- curation_records_snapshot(provider, config)
  expected <- curation_snapshot_version(live, scope, config = config)
  saved <- curation_app_read_snapshot(artifact_store, scope, actor)
  expect_identical(saved$version, expected)
  expect_identical(saved$records[, c("country", "document_id")], live[, c("country", "document_id")])
})

test_that("compatible persisted startup does not read the provider", {
  curation_app_c3_skip()
  root <- withr::local_tempdir()
  review_repository <- curation_review_repository_sqlite(file.path(root, "reviews.sqlite"))
  artifact_store <- curation_artifact_store_sqlite(list(root = file.path(root, "artifacts")))
  on.exit(review_repository$close(), add = TRUE)
  on.exit(artifact_store$close(), add = TRUE)

  records <- curation_app_c3_records()
  provider_calls <- 0L
  provider <- curation_records_provider(
    read_fn = function(corpus_id, countries) {
      provider_calls <<- provider_calls + 1L
      records
    },
    refresh_fn = function(corpus_id, countries) {
      provider_calls <<- provider_calls + 1L
      records
    }
  )
  config <- curation_app_c3_config(root)
  actor <- curation_actor("alice", roles = "reviewer")
  prepared <- curation_records_snapshot(provider, config)
  curation_artifact_write_snapshot(
    artifact_store, config$identity, prepared, actor,
    expected_revision = 0L
  )
  provider_calls <- 0L
  artifact_reads <- 0L
  enrichment_calls <- 0L
  real_enrich <- get("curation_enrich_records", asNamespace("planscanR.screen"))
  real_snapshot_read <- get(
    "curation_artifact_read_snapshot",
    envir = asNamespace("planscanR.screen")
  )
  testthat::local_mocked_bindings(
    curation_enrich_records = function(...) {
      enrichment_calls <<- enrichment_calls + 1L
      real_enrich(...)
    },
    curation_artifact_read_snapshot = function(store, scope, actor, version = NULL) {
      artifact_reads <<- artifact_reads + 1L
      real_snapshot_read(store, scope, actor, version = version)
    },
    .package = "planscanR.screen"
  )

  context <- list(
    config = config,
    records_provider = provider,
    review_repository = review_repository,
    artifact_store = artifact_store,
    actor_resolver = curation_actor_resolver(function(request) actor),
    request = NULL,
    app_state = new.env(parent = emptyenv())
  )
  shiny::testServer(
    function(input, output, session) {
      curation_app_server(input, output, session, context)
    },
    {}
  )
  shiny::testServer(
    function(input, output, session) {
      curation_app_server(input, output, session, context)
    },
    {}
  )

  expect_identical(provider_calls, 0L)
  expect_identical(artifact_reads, 1L)
  expect_identical(enrichment_calls, 0L)
})

test_that("a cold startup persists once and a restart reuses the snapshot", {
  curation_app_c3_skip()
  root <- withr::local_tempdir()
  review_repository <- curation_review_repository_sqlite(file.path(root, "reviews.sqlite"))
  artifact_store <- curation_artifact_store_sqlite(list(root = file.path(root, "artifacts")))
  on.exit(review_repository$close(), add = TRUE)
  on.exit(artifact_store$close(), add = TRUE)

  provider_calls <- 0L
  provider <- curation_records_provider(
    read_fn = function(corpus_id, countries) {
      provider_calls <<- provider_calls + 1L
      curation_app_c3_records()
    },
    refresh_fn = function(corpus_id, countries) {
      provider_calls <<- provider_calls + 1L
      curation_app_c3_records()
    }
  )
  config <- curation_app_c3_config(root)
  actor <- curation_actor("alice", roles = "reviewer")
  context <- list(
    config = config,
    records_provider = provider,
    review_repository = review_repository,
    artifact_store = artifact_store,
    actor_resolver = curation_actor_resolver(function(request) actor),
    request = NULL
  )

  shiny::testServer(
    function(input, output, session) {
      curation_app_server(input, output, session, context)
    },
    {}
  )
  expect_identical(provider_calls, 1L)

  provider_calls <- 0L
  shiny::testServer(
    function(input, output, session) {
      curation_app_server(input, output, session, context)
    },
    {}
  )
  expect_identical(provider_calls, 0L)
  expect_true(!is.null(curation_app_read_snapshot(artifact_store, config$identity, actor)$records))
})

test_that("a corrupt or schema-mismatched snapshot is rebuilt once and then reused", {
  curation_app_c3_skip()
  for (scenario in c("corrupt", "schema", "config")) {
    root <- withr::local_tempdir()
    review_repository <- curation_review_repository_sqlite(file.path(root, "reviews.sqlite"))
    artifact_store <- curation_artifact_store_sqlite(list(root = file.path(root, "artifacts")))
    on.exit(review_repository$close(), add = TRUE)
    on.exit(artifact_store$close(), add = TRUE)

    provider_calls <- 0L
    provider <- curation_records_provider(
      read_fn = function(corpus_id, countries) {
        provider_calls <<- provider_calls + 1L
        curation_app_c3_records()
      },
      refresh_fn = function(corpus_id, countries) {
        provider_calls <<- provider_calls + 1L
        curation_app_c3_records()
      }
    )
    config <- curation_app_c3_config(root)
    actor <- curation_actor("alice", roles = "reviewer")
    prepared <- curation_records_snapshot(provider, config)
    if (identical(scenario, "schema")) {
      metadata <- attr(prepared, "curation_snapshot_metadata", exact = TRUE)
      metadata$schema_version <- "curation-snapshot.legacy"
      attr(prepared, "curation_snapshot_metadata") <- metadata
    }
    curation_artifact_write_snapshot(
      artifact_store, config$identity, prepared, actor,
      expected_revision = 0L
    )
    saved <- curation_artifact_read_snapshot(artifact_store, config$identity, actor)
    if (identical(scenario, "corrupt")) writeLines("broken", saved$path)
    if (identical(scenario, "config")) config$lexicon$topic <- "changed"
    provider_calls <- 0L

    context <- list(
      config = config,
      records_provider = provider,
      review_repository = review_repository,
      artifact_store = artifact_store,
      actor_resolver = curation_actor_resolver(function(request) actor),
      request = NULL
    )
    shiny::testServer(
      function(input, output, session) {
        curation_app_server(input, output, session, context)
      },
      {}
    )
    expect_identical(provider_calls, 1L)
    expect_true(!is.null(curation_app_read_snapshot(artifact_store, config$identity, actor)$records))

    provider_calls <- 0L
    shiny::testServer(
      function(input, output, session) {
        curation_app_server(input, output, session, context)
      },
      {}
    )
    expect_identical(provider_calls, 0L)
    artifact_store$close()
    review_repository$close()
  }
})

test_that("explicit rebuild publishes new scores and failed refresh keeps the prior snapshot", {
  curation_app_c3_skip()
  root <- withr::local_tempdir()
  review_repository <- curation_review_repository_sqlite(file.path(root, "reviews.sqlite"))
  artifact_store <- curation_artifact_store_sqlite(list(root = file.path(root, "artifacts")))
  on.exit(review_repository$close(), add = TRUE)
  on.exit(artifact_store$close(), add = TRUE)

  base <- curation_app_c3_records()
  base$relevance_score_topic <- c(0.1, 0.2)
  refreshed <- base
  refreshed$relevance_score_topic <- c(0.9, 0.8)
  refresh_mode <- "changed"
  provider_calls <- 0L
  provider <- curation_records_provider(
    read_fn = function(corpus_id, countries) {
      provider_calls <<- provider_calls + 1L
      base
    },
    refresh_fn = function(corpus_id, countries) {
      provider_calls <<- provider_calls + 1L
      if (identical(refresh_mode, "error")) stop("fixture refresh failed")
      refreshed
    }
  )
  config <- curation_app_c3_config(root)
  actor <- curation_actor("alice", roles = "reviewer")
  prepared <- curation_records_snapshot(provider, config)
  curation_artifact_write_snapshot(
    artifact_store, config$identity, prepared, actor,
    expected_revision = 0L
  )
  provider_calls <- 0L
  context <- list(
    config = config,
    records_provider = provider,
    review_repository = review_repository,
    artifact_store = artifact_store,
    actor_resolver = curation_actor_resolver(function(request) actor),
    request = NULL,
    app_state = new.env(parent = emptyenv())
  )

  shiny::testServer(
    function(input, output, session) {
      curation_app_server(input, output, session, context)
    },
    {
      curation_app_c3_pulse(session, "rebuild", 1L)
    }
  )
  expect_identical(provider_calls, 1L)
  published <- curation_app_read_snapshot(artifact_store, config$identity, actor)
  expect_true(any(published$records$cosine_max > prepared$cosine_max))
  published_version <- published$version

  refreshed$relevance_score_topic <- c(0.3, 0.4)
  real_write <- get("curation_artifact_write_snapshot", asNamespace("planscanR.screen"))
  reject_write <- TRUE
  testthat::local_mocked_bindings(
    curation_artifact_write_snapshot = function(...) {
      if (reject_write) stop("fixture publication failure")
      real_write(...)
    },
    .package = "planscanR.screen"
  )
  shiny::testServer(
    function(input, output, session) curation_app_server(input, output, session, context),
    {
      curation_app_c3_pulse(session, "rebuild", 1L)
    }
  )
  expect_identical(context$app_state$snapshot$version, published_version)
  expect_identical(context$app_state$snapshot$records, published$records)
  expect_identical(curation_app_read_snapshot(artifact_store, config$identity, actor)$version, published_version)
  reject_write <- FALSE
  refresh_mode <- "error"
  provider_calls <- 0L
  shiny::testServer(
    function(input, output, session) {
      curation_app_server(input, output, session, context)
    },
    {
      curation_app_c3_pulse(session, "rebuild", 1L)
    }
  )
  expect_identical(provider_calls, 1L)
  expect_identical(
    curation_app_read_snapshot(artifact_store, config$identity, actor)$version,
    published_version
  )
})

test_that("a cached snapshot is rejected when its configuration fingerprint is stale", {
  root <- withr::local_tempdir()
  config <- curation_app_c3_config(root)
  info <- list(metadata = list(config_fingerprint = curation_config_fingerprint(config)))
  expect_true(curation_app_snapshot_config_matches(info, config))

  changed <- config
  changed$selection_controls$kw_min <- 99L
  expect_false(curation_app_snapshot_config_matches(info, changed))
})

test_that("restart reads the newest immutable sample for the authenticated reviewer", {
  curation_app_c3_skip()
  root <- withr::local_tempdir()
  store <- curation_artifact_store_sqlite(list(root = root))
  on.exit(store$close(), add = TRUE)
  scope <- curation_scope("sample-project", "sample-corpus", "sample.v1")
  actor <- curation_actor("alice", roles = "reviewer")

  first <- list(
    sample_id = "sample-first",
    reviewer_id = actor$id,
    queue = tibble::tibble(country = "nl", document_id = "first")
  )
  attr(first, "curation_sample_metadata") <- list(sample_id = first$sample_id)
  second <- list(
    sample_id = "sample-second",
    reviewer_id = actor$id,
    queue = tibble::tibble(country = "de", document_id = "second")
  )
  attr(second, "curation_sample_metadata") <- list(sample_id = second$sample_id)

  expect_identical(
    curation_artifact_write_sample(store, scope, first, actor$id, actor, 0L)$sample_id,
    "sample-first"
  )
  expect_identical(
    curation_artifact_write_sample(store, scope, second, actor$id, actor, 0L)$sample_id,
    "sample-second"
  )

  restored <- curation_app_read_sample(store, scope, actor)
  expect_identical(restored$id, "sample-second")
  expect_identical(restored$queue$document_id, "second")
})

test_that("sample ID generation does not change the caller RNG stream", {
  scope <- curation_scope("sample-project", "sample-corpus", "sample.v1")
  queue <- tibble::tibble(country = "nl", document_id = "one")
  set.seed(20261002)
  before <- .Random.seed
  id <- curation_app_new_sample_id(scope, "alice", queue, seed = 7L)
  expect_match(id, "^sample-[0-9a-f]+$")
  expect_identical(.Random.seed, before)
})

test_that("a review conflict refreshes the canonical row without retrying a stale write", {
  rows <- data.frame(
    country = "nl", document_id = "shared", reviewer_id = "alice",
    decision = "keep", source = "browse", note = NA_character_,
    revision = 1L, deleted = FALSE, stringsAsFactors = FALSE
  )
  current <- rows
  current$decision <- "drop"
  current$revision <- 2L

  refreshed <- curation_app_merge_review_row(rows, current)
  expect_identical(refreshed$decision, "drop")
  expect_identical(refreshed$revision, 2L)
})

test_that("cross-session review refresh preserves a pending local CAS revision", {
  local <- data.frame(
    country = c("nl", "de"),
    document_id = c("pending", "remote"),
    reviewer_id = c("alice", "alice"),
    decision = c("keep", "keep"),
    source = c("browse", "browse"),
    revision = c(1L, 1L),
    deleted = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  incoming <- local
  incoming$decision <- c("drop", "drop")
  incoming$revision <- c(2L, 2L)
  incoming <- rbind(
    incoming,
    data.frame(
      country = "nl", document_id = "new", reviewer_id = "alice",
      decision = "unsure", source = "browse", revision = 1L,
      deleted = FALSE, stringsAsFactors = FALSE
    )
  )
  pending_key <- curation_app_key_string("nl", "pending", "alice")
  merged <- curation_app_merge_reviews_preserving_pending(
    local, incoming,
    pending_keys = pending_key
  )
  pending <- merged[merged$document_id == "pending", , drop = FALSE]
  remote <- merged[merged$document_id == "remote", , drop = FALSE]
  added <- merged[merged$document_id == "new", , drop = FALSE]
  expect_identical(pending$decision, "keep")
  expect_identical(pending$revision, 1L)
  expect_identical(remote$decision, "drop")
  expect_identical(remote$revision, 2L)
  expect_identical(added$decision, "unsure")
  updates <- curation_app_review_refresh_updates(local, incoming, pending_key)
  expect_identical(vapply(updates, `[[`, character(1), "key"), c(
    curation_app_key_string("de", "remote", "alice"),
    curation_app_key_string("nl", "new", "alice")
  ))
  expect_identical(vapply(updates, function(update) update[["revision"]], integer(1)), c(2L, 1L))
  canonical <- curation_app_canonical_review_update(
    merged,
    curation_review_key("de", "remote", "alice")
  )
  expect_identical(canonical$decision, "drop")
  expect_identical(canonical$revision, 2L)
})

test_that("the app re-resolves current roles before a mutation", {
  actor_id <- "alice"
  roles <- "reviewer"
  resolver <- curation_actor_resolver(function(request) {
    curation_actor(actor_id, roles = roles)
  })
  context <- list(actor_resolver = resolver, request = NULL)
  initial <- curation_actor("alice", roles = "reviewer")

  expect_identical(
    curation_app_resolve_current_actor(context, list(), initial)$roles,
    "reviewer"
  )
  roles <- "admin"
  expect_identical(
    curation_app_resolve_current_actor(context, list(), initial)$roles,
    "admin"
  )
  actor_id <- "other-user"
  expect_error(
    curation_app_resolve_current_actor(context, list(), initial),
    class = "planscanR_error_curation_actor"
  )
})

test_that("model provenance contains the immutable C3 training inputs", {
  root <- withr::local_tempdir()
  config <- curation_app_c3_config(root)
  scope <- config$identity
  actor <- curation_actor("trainer", roles = "reviewer")
  records <- curation_records_snapshot(
    curation_records_provider(function(corpus_id, countries) curation_app_c3_records()),
    config
  )
  reviews <- data.frame(
    country = "nl", document_id = "c3-one", reviewer_id = actor$id,
    decision = "keep", source = "random", stringsAsFactors = FALSE
  )

  provenance <- curation_app_model_provenance(
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
  expect_true(all(c(
    "review_fingerprint", "snapshot_version", "config_fingerprint",
    "corpus_snapshot", "training_parameters", "creator"
  ) %in% names(provenance)))
  expect_identical(provenance$creator, actor$id)
  expect_identical(provenance$corpus_snapshot$corpus_id, scope$corpus_id)
  expect_identical(provenance$corpus_snapshot$version, provenance$snapshot_version)
})


test_that("separate apps and scopes isolate corpora while reviewers keep their own decisions", {
  curation_app_c3_skip()
  root <- withr::local_tempdir()
  repo <- curation_review_repository_sqlite(file.path(root, "reviews.sqlite"))
  store <- curation_artifact_store_sqlite(list(root = file.path(root, "artifacts")))
  on.exit(repo$close(), add = TRUE)
  on.exit(store$close(), add = TRUE)
  config <- curation_app_c3_config(root)
  actor <- curation_actor("alice", roles = "reviewer")
  provider <- curation_records_provider(function(corpus_id, countries) curation_app_c3_records())
  resolver <- curation_actor_resolver(function(request) actor)
  app <- curation_app(config, provider, repo, store, resolver)
  other_config <- config
  other_config$identity <- curation_scope("other-project", "other-corpus", "c3-app.v1")
  other <- curation_app(other_config, provider, repo, store, resolver)
  state <- environment(app$serverFuncSource())$context$app_state
  other_state <- environment(other$serverFuncSource())$context$app_state
  expect_false(identical(state, other_state))
  for (reviewer in c("alice", "bob")) {
    actor <- curation_actor(reviewer, roles = "reviewer")
    shiny::testServer(app, {
      curation_app_c3_prime(session, "row_decision", list(
        id = "prime", country = "nl", decision = "keep", source = "browse", revision = 0L, nonce = 0L
      ))
      curation_app_c3_pulse(session, "row_decision", list(
        id = "c3-one", country = "nl", decision = if (reviewer == "alice") "keep" else "drop",
        source = "browse", revision = 0L, nonce = 1L
      ))
    })
  }
  rows <- curation_review_list(repo, config$identity, actor = actor)
  expect_setequal(rows$reviewer_id, c("alice", "bob"))
  expect_identical(rows$decision[rows$reviewer_id == "alice"], "keep")
  expect_identical(rows$decision[rows$reviewer_id == "bob"], "drop")
  expect_setequal(ls(state), "snapshot")
  shiny::testServer(other, {})
  expect_false(identical(state$snapshot$version, other_state$snapshot$version))
  expect_equal(nrow(curation_review_list(repo, other_config$identity, actor = actor)), 0L)
})
