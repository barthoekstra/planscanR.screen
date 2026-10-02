# ACQ-C3: concrete artifact persistence, identity invalidation, and CAS
# publication. These tests deliberately use the package's public artifact
# contract and local SQLite/RDS fixtures only.

c3_test_paths <- function() {
  root <- tempfile("planscanR-c3-artifacts-")
  dir.create(root, recursive = TRUE)
  root
}

c3_test_scope <- function(project = "project-c3", corpus = "corpus-c3", config = "policy.v1") {
  curation_scope(project, corpus, config)
}

c3_test_actor <- function(id, roles = "reviewer") {
  curation_actor(id, roles = roles)
}

c3_test_config <- function(topic = "signal topic") {
  curation_config(
    project_id = "project-c3",
    corpus_id = "corpus-c3",
    config_id = "policy.v1",
    topics = c(signal = topic),
    labels = c(keep = "keep", drop = "drop"),
    lexicon = list(signal = "signal"),
    select_fn = function(records, controls) {
      records$selected <- TRUE
      records
    },
    selection_controls = list(),
    funnel_stages = list(list(
      key = "indexed",
      label = "Indexed",
      fn = function(records, context) rep(TRUE, nrow(records))
    )),
    branding = list(title = "C3 fixture", primary = "#123456"),
    artifact_paths = list(root = tempfile("c3-config-root-"))
  )
}

c3_test_model <- function(tag = "one") {
  structure(
    list(
      workflow = NULL,
      learner_name = "fixture",
      features = "feature",
      topics = c(signal = "signal topic"),
      labels = c(keep = "keep", drop = "drop"),
      fixture_tag = tag
    ),
    class = "planscanR_selection_model"
  )
}

c3_test_provenance <- function(scope, actor, tag = "fixture") {
  list(
    review_fingerprint = paste0("reviews-", tag),
    snapshot_version = paste0("snapshot-", tag),
    config_fingerprint = paste0("config-", tag),
    corpus_snapshot = list(corpus_id = scope$corpus_id, version = paste0("corpus-", tag)),
    training_parameters = list(seed = 42L, learner = "fixture"),
    creator = actor$id
  )
}

test_that("relative artifact roots are resolved once from the caller directory", {
  caller <- getwd()
  base <- tempfile("planscanR-c3-relative-root-")
  dir.create(base, recursive = TRUE)
  on.exit(setwd(caller), add = TRUE)
  setwd(base)
  paths <- curation_artifact_sqlite_paths(list(root = "relative-root"))
  expect_identical(
    paths$root,
    normalizePath(file.path(base, "relative-root"), mustWork = FALSE)
  )
  expect_identical(dirname(paths$database), paths$root)
  expect_error(
    curation_artifact_sqlite_paths(list(root = base, models = tempdir())),
    "inside the explicit artifact root"
  )
})

test_that("snapshot versions include content, configuration, schema, and filter coverage", {
  scope <- c3_test_scope()
  records <- tibble::tibble(
    country = c("nl", "nl"),
    document_id = c("one", "two"),
    title = c("first", "second"),
    summary = c("summary one", "summary two")
  )
  config_a <- c3_test_config("signal topic")
  config_b <- c3_test_config("changed topic")

  first <- curation_snapshot_version(records, scope, config = config_a, countries = "nl")
  changed_content <- records
  changed_content$title[[1L]] <- "changed"
  second <- curation_snapshot_version(
    changed_content,
    scope,
    config = config_a,
    countries = "nl"
  )
  changed_config <- curation_snapshot_version(
    records,
    scope,
    config = config_b,
    countries = "nl"
  )
  changed_filter <- curation_snapshot_version(
    records,
    scope,
    config = config_a,
    countries = c("nl", "de")
  )
  stale_a <- structure(
    records,
    curation_snapshot_metadata = list(
      version = "stale-version",
      config_fingerprint = curation_config_fingerprint(config_a)
    )
  )
  stale_b <- structure(
    changed_content,
    curation_snapshot_metadata = list(
      version = "stale-version",
      config_fingerprint = curation_config_fingerprint(config_b)
    )
  )
  make_select_fn <- function(limit) {
    local({
      threshold <- limit
      function(records, controls) {
        records$selected <- nrow(records) > threshold
        records
      }
    })
  }
  closure_config_a <- c3_test_config()
  closure_config_a$select_fn <- make_select_fn(1L)
  closure_config_b <- c3_test_config()
  closure_config_b$select_fn <- make_select_fn(2L)

  expect_type(first, "character")
  expect_length(first, 1L)
  expect_false(identical(first, second))
  expect_false(identical(first, changed_config))
  expect_false(identical(first, changed_filter))
  expect_false(identical(
    curation_snapshot_version(stale_a, scope),
    curation_snapshot_version(stale_b, scope)
  ))
  expect_false(identical(
    curation_config_fingerprint(closure_config_a),
    curation_config_fingerprint(closure_config_b)
  ))
})

test_that("SQLite artifact store isolates samples by scope and reviewer", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  root <- c3_test_paths()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  store <- curation_artifact_store_sqlite(list(root = root))
  on.exit(store$close(), add = TRUE)
  scope_a <- c3_test_scope(config = "policy.a")
  scope_b <- c3_test_scope(config = "policy.b")
  alice <- c3_test_actor("alice")
  bob <- c3_test_actor("bob")
  sample <- tibble::tibble(country = "nl", document_id = "same-native-id")

  written_a <- curation_artifact_write_sample(
    store, scope_a, sample,
    reviewer_id = alice$id, actor = alice,
    expected_revision = 0L
  )
  written_b <- curation_artifact_write_sample(
    store, scope_b, sample,
    reviewer_id = bob$id, actor = bob,
    expected_revision = 0L
  )

  expect_true(isTRUE(written_a$ok))
  expect_true(isTRUE(written_b$ok))
  expect_false(identical(written_a$sample_id, written_b$sample_id))
  expect_equal(nrow(curation_artifact_list_samples(store, scope_a, alice$id, alice)), 1L)
  expect_equal(nrow(curation_artifact_list_samples(store, scope_a, bob$id, bob)), 0L)
  expect_equal(
    curation_artifact_read_sample(store, scope_a, written_a$sample_id, alice)$payload,
    sample
  )
  expect_error(
    curation_artifact_read_sample(store, scope_a, written_a$sample_id, bob),
    class = "planscanR_error_curation_authorization"
  )
  expect_null(curation_artifact_read_sample(store, scope_b, written_a$sample_id, bob))

  shared_a <- structure(
    tibble::tibble(country = "nl", document_id = "alice-shared"),
    curation_sample_metadata = list(sample_id = "same-caller-id")
  )
  shared_b <- structure(
    tibble::tibble(country = "nl", document_id = "bob-shared"),
    curation_sample_metadata = list(sample_id = "same-caller-id")
  )
  curation_artifact_write_sample(
    store, scope_a, shared_a,
    reviewer_id = alice$id, actor = alice,
    expected_revision = 0L
  )
  curation_artifact_write_sample(
    store, scope_a, shared_b,
    reviewer_id = bob$id, actor = bob,
    expected_revision = 0L
  )
  expect_identical(
    curation_artifact_read_sample(store, scope_a, "same-caller-id", alice)$payload,
    shared_a
  )
  expect_identical(
    curation_artifact_read_sample(store, scope_a, "same-caller-id", bob)$payload,
    shared_b
  )
  expect_error(
    curation_artifact_read_sample(
      store, scope_a, "same-caller-id", c3_test_actor("administrator", roles = "admin")
    ),
    "ambiguous across reviewers"
  )
})

test_that("snapshot writes invalidate same IDs when content or config changes", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  root <- c3_test_paths()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  store <- curation_artifact_store_sqlite(list(root = root))
  on.exit(store$close(), add = TRUE)
  scope <- c3_test_scope()
  actor <- c3_test_actor("alice")
  config <- c3_test_config()
  records <- tibble::tibble(
    country = "nl", document_id = "same", title = "original", summary = "text"
  )
  first <- structure(
    records,
    curation_snapshot_metadata = list(
      config_fingerprint = curation_config_fingerprint(config),
      countries = "nl"
    )
  )
  first_write <- curation_artifact_write_snapshot(
    store, scope, first, actor,
    expected_revision = 0L
  )
  changed <- first
  changed$title[[1L]] <- "updated"
  second_write <- curation_artifact_write_snapshot(
    store, scope, changed, actor,
    expected_revision = 0L
  )

  expect_true(isTRUE(first_write$ok))
  expect_true(isTRUE(second_write$ok))
  expect_false(identical(first_write$version, second_write$version))
  latest <- curation_artifact_read_snapshot(store, scope, actor)
  expect_identical(latest$version, second_write$version)
  expect_identical(latest$payload$title, "updated")
  expect_false(grepl("/", basename(latest$path), fixed = TRUE))
  expect_equal(nrow(curation_artifact_read_snapshot(store, scope, actor, first_write$version)$payload), 1L)
})

test_that("model versions retain legacy payload format and provenance", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  root <- c3_test_paths()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  store <- curation_artifact_store_sqlite(list(root = root))
  on.exit(store$close(), add = TRUE)
  scope <- c3_test_scope()
  actor <- c3_test_actor("trainer")
  model <- c3_test_model()
  provenance <- list(
    review_fingerprint = "reviews-sha",
    snapshot_version = "snapshot-sha",
    config_fingerprint = "config-sha",
    corpus_snapshot = list(corpus_id = scope$corpus_id, version = "corpus-sha"),
    training_parameters = list(seed = 42L, learner = "fixture"),
    creator = actor$id
  )
  written <- curation_artifact_write_model(
    store, scope, model, provenance, actor,
    expected_revision = 0L
  )
  read <- curation_artifact_read_model(store, scope, written$model_id, actor)

  expect_true(isTRUE(written$ok))
  expect_s3_class(read$payload, "planscanR_selection_model")
  expect_identical(read$payload$fixture_tag, "one")
  expect_identical(read$provenance$review_fingerprint, "reviews-sha")
  expect_identical(read$provenance$creator, actor$id)
  expect_true(file.exists(read$path))
  expect_error(
    curation_artifact_write_model(
      store, scope, c3_test_model("overwrite"), provenance, actor,
      expected_revision = 1L
    ),
    class = "planscanR_error_curation_conflict"
  )

  legacy_path <- file.path(root, "selection_model.rds")
  saveRDS(model, legacy_path)
  expect_s3_class(curation_load_model(legacy_path), "planscanR_selection_model")
  expect_true(file.exists(legacy_path))
})

test_that("publication uses CAS and failed targets preserve the previous active model", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  root <- c3_test_paths()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  store <- curation_artifact_store_sqlite(list(root = root))
  on.exit(store$close(), add = TRUE)
  scope <- c3_test_scope()
  trainer <- c3_test_actor("trainer")
  first <- curation_artifact_write_model(
    store, scope, c3_test_model("first"), c3_test_provenance(scope, trainer, "first"), trainer, 0L
  )
  second <- curation_artifact_write_model(
    store, scope, c3_test_model("second"), c3_test_provenance(scope, trainer, "second"), trainer, 0L
  )
  expect_true(isTRUE(curation_artifact_publish_model(
    store, scope, first$model_id, trainer,
    expected_active_revision = 0L
  )$ok))
  stale <- tryCatch(
    curation_artifact_publish_model(
      store, scope, second$model_id, trainer,
      expected_active_revision = 0L
    ),
    error = identity
  )
  expect_s3_class(stale, "planscanR_error_curation_conflict")
  expect_identical(curation_artifact_active_model(store, scope, trainer)$active_model_id, first$model_id)

  DBI::dbExecute(
    store$connection,
    paste(
      "CREATE TRIGGER c3_abort_active_update BEFORE UPDATE ON",
      "curation_artifact_active_models BEGIN SELECT RAISE(ABORT,",
      "'forced active publication failure'); END"
    )
  )
  expect_error(
    curation_artifact_publish_model(
      store, scope, second$model_id, trainer,
      expected_active_revision = 1L
    ),
    "forced active publication failure"
  )
  DBI::dbExecute(store$connection, "DROP TRIGGER c3_abort_active_update")
  rolled_back <- curation_artifact_active_model(store, scope, trainer)
  expect_identical(rolled_back$active_model_id, first$model_id)
  expect_identical(rolled_back$active_revision, 1L)
  expect_identical(
    curation_artifact_read_model(store, scope, rolled_back$active_model_id, trainer)$payload$fixture_tag,
    "first"
  )

  expect_error(
    curation_artifact_publish_model(
      store, scope, "missing-model", trainer,
      expected_active_revision = 1L
    ),
    class = "planscanR_error_curation_artifact"
  )
  expect_identical(curation_artifact_active_model(store, scope, trainer)$active_model_id, first$model_id)

  second_path <- curation_artifact_read_model(store, scope, second$model_id, trainer)$path
  saveRDS(c3_test_model("tampered"), second_path)
  expect_error(
    curation_artifact_read_model(store, scope, second$model_id, trainer),
    class = "planscanR_error_curation_artifact"
  )
  saveRDS(c3_test_model("second"), second_path)
  writeLines("malformed", second_path)
  expect_error(
    curation_artifact_publish_model(
      store, scope, second$model_id, trainer,
      expected_active_revision = 1L
    ),
    class = "planscanR_error_curation_artifact"
  )
  expect_identical(curation_artifact_active_model(store, scope, trainer)$active_model_id, first$model_id)
})

test_that("independent R processes cannot both publish a stale active revision", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("pkgload")
  root <- c3_test_paths()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  db <- file.path(root, "artifacts.sqlite")
  barrier <- file.path(root, "barrier")
  script <- file.path(root, "worker.R")
  outputs <- file.path(root, c("a.out", "b.out"))
  package_root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "package_root <- args[[1L]]; db <- args[[2L]]; barrier <- args[[3L]]; out <- args[[4L]]; id <- args[[5L]]",
    "result <- tryCatch({",
    "if (file.exists(file.path(package_root, 'R', 'curation_artifact_sqlite.R'))) pkgload::load_all(package_root, quiet = TRUE) else library('planscanR.screen', character.only = TRUE)",
    "store <- curation_artifact_store_sqlite(list(root = dirname(db), database = db), busy_timeout = 10000L)",
    "scope <- curation_scope('project-c3', 'corpus-c3', 'policy.v1')",
    "actor <- curation_actor(paste0('worker-', id), roles = 'reviewer')",
    "model <- structure(list(workflow = NULL, topics = c(signal = 'signal topic'), labels = c(keep = 'keep', drop = 'drop'), fixture_tag = id), class = 'planscanR_selection_model')",
    "provenance <- list(review_fingerprint = paste0('reviews-', id), snapshot_version = paste0('snapshot-', id), config_fingerprint = paste0('config-', id), corpus_snapshot = list(corpus_id = scope$corpus_id, version = paste0('corpus-', id)), training_parameters = list(seed = 42L, learner = 'fixture'), creator = actor$id)",
    "written <- curation_artifact_write_model(store, scope, model, provenance, actor, 0L)",
    "file.create(paste0(barrier, '-', id))",
    "deadline <- Sys.time() + 20",
    "while (!(file.exists(paste0(barrier, '-a')) && file.exists(paste0(barrier, '-b')))) { if (Sys.time() > deadline) stop('barrier timeout'); Sys.sleep(0.02) }",
    "published <- tryCatch(curation_artifact_publish_model(store, scope, written$model_id, actor, 0L), error = function(e) e)",
    "if (is.list(published) && isTRUE(published$ok)) 'ok' else if (inherits(published, 'error')) paste('error', conditionMessage(published)) else 'failed'",
    "}, error = function(error) paste('error', conditionMessage(error)))",
    "writeLines(result, out)",
    "if (exists('store') && inherits(store, 'planscanR_curation_artifact_store_sqlite')) store$close()"
  ), script)
  rscript <- file.path(R.home("bin"), "Rscript")
  pids <- lapply(seq_along(outputs), function(i) {
    system2(
      rscript,
      c("--vanilla", script, package_root, db, barrier, outputs[[i]], c("a", "b")[[i]]),
      stdout = FALSE,
      stderr = FALSE,
      wait = FALSE
    )
  })
  deadline <- Sys.time() + 30
  while (!all(file.exists(outputs))) {
    if (Sys.time() > deadline) stop("artifact publisher processes did not finish")
    Sys.sleep(0.05)
  }
  results <- unname(unlist(lapply(outputs, readLines)))
  expect_equal(sum(results == "ok"), 1L)
  expect_equal(
    sum(grepl("^error active model revision conflict: expected 0, current 1\\.$", results)),
    1L
  )
  store <- curation_artifact_store_sqlite(list(root = root, database = db))
  on.exit(store$close(), add = TRUE)
  active <- curation_artifact_active_model(
    store, c3_test_scope(), c3_test_actor("observer")
  )
  expect_identical(active$active_revision, 1L)
  expect_equal(nrow(curation_artifact_list_models(store, c3_test_scope(), c3_test_actor("observer"))), 2L)
})

test_that("independent sample writers cannot replace one explicit sample ID", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("pkgload")
  root <- c3_test_paths()
  on.exit(unlink(root, recursive = TRUE, force = TRUE), add = TRUE)
  db <- file.path(root, "sample-artifacts.sqlite")
  barrier <- file.path(root, "sample-barrier")
  script <- file.path(root, "sample-worker.R")
  outputs <- file.path(root, c("sample-a.out", "sample-b.out"))
  package_root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "package_root <- args[[1L]]; db <- args[[2L]]; barrier <- args[[3L]]; out <- args[[4L]]; id <- args[[5L]]",
    "store <- NULL",
    "result <- tryCatch({",
    "if (file.exists(file.path(package_root, 'R', 'curation_artifact_sqlite.R'))) pkgload::load_all(package_root, quiet = TRUE) else library('planscanR.screen', character.only = TRUE)",
    "store <- curation_artifact_store_sqlite(list(root = dirname(db), database = db), busy_timeout = 10000L)",
    "scope <- curation_scope('project-c3', 'corpus-c3', 'policy.v1')",
    "actor <- curation_actor('reviewer', roles = 'reviewer')",
    "sample <- structure(data.frame(country = 'nl', document_id = id), curation_sample_metadata = list(sample_id = 'shared-sample'))",
    "file.create(paste0(barrier, '-', id))",
    "deadline <- Sys.time() + 20",
    "while (!(file.exists(paste0(barrier, '-a')) && file.exists(paste0(barrier, '-b')))) { if (Sys.time() > deadline) stop('barrier timeout'); Sys.sleep(0.02) }",
    "written <- curation_artifact_write_sample(store, scope, sample, actor$id, actor, 0L)",
    "if (is.list(written) && isTRUE(written$ok)) paste('ok', id) else 'failed'",
    "}, error = function(error) paste('error', conditionMessage(error)))",
    "writeLines(result, out)",
    "if (inherits(store, 'planscanR_curation_artifact_store_sqlite')) store$close()"
  ), script)
  rscript <- file.path(R.home("bin"), "Rscript")
  lapply(seq_along(outputs), function(i) {
    system2(
      rscript,
      c("--vanilla", script, package_root, db, barrier, outputs[[i]], c("a", "b")[[i]]),
      stdout = FALSE,
      stderr = FALSE,
      wait = FALSE
    )
  })
  deadline <- Sys.time() + 30
  while (!all(file.exists(outputs))) {
    if (Sys.time() > deadline) stop("sample writer processes did not finish")
    Sys.sleep(0.05)
  }
  results <- unname(unlist(lapply(outputs, readLines)))
  expect_equal(sum(grepl("^ok ", results)), 1L)
  expect_equal(sum(grepl("^error ", results)), 1L)

  store <- curation_artifact_store_sqlite(list(root = root, database = db))
  on.exit(store$close(), add = TRUE)
  scope <- c3_test_scope()
  reviewer <- c3_test_actor("reviewer")
  expect_equal(nrow(curation_artifact_list_samples(store, scope, reviewer$id, reviewer)), 1L)
  read <- curation_artifact_read_sample(store, scope, "shared-sample", reviewer)
  winner <- sub("^ok ", "", results[grepl("^ok ", results)])
  expect_identical(
    read$payload,
    structure(
      data.frame(country = "nl", document_id = winner),
      curation_sample_metadata = list(sample_id = "shared-sample")
    )
  )
  expect_true(file.exists(read$path))
})


test_that("snapshot publication holds the writer lock and preserves generic immutable payloads", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  root <- withr::local_tempdir()
  store <- curation_artifact_store_sqlite(list(root = root))
  on.exit(store$close(), add = TRUE)
  other <- DBI::dbConnect(RSQLite::SQLite(), file.path(root, "curation-artifacts.sqlite"))
  on.exit(DBI::dbDisconnect(other), add = TRUE)
  DBI::dbExecute(other, "PRAGMA busy_timeout = 0")
  atomic_save <- get("curation_artifact_sqlite_atomic_save", asNamespace("planscanR.screen"))
  lock_checks <- 0L
  testthat::local_mocked_bindings(
    curation_artifact_sqlite_atomic_save = function(value, path) {
      lock_checks <<- lock_checks + 1L
      expect_error(DBI::dbExecute(other, "BEGIN IMMEDIATE"), "locked")
      atomic_save(value, path)
    },
    .package = "planscanR.screen"
  )
  scope <- c3_test_scope()
  actor <- c3_test_actor("alice")
  payload <- list(records = "a generic serializable payload")
  written <- curation_artifact_write_snapshot(store, scope, payload, actor, 0L)
  expect_error(curation_artifact_write_snapshot(store, scope, payload, actor, 0L),
    class = "planscanR_error_curation_conflict"
  )
  expect_identical(curation_artifact_read_snapshot(store, scope, actor)$payload, payload)
  writeLines("corrupt", written$path)
  repaired <- curation_artifact_write_snapshot(store, scope, payload, actor, 0L)
  expect_identical(repaired$version, written$version)
  expect_identical(curation_artifact_read_snapshot(store, scope, actor)$payload, payload)
  expect_identical(lock_checks, 3L)
})
