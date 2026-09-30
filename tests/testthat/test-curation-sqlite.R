test_that("SQLite review repository opens a WAL database", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE), add = TRUE)

  repo <- curation_review_repository_sqlite(db)
  expect_s3_class(repo, "planscanR_curation_review_repository_sqlite")
  expect_true(file.exists(db))
  expect_identical(repo$path, normalizePath(db, mustWork = FALSE))
  expect_identical(repo$journal_mode, "wal")
  expect_true(isTRUE(repo$foreign_keys))
  repo$close()
})

test_that("SQLite reviews support revisions, tombstones, and stale CAS errors", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE), add = TRUE)
  repo <- curation_review_repository_sqlite(db)
  scope <- curation_scope("campaign", "corpus", "policy.v1")
  actor <- curation_actor("alice", roles = "reviewer")
  decision <- data.frame(
    country = "nl", document_id = "doc-1", reviewer_id = "alice",
    decision = "keep", source = "browse", note = "first",
    reviewed_at = "2026-09-30T12:00:00Z", sidecar_path = "files/nl/doc-1.json",
    stringsAsFactors = FALSE
  )

  created <- curation_review_upsert(repo, scope, decision, actor, 0L)
  expect_true(isTRUE(created$ok))
  expect_identical(created$revision, 1L)
  authored <- curation_review_read(
    repo, scope, curation_review_key("nl", "doc-1", "alice"), actor
  )
  expect_identical(authored$decision, "keep")
  expect_error(
    curation_review_upsert(repo, scope, decision, actor, 0L),
    class = "planscanR_error_curation_conflict"
  )

  cleared <- curation_review_clear(
    repo, scope, curation_review_key("nl", "doc-1", "alice"), actor, 1L
  )
  expect_identical(cleared$revision, 2L)
  tombstone <- curation_review_read(repo, scope, curation_review_key("nl", "doc-1", "alice"), actor)
  expect_true(isTRUE(tombstone$deleted))
  expect_identical(tombstone$revision, 2L)
  expect_identical(tombstone$reviewed_at, authored$reviewed_at)
  expect_true(nzchar(tombstone$modified_at))
  expect_error(
    curation_review_upsert(repo, scope, decision, actor, 1L),
    class = "planscanR_error_curation_conflict"
  )
  recreated <- curation_review_upsert(repo, scope, decision, actor, 2L)
  expect_identical(recreated$revision, 3L)
  repo$close()
})

test_that("bulk review writes validate all revisions before changing any row", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE), add = TRUE)
  repo <- curation_review_repository_sqlite(db)
  scope <- curation_scope("campaign", "corpus", "policy.v1")
  actor <- curation_actor("alice", roles = "reviewer")
  rows <- data.frame(
    country = c("nl", "nl"),
    document_id = c("one", "two"),
    reviewer_id = "alice",
    decision = c("keep", "drop"),
    source = "bulk",
    note = c("one", "two"),
    stringsAsFactors = FALSE
  )
  first <- curation_review_bulk_upsert(repo, scope, rows, actor, c(0L, 0L))
  expect_true(isTRUE(first$ok))
  expect_identical(first$revision, c(1L, 1L))
  changed <- rows
  changed$decision <- c("drop", "keep")
  expect_error(
    curation_review_bulk_upsert(repo, scope, changed, actor, c(0L, 1L)),
    class = "planscanR_error_curation_conflict"
  )
  after <- curation_review_list(repo, scope, actor = actor)
  expect_identical(after$decision, c("keep", "drop"))
  expect_error(
    curation_review_bulk_upsert(repo, scope, rbind(rows, rows[1L, ]), actor, c(1L, 1L, 1L)),
    class = "planscanR_error_curation_duplicate"
  )
  repo$close()
})

test_that("trusted actor policy isolates reviewers and scopes while config remains provenance", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE), add = TRUE)
  repo <- curation_review_repository_sqlite(db)
  scope_a <- curation_scope("campaign", "corpus", "policy.a")
  scope_b <- curation_scope("campaign", "corpus", "policy.b")
  other_project <- curation_scope("other", "corpus", "policy.a")
  alice <- curation_actor("alice", roles = "reviewer", display_name = "Alice")
  bob <- curation_actor("bob", roles = "reviewer", display_name = "Alice")
  row <- data.frame(
    country = "nl", document_id = "same", reviewer_id = "alice",
    decision = "keep", source = "browse", stringsAsFactors = FALSE
  )
  curation_review_upsert(repo, scope_a, row, alice, 0L)
  expect_error(
    curation_review_upsert(repo, scope_a, row, bob, 1L),
    class = "planscanR_error_curation_authorization"
  )
  expect_equal(
    curation_review_read(repo, scope_b, curation_review_key("nl", "same", "alice"), alice)$decision,
    "keep"
  )
  expect_null(curation_review_read(repo, other_project, curation_review_key("nl", "same", "alice"), alice))
  expect_identical(
    curation_review_read(repo, scope_b, curation_review_key("nl", "same", "alice"), alice)$config_id,
    "policy.a"
  )
  viewer <- curation_actor("alice", roles = "viewer")
  no_role <- curation_actor("alice")
  expect_error(
    curation_review_upsert(repo, scope_a, row, viewer, 1L),
    class = "planscanR_error_curation_authorization"
  )
  expect_error(
    curation_review_upsert(repo, scope_a, row, no_role, 1L),
    class = "planscanR_error_curation_authorization"
  )
  repo$close()
})

test_that("import ledger makes replay a no-op after edits and tombstones", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE), add = TRUE)
  repo <- curation_review_repository_sqlite(db)
  scope <- curation_scope("campaign", "corpus", "policy.v1")
  importer <- curation_actor("migration", roles = "importer")
  reviewer <- curation_actor("legacy-user", roles = "reviewer")
  imported <- data.frame(
    country = "nl", document_id = "legacy-1", reviewer_id = "legacy-user",
    decision = "keep", source = "random", note = "line one\nline two",
    reviewed_at = "2026-09-01T10:00:00+02:00",
    sidecar_path = "files/nl/legacy-1/é.json",
    import_source = "biogain-legacy-v1", import_row_id = "nl::legacy-1::legacy-user",
    import_fingerprint = "fixture-fingerprint", stringsAsFactors = FALSE
  )
  first <- curation_review_import(repo, scope, imported, importer, 0L)
  expect_identical(first$revision, 1L)
  edited <- imported
  edited$reviewer_id <- "legacy-user"
  edited$decision <- "drop"
  edited$note <- "edited live"
  edited$reviewed_at <- "2026-09-02T10:00:00Z"
  curation_review_upsert(repo, scope, edited[, c("country", "document_id", "reviewer_id", "decision", "source", "note", "reviewed_at", "sidecar_path")], reviewer, 1L)
  replay <- curation_review_import(repo, scope, imported, importer, 0L)
  expect_true(isTRUE(replay$results$no_op[[1L]]))
  expect_identical(replay$revision, 2L)
  expect_identical(
    curation_review_read(repo, scope, curation_review_key("nl", "legacy-1", "legacy-user"), reviewer)$note,
    "edited live"
  )
  curation_review_clear(repo, scope, curation_review_key("nl", "legacy-1", "legacy-user"), reviewer, 2L)
  replay_after_clear <- curation_review_import(repo, scope, imported, importer, 0L)
  expect_true(isTRUE(replay_after_clear$results$no_op[[1L]]))
  expect_identical(replay_after_clear$revision, 3L)
  changed <- imported
  changed$decision <- "drop"
  expect_error(
    curation_review_import(repo, scope, changed, importer, 0L),
    class = "planscanR_error_curation_import_conflict"
  )
  repo$close()
})

test_that("CSV export retains Unicode, newlines, import provenance, and tombstones", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE), add = TRUE)
  repo <- curation_review_repository_sqlite(db)
  scope <- curation_scope("campaign", "corpus", "policy.v1")
  actor <- curation_actor("alice", roles = "reviewer")
  row <- data.frame(
    country = "nl", document_id = "unicode", reviewer_id = "alice",
    decision = "keep", source = "browse", note = "café\nnewline",
    sidecar_path = "files/nl/unicode/é.json", stringsAsFactors = FALSE
  )
  curation_review_upsert(repo, scope, row, actor, 0L)
  curation_review_clear(repo, scope, curation_review_key("nl", "unicode", "alice"), actor, 1L)
  csv <- curation_review_export(repo, scope, actor, "csv")
  expect_true(is.data.frame(csv))
  expect_true(all(c("config_id", "import_source", "modified_at", "revision", "deleted") %in% names(csv)))
  expect_true(isTRUE(csv$deleted[[1L]]))
  path <- tempfile(fileext = ".csv")
  repo$export_csv(scope, actor, path)
  roundtrip <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  expect_identical(roundtrip$sidecar_path[[1L]], "files/nl/unicode/é.json")
  expect_true(file.exists(path))
  expect_error(
    repo$export_csv(scope, NULL),
    class = "planscanR_error_curation_actor"
  )
  expect_error(repo$export_csv(scope, actor, repo$path))
  expect_error(repo$export_csv(scope, actor, paste0(repo$path, "-wal")))
  unlink(path)
  repo$close()
})

test_that("future schema versions are rejected before use", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE), add = TRUE)
  repo <- curation_review_repository_sqlite(db)
  repo$close()
  conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = db)
  DBI::dbExecute(conn, "PRAGMA user_version = 999")
  DBI::dbDisconnect(conn)
  expect_error(curation_review_repository_sqlite(db), class = "planscanR_error_curation_schema")
})

test_that("bounded busy handling recovers after an interrupted writer transaction", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE), add = TRUE)
  first <- curation_review_repository_sqlite(db, busy_timeout = 50L)
  second <- curation_review_repository_sqlite(db, busy_timeout = 50L)
  scope <- curation_scope("campaign", "corpus", "policy.v1")
  actor <- curation_actor("alice", roles = "reviewer")
  row <- data.frame(
    country = "nl", document_id = "busy", reviewer_id = "alice",
    decision = "keep", source = "browse", stringsAsFactors = FALSE
  )
  DBI::dbExecute(first$connection, "BEGIN IMMEDIATE")
  expect_error(
    curation_review_upsert(second, scope, row, actor, 0L),
    class = "planscanR_error_curation_busy"
  )
  DBI::dbRollback(first$connection)
  expect_identical(curation_review_upsert(second, scope, row, actor, 0L)$revision, 1L)
  first$close()
  second$close()
})

test_that("an interrupt rolls back an open transaction before unwinding", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE), add = TRUE)
  repo <- curation_review_repository_sqlite(db)
  interrupted <- NULL
  withRestarts(
    abort_transaction = function(value = NULL) value,
    withCallingHandlers(
      curation_sqlite_with_transaction(
        repo$connection,
        function() {
          stop(structure(list(message = "synthetic interrupt"), class = c("interrupt", "condition")))
        },
        busy_timeout = 100L
      ),
      interrupt = function(condition) {
        interrupted <<- condition
        invokeRestart("abort_transaction")
      }
    )
  )
  expect_s3_class(interrupted, "interrupt")
  scope <- curation_scope("campaign", "corpus", "policy.v1")
  actor <- curation_actor("alice", roles = "reviewer")
  row <- data.frame(
    country = "nl", document_id = "after-interrupt", reviewer_id = "alice",
    decision = "keep", source = "browse", stringsAsFactors = FALSE
  )
  expect_identical(curation_review_upsert(repo, scope, row, actor, 0L)$revision, 1L)
  repo$close()
})

test_that("independent R processes commit distinct reviews without lost decisions", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  barrier <- tempfile("planscanR-barrier-")
  worker <- tempfile(fileext = ".R")
  output_a <- tempfile("planscanR-worker-a-")
  output_b <- tempfile("planscanR-worker-b-")
  on.exit(unlink(c(
    db, paste0(db, "-wal"), paste0(db, "-shm"), barrier,
    paste0(barrier, "-a"), paste0(barrier, "-b"), worker, output_a, output_b
  ), force = TRUE), add = TRUE)
  root_candidates <- c(
    testthat::test_path("..", ".."),
    testthat::test_path("..", "..", "planscanR.screen")
  )
  source_roots <- root_candidates[
    vapply(
      root_candidates,
      function(path) file.exists(file.path(path, "R", "curation_sqlite.R")),
      logical(1)
    )
  ]
  root <- if (length(source_roots) > 0L) {
    normalizePath(source_roots[[1L]], mustWork = TRUE)
  } else {
    "__installed_namespace__"
  }
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "root <- args[[1L]]; db <- args[[2L]]; barrier <- args[[3L]]; output <- args[[4L]]; id <- args[[5L]]",
    "worker <- new.env(parent = globalenv())",
    "if (nzchar(root) && !identical(root, '__installed_namespace__')) {",
    "  source(file.path(root, 'R', 'curation_config.R'), local = worker)",
    "  source(file.path(root, 'R', 'curation_reviews.R'), local = worker)",
    "  source(file.path(root, 'R', 'curation_sqlite.R'), local = worker)",
    "  get_worker <- function(name) get(name, envir = worker, inherits = FALSE)",
    "} else if (requireNamespace('planscanR.screen', quietly = TRUE)) {",
    "  package_namespace <- asNamespace('planscanR.screen')",
    "  get_worker <- function(name) get(name, envir = package_namespace, inherits = FALSE)",
    "} else {",
    "  stop('planscanR.screen source and installed namespace are unavailable')",
    "}",
    "get_worker('curation_review_repository_sqlite')(db, busy_timeout = 5000L) -> repo",
    "invisible(file.create(paste0(barrier, '-', id)))",
    "deadline <- Sys.time() + 15",
    "while (!(file.exists(paste0(barrier, '-a')) && file.exists(paste0(barrier, '-b')))) { if (Sys.time() > deadline) stop('barrier timeout'); Sys.sleep(0.02) }",
    "scope <- get_worker('curation_scope')('campaign', 'corpus', 'policy.v1')",
    "actor <- get_worker('curation_actor')(id, roles = 'reviewer')",
    "row <- data.frame(country = 'nl', document_id = paste0('process-', id), reviewer_id = id, decision = id, source = 'process', stringsAsFactors = FALSE)",
    "result <- get_worker('curation_review_upsert')(repo, scope, row, actor, 0L)",
    "writeLines(if (isTRUE(result$ok)) 'ok' else 'failed', output)",
    "repo$close()"
  ), worker)
  rscript <- file.path(R.home("bin"), "Rscript")
  system2(rscript, c("--vanilla", worker, root, db, barrier, output_a, "a"), wait = FALSE)
  system2(rscript, c("--vanilla", worker, root, db, barrier, output_b, "b"), wait = FALSE)
  deadline <- Sys.time() + 30
  repeat {
    if (file.exists(output_a) && file.exists(output_b)) break
    if (Sys.time() > deadline) stop("process workers did not finish before the deadline")
    Sys.sleep(0.05)
  }
  expect_identical(readLines(output_a), "ok")
  expect_identical(readLines(output_b), "ok")
  repo <- curation_review_repository_sqlite(db)
  scope <- curation_scope("campaign", "corpus", "policy.v1")
  actor <- curation_actor("observer", roles = "reviewer")
  rows <- curation_review_list(repo, scope, actor = actor)
  expect_setequal(rows$document_id, c("process-a", "process-b"))
  expect_setequal(rows$decision, c("a", "b"))
  repo$close()
})

test_that("a killed writer leaves no partial review or import ledger transaction", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  worker <- tempfile(fileext = ".R")
  pid_file <- tempfile("planscanR-killed-pid-")
  ready <- tempfile("planscanR-killed-ready-")
  child_log <- tempfile("planscanR-killed-log-")
  on.exit(unlink(c(
    db, paste0(db, "-wal"), paste0(db, "-shm"), worker, pid_file, ready, child_log
  ), force = TRUE), add = TRUE)
  root_candidates <- c(
    testthat::test_path("..", ".."),
    testthat::test_path("..", "..", "planscanR.screen")
  )
  source_roots <- root_candidates[
    vapply(
      root_candidates,
      function(path) file.exists(file.path(path, "R", "curation_sqlite.R")),
      logical(1)
    )
  ]
  root <- if (length(source_roots) > 0L) {
    normalizePath(source_roots[[1L]], mustWork = TRUE)
  } else {
    "__installed_namespace__"
  }
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "root <- args[[1L]]; db <- args[[2L]]; pid_file <- args[[3L]]; ready <- args[[4L]]",
    "worker <- new.env(parent = globalenv())",
    "if (nzchar(root) && !identical(root, '__installed_namespace__')) {",
    "  source(file.path(root, 'R', 'curation_config.R'), local = worker)",
    "  source(file.path(root, 'R', 'curation_reviews.R'), local = worker)",
    "  source(file.path(root, 'R', 'curation_sqlite.R'), local = worker)",
    "  get_worker <- function(name) get(name, envir = worker, inherits = FALSE)",
    "} else if (requireNamespace('planscanR.screen', quietly = TRUE)) {",
    "  package_namespace <- asNamespace('planscanR.screen')",
    "  get_worker <- function(name) get(name, envir = package_namespace, inherits = FALSE)",
    "} else {",
    "  stop('planscanR.screen source and installed namespace are unavailable')",
    "}",
    "repo <- get_worker('curation_review_repository_sqlite')(db, busy_timeout = 5000L)",
    "DBI::dbExecute(repo$connection, 'BEGIN IMMEDIATE')",
    "DBI::dbExecute(repo$connection, paste('INSERT INTO curation_reviews (project_id, corpus_id, config_id, country, document_id, reviewer_id, decision, source, note, reviewed_at, sidecar_path, revision, deleted, import_source, import_row_id, import_fingerprint, modified_at)', 'VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)'), params = list('campaign', 'corpus', 'policy.v1', 'nl', 'killed', 'legacy-user', 'keep', 'import', 'before kill', '2026-09-30T10:00:00Z', NA_character_, 1L, 0L, 'kill-source', 'kill-row', 'payload', '2026-09-30T10:00:01Z'))",
    "DBI::dbExecute(repo$connection, paste('INSERT INTO curation_import_ledger (project_id, corpus_id, import_source, import_row_id, country, document_id, reviewer_id, payload_fingerprint, import_fingerprint, config_id, imported_at)', 'VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)'), params = list('campaign', 'corpus', 'kill-source', 'kill-row', 'nl', 'killed', 'legacy-user', 'payload', 'source-fingerprint', 'policy.v1', '2026-09-30T10:00:01Z'))",
    "writeLines(as.character(Sys.getpid()), pid_file)",
    "invisible(file.create(ready))",
    "repeat Sys.sleep(1)"
  ), worker)
  rscript <- file.path(R.home("bin"), "Rscript")
  system2(
    rscript,
    c("--vanilla", worker, root, db, pid_file, ready),
    stdout = child_log,
    stderr = child_log,
    wait = FALSE
  )
  deadline <- Sys.time() + 15
  repeat {
    if (file.exists(ready) && file.exists(pid_file)) break
    if (Sys.time() > deadline) stop("killed-transaction worker did not become ready")
    Sys.sleep(0.05)
  }
  tools::pskill(as.integer(readLines(pid_file)), signal = 9L)
  repo <- NULL
  deadline <- Sys.time() + 15
  repeat {
    attempt <- tryCatch(
      curation_review_repository_sqlite(db, busy_timeout = 100L),
      error = function(error) error
    )
    if (!inherits(attempt, "error")) {
      repo <- attempt
      break
    }
    if (Sys.time() > deadline) stop("database did not recover after killed writer")
    Sys.sleep(0.05)
  }
  scope <- curation_scope("campaign", "corpus", "policy.v1")
  actor <- curation_actor("observer", roles = "reviewer")
  expect_null(curation_review_read(
    repo, scope, curation_review_key("nl", "killed", "legacy-user"), actor
  ))
  expect_identical(
    DBI::dbGetQuery(repo$connection, "SELECT COUNT(*) AS n FROM curation_import_ledger")$n,
    0L
  )
  repo$close()
})
