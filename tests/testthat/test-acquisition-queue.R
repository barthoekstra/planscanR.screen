skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

queue_catalog <- function() {
  # Keep the path alive for callbacks that intentionally open a second
  # independent connection while the worker handler is running.
  acquisition_catalog_open(tempfile(fileext = ".sqlite"))
}

queue_source_document <- function(catalog, host = "portal.example") {
  source <- acquisition_catalog_register_source(
    catalog,
    country = "nl",
    portal = host,
    native_project_id = "project-1",
    source_url = paste0("https://", host, "/project-1")
  )
  document <- acquisition_catalog_register_document(
    catalog,
    source_ref_id = source$source_ref_id,
    native_attachment_id = "attachment-1",
    attachment_url = paste0("https://", host, "/project-1/file.pdf")
  )
  list(source = source, document = document)
}

test_that("enqueue is durable and idempotent by persisted target identity", {
  catalog <- queue_catalog()
  refs <- queue_source_document(catalog)

  first <- acquisition_queue_enqueue(
    catalog,
    kind = "download",
    target_type = "document",
    target_ref_id = refs$document$document_ref_id,
    host = "portal.example",
    max_attempts = 2L
  )
  replay <- acquisition_queue_enqueue(
    catalog,
    kind = "download",
    target_type = "document",
    target_ref_id = refs$document$document_ref_id,
    host = "portal.example",
    max_attempts = 2L
  )

  expect_identical(first$status, "inserted")
  expect_identical(replay$status, "replayed")
  expect_identical(replay$job_id, first$job_id)
  expect_equal(nrow(acquisition_queue_jobs(catalog)), 1L)
  expect_equal(nrow(acquisition_queue_attempts(catalog)), 0L)
  expect_error(
    acquisition_queue_enqueue(
      catalog,
      kind = "download",
      target_type = "document",
      target_ref_id = refs$document$document_ref_id,
      host = "other.example",
      idempotency_key = "different-key"
    ),
    class = "planscanR_error_acquisition_queue"
  )
  expect_error(
    acquisition_queue_enqueue(
      catalog,
      kind = "discover",
      target_type = "source",
      target_ref_id = refs$source$source_ref_id,
      idempotency_key = first$idempotency_key
    ),
    class = "planscanR_error_acquisition_queue_conflict"
  )
  catalog$close()
})

test_that("claims are fenced by unique tokens and lease expiry recovery", {
  catalog <- queue_catalog()
  refs <- queue_source_document(catalog)
  job <- acquisition_queue_enqueue(
    catalog,
    kind = "download",
    target_type = "document",
    target_ref_id = refs$document$document_ref_id,
    host = "portal.example",
    max_attempts = 3L
  )

  first <- acquisition_queue_claim(catalog, "worker-a", lease_seconds = 1L)
  expect_true(first$ok)
  expect_false(acquisition_queue_claim(catalog, "worker-b", lease_seconds = 1L)$ok)
  expect_true(acquisition_queue_heartbeat(
    catalog, first$job_id, first$lease_token,
    lease_seconds = 2L
  )$ok)
  expect_false(acquisition_queue_complete(
    catalog, first$job_id, "wrong-token",
    status = "succeeded"
  )$ok)

  DBI::dbExecute(
    catalog$conn,
    "UPDATE acquisition_jobs SET lease_expires_at = ? WHERE job_id = ?",
    params = list("2000-01-01T00:00:00Z", job$job_id)
  )
  second <- acquisition_queue_claim(catalog, "worker-b", lease_seconds = 60L)
  expect_true(second$ok)
  expect_false(identical(first$lease_token, second$lease_token))
  stale <- acquisition_queue_complete(
    catalog, first$job_id, first$lease_token,
    status = "succeeded"
  )
  expect_false(stale$ok)
  expect_identical(stale$status, "stale")
  done <- acquisition_queue_complete(
    catalog, second$job_id, second$lease_token,
    status = "succeeded"
  )
  expect_true(done$ok)
  expect_identical(
    DBI::dbGetQuery(catalog$conn, "SELECT state FROM acquisition_jobs WHERE job_id = ?", params = list(job$job_id))$state,
    "succeeded"
  )
  attempts <- acquisition_queue_attempts(catalog)
  expect_equal(nrow(attempts), 2L)
  expect_identical(attempts$outcome[[1L]], "expired")
  catalog$close()
})

test_that("retry state and shared host concurrency/cooldown survive reopen", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  catalog <- acquisition_catalog_open(path)
  refs <- queue_source_document(catalog)
  acquisition_queue_set_host_limit(catalog, "portal.example", max_concurrency = 1L)
  first_job <- acquisition_queue_enqueue(
    catalog, "download", "document", refs$document$document_ref_id,
    host = "portal.example", max_attempts = 2L
  )
  second_document <- acquisition_catalog_register_document(
    catalog, refs$source$source_ref_id,
    native_attachment_id = "attachment-2",
    attachment_url = "https://portal.example/project-1/file-2.pdf"
  )
  second_job <- acquisition_queue_enqueue(
    catalog, "download", "document", second_document$document_ref_id,
    host = "portal.example", max_attempts = 2L
  )
  claimed <- acquisition_queue_claim(catalog, "worker-a", lease_seconds = 60L)
  expect_identical(claimed$job_id, first_job$job_id)
  expect_false(acquisition_queue_claim(catalog, "worker-b", lease_seconds = 60L)$ok)
  retry <- acquisition_queue_complete(
    catalog,
    claimed$job_id,
    claimed$lease_token,
    status = "retryable",
    error_category = "http-429",
    retry_after_seconds = 60L,
    cooldown_seconds = 60L
  )
  expect_true(retry$ok)
  closed <- catalog$close()

  reopened <- acquisition_catalog_open(path)
  expect_true(isTRUE(closed))
  rows <- acquisition_queue_jobs(reopened)
  expect_identical(rows$state[match(first_job$job_id, rows$job_id)], "retry_wait")
  expect_identical(rows$state[match(second_job$job_id, rows$job_id)], "queued")
  expect_equal(acquisition_queue_attempts(reopened)$outcome, "retryable")
  expect_false(acquisition_queue_claim(reopened, "worker-c", lease_seconds = 60L)$ok)
  DBI::dbExecute(
    reopened$conn,
    "UPDATE acquisition_host_limits SET cooldown_until = ? WHERE host = ?",
    params = list("2000-01-01T00:00:00Z", "portal.example")
  )
  DBI::dbExecute(
    reopened$conn,
    "UPDATE acquisition_jobs SET available_at = ? WHERE job_id = ?",
    params = list("2000-01-01T00:00:00Z", first_job$job_id)
  )
  next_claim <- acquisition_queue_claim(reopened, "worker-c", lease_seconds = 60L)
  expect_true(next_claim$ok)
  expect_identical(next_claim$job_id, first_job$job_id)
  reopened$close()
})

test_that("verified migration batches do not consume the finite failure retry budget", {
  catalog <- queue_catalog()
  admin <- curation_actor("admin", roles = "admin")
  source_root <- withr::local_tempdir()
  target_root <- withr::local_tempdir()
  acquisition_storage_register_root(catalog, "source", source_root, admin)
  acquisition_storage_register_root(catalog, "target", target_root, admin, purpose = "staging")
  relocation <- acquisition_storage_enqueue_relocation(catalog, "source", "target", admin,
    estimated_bytes = 1024)
  DBI::dbExecute(catalog$conn,
    "UPDATE acquisition_jobs SET max_attempts = 1, available_at = ? WHERE job_id = ?",
    params = list("2000-01-01T00:00:00Z", relocation$job_id))

  progress <- acquisition_queue_claim(catalog, "worker-progress", lease_seconds = 60L)
  continued <- acquisition_queue_complete(catalog, progress$job_id, progress$lease_token,
    status = "retryable", error_category = "migration-batch-continue",
    retry_after_seconds = 0)
  expect_identical(continued$job_state, "retry_wait")
  DBI::dbExecute(catalog$conn,
    "UPDATE acquisition_jobs SET available_at = ? WHERE job_id = ?",
    params = list("2000-01-01T00:00:00Z", relocation$job_id))

  failure <- acquisition_queue_claim(catalog, "worker-failure", lease_seconds = 60L)
  failed <- acquisition_queue_complete(catalog, failure$job_id, failure$lease_token,
    status = "retryable", error_category = "copy-verification-failed",
    retry_after_seconds = 0)
  expect_identical(failed$job_state, "failed")
  catalog$close()
})

test_that("expired migration leases consume the bounded failure retry budget", {
  catalog <- queue_catalog()
  admin <- curation_actor("admin", roles = "admin")
  acquisition_storage_register_root(catalog, "source", withr::local_tempdir(), admin)
  acquisition_storage_register_root(catalog, "target", withr::local_tempdir(), admin,
    purpose = "staging")
  relocation <- acquisition_storage_enqueue_relocation(catalog, "source", "target", admin,
    estimated_bytes = 1024)
  DBI::dbExecute(catalog$conn,
    "UPDATE acquisition_jobs SET max_attempts = 2, available_at = ? WHERE job_id = ?",
    params = list("2000-01-01T00:00:00Z", relocation$job_id))
  first <- acquisition_queue_claim(catalog, "expired-worker-1", lease_seconds = 60L)
  DBI::dbExecute(catalog$conn, "UPDATE acquisition_jobs SET lease_expires_at = ? WHERE job_id = ?",
    params = list("2000-01-01T00:00:00Z", relocation$job_id))
  second <- acquisition_queue_claim(catalog, "expired-worker-2", lease_seconds = 60L)
  expect_true(second$ok)
  DBI::dbExecute(catalog$conn, "UPDATE acquisition_jobs SET lease_expires_at = ? WHERE job_id = ?",
    params = list("2000-01-01T00:00:00Z", relocation$job_id))
  exhausted <- acquisition_queue_claim(catalog, "expired-worker-3", lease_seconds = 60L)
  expect_false(exhausted$ok)
  expect_identical(acquisition_queue_jobs(catalog)$state[[1L]], "failed")
  catalog$close()
})

test_that("the worker runs injected handlers outside the transaction", {
  catalog <- queue_catalog()
  refs <- queue_source_document(catalog)
  acquisition_queue_enqueue(
    catalog,
    "discover",
    "source",
    refs$source$source_ref_id,
    host = "portal.example"
  )
  calls <- 0L
  result <- acquisition_worker_run(
    catalog,
    worker_id = "worker-test",
    max_jobs = 1L,
    handlers = list(discover = function(job, catalog) {
      calls <<- calls + 1L
      second <- acquisition_catalog_open(catalog$path)
      acquisition_catalog_transaction(second, function(conn) DBI::dbGetQuery(conn, "SELECT 1 AS ok"))
      second$close()
      list(
        status = "succeeded",
        payload = list(discovered = 1L),
        source_ref_id = refs$source$source_ref_id,
        documents = list(list(
          native_attachment_id = "attachment-from-discovery",
          attachment_url = "https://cdn.example/project-1/discovered.pdf",
          section = "generic",
          ordinal = 2L
        ))
      )
    })
  )
  expect_equal(calls, 1L)
  expect_identical(result[[1L]]$status, "succeeded")
  expect_identical(acquisition_queue_jobs(catalog)$state[[1L]], "succeeded")
  expect_equal(nrow(acquisition_catalog_documents(catalog, refs$source$source_ref_id)), 2L)
  expect_equal(nrow(acquisition_queue_jobs(catalog)), 2L)
  jobs <- acquisition_queue_jobs(catalog)
  expect_identical(jobs$host[jobs$kind == "discover"], "portal.example")
  expect_identical(jobs$host[jobs$kind == "download"], "cdn.example")
  expect_true(nzchar(acquisition_queue_attempts(catalog)$result_blob[[1L]]))
  catalog$close()
})

test_that("built-in discovery preserves an authoritative cached empty", {
  catalog <- queue_catalog()
  source <- acquisition_catalog_register_source(
    catalog,
    country = "nl",
    portal = "portal.example",
    native_project_id = "empty-project",
    source_url = "https://portal.example/empty-project",
    metadata = list(
      discovery_status = "empty",
      discovery_reason = "portal checked",
      discovery_checked_at = "2026-10-03T10:00:00Z"
    )
  )
  acquisition_queue_enqueue(catalog, "discover", "source", source$source_ref_id)
  result <- acquisition_worker_run(catalog, worker_id = "cached-discovery", max_jobs = 1L)
  expect_identical(result[[1L]]$status, "succeeded")
  expect_true(isTRUE(result[[1L]]$result$successful_empty))
  expect_identical(result[[1L]]$result$discovery_status, "empty")
  expect_identical(acquisition_queue_jobs(catalog)$state, "succeeded")
  expect_equal(nrow(acquisition_queue_jobs(catalog)), 1L)
  catalog$close()
})

test_that("the built-in downloader uses a collision-safe locked destination", {
  root <- withr::local_tempdir()
  withr::local_options(list(planscanR.cache_dir = root))
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  fake_download <- function(url, country, document_id, cache_dir, overwrite = FALSE) {
    calls$n <- calls$n + 1L
    destination <- planscanR::cache_path(url, country, document_id, cache_dir = cache_dir)
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    existed <- file.exists(destination)
    if (!existed) writeBin(charToRaw("offline fixture"), destination)
    sha <- unclass(as.character(openssl::sha256(readBin(destination, "raw", n = file.info(destination)$size))))
    planscanR::reconcile_cached_download(destination, expected_size_bytes = file.info(destination)$size, expected_sha256 = sha, url = url, repair = TRUE)
    tibble::tibble(
      url = url,
      local_path = destination,
      size_bytes = file.info(destination)$size,
      sha256 = unclass(as.character(openssl::sha256(readBin(destination, "raw", n = file.info(destination)$size)))),
      status = if (existed) "exists" else "downloaded",
      reason = NA_character_
    )
  }
  testthat::local_mocked_bindings(download_to_cache = fake_download, .package = "planscanR")
  catalog <- queue_catalog()
  refs <- queue_source_document(catalog)
  job <- acquisition_queue_enqueue(
    catalog,
    "download",
    "document",
    refs$document$document_ref_id
  )
  result <- acquisition_worker_run(catalog, worker_id = "built-in", max_jobs = 1L)
  expect_identical(result[[1L]]$status, "succeeded")
  expect_equal(calls$n, 1L)
  files <- acquisition_catalog_files(catalog, refs$document$document_ref_id)
  expect_identical(files$verification_status, "verified")
  expect_true(grepl(refs$source$source_ref_id, files$relative_path, fixed = TRUE))
  expect_identical(acquisition_queue_jobs(catalog)$state, "succeeded")
  catalog$close()
})

test_that("concurrent stale publication keeps one canonical file", {
  db <- withr::local_tempfile(fileext = ".sqlite")
  cache <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(db)
  # The production queue binds the downloader to a registered storage root.
  # Keep this subprocess fixture on the same root as its mocked publication so
  # the reservation-derived transfer ceiling is finite in both workers.
  acquisition_storage_register_root(
    catalog,
    "00-test-downloads",
    cache,
    curation_actor("test-admin", roles = "admin"),
    reserve_bytes = 1L,
    reserve_fraction = 0
  )
  refs <- queue_source_document(catalog)
  job <- acquisition_queue_enqueue(catalog, "download", "document", refs$document$document_ref_id)
  worker <- withr::local_tempfile(fileext = ".R")
  ready <- withr::local_tempfile()
  writes <- withr::local_tempfile()
  outputs <- c(withr::local_tempfile(), withr::local_tempfile())
  on.exit(unlink(c(worker, ready, writes, outputs, db, paste0(db, c("-wal", "-shm"))), force = TRUE), add = TRUE)
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "root <- args[[1L]]; db <- args[[2L]]; cache <- args[[3L]]; output <- args[[4L]]; ready <- args[[5L]]; worker_id <- args[[6L]]; writes <- args[[7L]]",
    "source(file.path(root, 'R', 'utils_screen.R'), local = globalenv())",
    "source(file.path(root, 'R', 'acquisition_catalog.R'), local = globalenv())",
    "source(file.path(root, 'R', 'acquisition_queue.R'), local = globalenv())",
    "source(file.path(root, 'R', 'acquisition_storage.R'), local = globalenv())",
    "testthat::local_mocked_bindings(download_to_cache = function(url, country, document_id, cache_dir, overwrite = FALSE) {",
    "  destination <- planscanR::cache_path(url, country, document_id, cache_dir = cache_dir)",
    "  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)",
    "  existed <- file.exists(destination)",
    "  if (!existed) { writeBin(charToRaw('offline concurrent fixture'), destination); cat('new-publication\\n', file = writes, append = TRUE) }",
    "  sha <- unclass(as.character(openssl::sha256(readBin(destination, 'raw', n = file.info(destination)$size))))",
    "  planscanR::reconcile_cached_download(destination, expected_size_bytes = file.info(destination)$size, expected_sha256 = sha, url = url, repair = TRUE)",
    "  Sys.sleep(0.8)",
    "  tibble::tibble(url = url, local_path = destination, size_bytes = file.info(destination)$size, sha256 = sha, status = if (existed) 'exists' else 'downloaded', reason = NA_character_)",
    "}, .package = 'planscanR')",
    "options(planscanR.cache_dir = cache)",
    "catalog <- acquisition_catalog_open(db)",
    "claim <- acquisition_queue_claim(catalog, worker_id, lease_seconds = 2L)",
    "if (!isTRUE(claim$ok)) stop('publication worker did not claim')",
    "if (worker_id == 'publisher-a') writeLines('ready', ready)",
    "result <- acquisition_queue_default_download(claim, catalog, lock_timeout = 10)",
    "done <- acquisition_queue_complete(catalog, claim$job_id, claim$lease_token, status = result$status, result = result)",
    "writeLines(c(result$status, done$status), output)",
    "catalog$close()"
  ), worker)
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  system2(rscript, c("--vanilla", worker, root, db, cache, outputs[[1L]], ready, "publisher-a", writes), wait = FALSE)
  deadline <- Sys.time() + 15
  while (!file.exists(ready)) {
    if (Sys.time() > deadline) stop("first publication worker did not start")
    Sys.sleep(0.05)
  }
  DBI::dbExecute(
    catalog$conn,
    "UPDATE acquisition_jobs SET lease_expires_at = ? WHERE job_id = ?",
    params = list("2000-01-01T00:00:00Z", job$job_id)
  )
  system2(rscript, c("--vanilla", worker, root, db, cache, outputs[[2L]], ready, "publisher-b", writes), wait = FALSE)
  while (!all(file.exists(outputs))) {
    if (Sys.time() > deadline + 15) stop("publication workers did not finish")
    Sys.sleep(0.05)
  }
  expect_true(all(vapply(outputs, function(path) identical(readLines(path)[[1L]], "succeeded"), logical(1))))
  expect_length(readLines(writes), 1L)
  files <- acquisition_catalog_files(catalog, refs$document$document_ref_id)
  expect_equal(nrow(files), 1L)
  expect_identical(files$verification_status, "verified")
  expect_identical(readBin(file.path(cache, files$relative_path), "raw", n = file.info(file.path(cache, files$relative_path))$size), charToRaw("offline concurrent fixture"))
  expect_identical(acquisition_queue_jobs(catalog)$state, "succeeded")
  expect_setequal(acquisition_queue_attempts(catalog)$outcome, c("expired", "succeeded"))
  catalog$close()
})

test_that("worker renews a blocking handler past its lease", {
  catalog <- queue_catalog()
  refs <- queue_source_document(catalog)
  job <- acquisition_queue_enqueue(catalog, "discover", "source", refs$source$source_ref_id)
  observer <- withr::local_tempfile(fileext = ".R")
  observer_output <- withr::local_tempfile()
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "root <- args[[1L]]; db <- args[[2L]]; job_id <- args[[3L]]; output <- args[[4L]]",
    "source(file.path(root, 'R', 'acquisition_catalog.R'), local = globalenv())",
    "source(file.path(root, 'R', 'acquisition_queue.R'), local = globalenv())",
    "Sys.sleep(1.2)",
    "catalog <- acquisition_catalog_open(db)",
    "claim <- acquisition_queue_claim(catalog, 'observer', lease_seconds = 1L)",
    "writeLines(if (isTRUE(claim$ok)) 'claimed' else 'unclaimed', output)",
    "catalog$close()"
  ), observer)
  result <- acquisition_worker_run(
    catalog,
    worker_id = "long-handler-worker",
    max_jobs = 1L,
    lease_seconds = 1L,
    handlers = list(discover = function(job, catalog) {
      system2(
        rscript,
        c("--vanilla", observer, root, catalog$path, job$job_id, observer_output),
        stdout = FALSE,
        stderr = FALSE,
        wait = FALSE
      )
      Sys.sleep(2)
      list(status = "succeeded")
    })
  )
  deadline <- Sys.time() + 5
  while (!file.exists(observer_output) && Sys.time() < deadline) Sys.sleep(0.05)
  expect_true(file.exists(observer_output))
  expect_identical(readLines(observer_output), "unclaimed")
  expect_identical(result[[1L]]$status, "succeeded")
  expect_identical(acquisition_queue_jobs(catalog)$state, "succeeded")
  expect_identical(acquisition_queue_attempts(catalog)$outcome, "succeeded")
  expect_identical(acquisition_queue_jobs(catalog)$job_id, job$job_id)
  catalog$close()
})

test_that("independent processes claim a job only once", {
  db <- withr::local_tempfile(fileext = ".sqlite")
  catalog <- acquisition_catalog_open(db)
  refs <- queue_source_document(catalog)
  acquisition_queue_enqueue(catalog, "download", "document", refs$document$document_ref_id)
  catalog$close()
  worker <- withr::local_tempfile(fileext = ".R")
  outputs <- c(withr::local_tempfile(), withr::local_tempfile())
  on.exit(unlink(c(worker, outputs, db, paste0(db, c("-wal", "-shm"))), force = TRUE), add = TRUE)
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "root <- args[[1L]]; db <- args[[2L]]; output <- args[[3L]]; id <- args[[4L]]",
    "source(file.path(root, 'R', 'acquisition_catalog.R'), local = globalenv())",
    "source(file.path(root, 'R', 'acquisition_queue.R'), local = globalenv())",
    "catalog <- acquisition_catalog_open(db)",
    "claimed <- acquisition_queue_claim(catalog, id, lease_seconds = 5L)",
    "writeLines(if (isTRUE(claimed$ok)) claimed$lease_token else 'none', output)",
    "catalog$close()"
  ), worker)
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  system2(rscript, c("--vanilla", worker, root, db, outputs[[1L]], "a"), wait = FALSE)
  system2(rscript, c("--vanilla", worker, root, db, outputs[[2L]], "b"), wait = FALSE)
  deadline <- Sys.time() + 20
  while (!all(file.exists(outputs))) {
    if (Sys.time() > deadline) stop("queue claim workers did not finish")
    Sys.sleep(0.05)
  }
  tokens <- unlist(lapply(outputs, readLines), use.names = FALSE)
  expect_equal(sum(tokens != "none"), 1L)
})

test_that("a killed worker lease is recovered by a fresh process", {
  db <- withr::local_tempfile(fileext = ".sqlite")
  catalog <- acquisition_catalog_open(db)
  refs <- queue_source_document(catalog)
  job <- acquisition_queue_enqueue(
    catalog, "download", "document", refs$document$document_ref_id,
    host = "portal.example", max_attempts = 3L
  )
  catalog$close()
  worker <- withr::local_tempfile(fileext = ".R")
  output <- withr::local_tempfile()
  pid_file <- withr::local_tempfile()
  ready <- withr::local_tempfile()
  on.exit(unlink(c(worker, output, pid_file, ready, db, paste0(db, c("-wal", "-shm"))), force = TRUE), add = TRUE)
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "root <- args[[1L]]; db <- args[[2L]]; output <- args[[3L]]; pid_file <- args[[4L]]; ready <- args[[5L]]",
    "source(file.path(root, 'R', 'acquisition_catalog.R'), local = globalenv())",
    "source(file.path(root, 'R', 'acquisition_queue.R'), local = globalenv())",
    "catalog <- acquisition_catalog_open(db)",
    "claim <- acquisition_queue_claim(catalog, 'killed-worker', lease_seconds = 1L)",
    "guard <- acquisition_queue_start_heartbeat_guard(catalog, claim, lease_seconds = 1L)",
    "if (!isTRUE(guard$ok)) stop(guard$error_message)",
    "writeLines(as.character(Sys.getpid()), pid_file)",
    "writeLines(c(claim$job_id, claim$lease_token), output)",
    "invisible(file.create(ready))",
    "repeat Sys.sleep(1)"
  ), worker)
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", worker, root, db, output, pid_file, ready),
    wait = FALSE
  )
  deadline <- Sys.time() + 15
  while (!file.exists(ready)) {
    if (Sys.time() > deadline) stop("killed-worker child did not claim")
    Sys.sleep(0.05)
  }
  tools::pskill(as.integer(readLines(pid_file)), signal = 9L)
  Sys.sleep(1.3)
  restarted <- acquisition_catalog_open(db)
  second <- acquisition_queue_claim(restarted, "restarted-worker", lease_seconds = 30L)
  expect_true(second$ok)
  expect_identical(second$job_id, job$job_id)
  expect_identical(second$attempt_no, 2L)
  stale <- acquisition_queue_complete(
    restarted, job$job_id, readLines(output)[[2L]],
    status = "succeeded"
  )
  expect_false(stale$ok)
  expect_true(acquisition_queue_complete(
    restarted, second$job_id, second$lease_token,
    status = "succeeded"
  )$ok)
  expect_equal(nrow(acquisition_queue_attempts(restarted)), 2L)
  restarted$close()
})
