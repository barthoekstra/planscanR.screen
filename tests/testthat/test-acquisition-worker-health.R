testthat::skip_if_not_installed("DBI")
testthat::skip_if_not_installed("RSQLite")

worker_health_catalog <- function() {
  acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
}

worker_health_time <- function(value) {
  as.POSIXct(value, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

testthat::test_that("worker health distinguishes idle, busy, and unknown without queue attempts", {
  catalog <- worker_health_catalog()
  withr::defer(catalog$close())
  acquisition_worker_health_configure(catalog, c("worker-1", "worker-2"))

  acquisition_worker_health_start(
    catalog,
    "worker-1",
    now = worker_health_time("2026-10-08 10:00:00")
  )
  idle <- acquisition_worker_health_status(
    catalog,
    now = worker_health_time("2026-10-08 10:00:05"),
    freshness_seconds = 30
  )

  testthat::expect_identical(idle$counts[["expected"]], 2L)
  testthat::expect_identical(idle$counts[["available"]], 1L)
  testthat::expect_identical(idle$counts[["alive"]], 1L)
  testthat::expect_identical(idle$workers$state[match("worker-1", idle$workers$worker_id)], "idle")
  testthat::expect_true(idle$workers$available[match("worker-1", idle$workers$worker_id)])
  testthat::expect_identical(idle$workers$state[match("worker-2", idle$workers$worker_id)], "unknown")
  testthat::expect_false(any(idle$workers$worker_id %in% acquisition_queue_attempts(catalog)$worker_id))

  acquisition_worker_health_heartbeat(
    catalog,
    "worker-1",
    state = "busy",
    now = worker_health_time("2026-10-08 10:00:10")
  )
  busy <- acquisition_worker_health_status(
    catalog,
    now = worker_health_time("2026-10-08 10:00:15"),
    freshness_seconds = 30
  )
  testthat::expect_identical(busy$workers$state[match("worker-1", busy$workers$worker_id)], "busy")
  testthat::expect_false(busy$workers$available[match("worker-1", busy$workers$worker_id)])
  testthat::expect_identical(busy$counts[["alive"]], 1L)
  testthat::expect_identical(busy$counts[["available"]], 0L)
})

testthat::test_that("worker health reports stale, stopped, error, and restart transitions", {
  catalog <- worker_health_catalog()
  withr::defer(catalog$close())
  acquisition_worker_health_configure(catalog, c("worker-1", "worker-2"))

  acquisition_worker_health_start(
    catalog,
    "worker-1",
    now = worker_health_time("2026-10-08 10:00:00")
  )
  stale <- acquisition_worker_health_status(
    catalog,
    now = worker_health_time("2026-10-08 10:01:00"),
    freshness_seconds = 30
  )
  testthat::expect_identical(stale$workers$state[match("worker-1", stale$workers$worker_id)], "stale")
  testthat::expect_false(stale$workers$available[match("worker-1", stale$workers$worker_id)])

  acquisition_worker_health_stop(
    catalog,
    "worker-1",
    now = worker_health_time("2026-10-08 10:01:05")
  )
  acquisition_worker_health_stop(
    catalog,
    "worker-2",
    state = "error",
    error_category = "worker-crash",
    error_message = "GET https://user:password@example.test/?access_token=secret",
    now = worker_health_time("2026-10-08 10:01:06")
  )
  stopped <- acquisition_worker_health_status(
    catalog,
    now = worker_health_time("2026-10-08 10:02:00"),
    freshness_seconds = 30
  )
  testthat::expect_identical(stopped$workers$state[match("worker-1", stopped$workers$worker_id)], "stopped")
  testthat::expect_identical(stopped$workers$state[match("worker-2", stopped$workers$worker_id)], "error")
  error_text <- stopped$workers$error_message[match("worker-2", stopped$workers$worker_id)]
  testthat::expect_false(grepl("password|secret", error_text, ignore.case = TRUE))

  acquisition_worker_health_start(
    catalog,
    "worker-1",
    now = worker_health_time("2026-10-08 10:02:05")
  )
  restarted <- acquisition_worker_health_status(
    catalog,
    now = worker_health_time("2026-10-08 10:02:10"),
    freshness_seconds = 30
  )
  testthat::expect_identical(restarted$workers$state[match("worker-1", restarted$workers$worker_id)], "idle")
  testthat::expect_true(restarted$workers$available[match("worker-1", restarted$workers$worker_id)])
  testthat::expect_identical(restarted$counts[["available"]], 1L)
})

testthat::test_that("worker health is bounded and stores no heartbeat bytes", {
  catalog <- worker_health_catalog()
  withr::defer(catalog$close())

  testthat::expect_error(
    acquisition_worker_health_configure(catalog, paste0("worker-", 1:33)),
    class = "planscanR_error_acquisition_queue"
  )
  acquisition_worker_health_start(
    catalog,
    "worker-1",
    now = worker_health_time("2026-10-08 10:00:00")
  )
  status <- acquisition_worker_health_status(
    catalog,
    now = worker_health_time("2026-10-08 10:00:01"),
    freshness_seconds = 30
  )
  testthat::expect_false(any(c("bytes", "size_bytes", "completed_bytes") %in% names(status$workers)))
  testthat::expect_lte(nrow(status$workers), 32L)
})

testthat::test_that("standalone queue callers do not register transient health identities", {
  catalog <- worker_health_catalog()
  withr::defer(catalog$close())
  for (worker_id in paste0("direct-", seq_len(40L))) {
    testthat::expect_length(
      acquisition_worker_run(catalog, worker_id = worker_id, max_jobs = 0L),
      0L
    )
  }
  testthat::expect_equal(
    DBI::dbGetQuery(
      catalog$conn,
      "SELECT COUNT(*) AS n FROM acquisition_worker_health WHERE expected = 1"
    )$n,
    0L
  )
})

testthat::test_that("a killed worker process becomes stale without a graceful stop", {
  testthat::skip_if_not(.Platform$OS.type == "unix")
  db <- withr::local_tempfile(fileext = ".sqlite")
  worker <- withr::local_tempfile(fileext = ".R")
  pid_file <- withr::local_tempfile()
  ready <- withr::local_tempfile()
  withr::defer(unlink(c(worker, pid_file, ready, db, paste0(db, c("-wal", "-shm"))), force = TRUE))
  writeLines(c(
    "args <- commandArgs(trailingOnly = TRUE)",
    "root <- args[[1L]]; db <- args[[2L]]; pid_file <- args[[3L]]; ready <- args[[4L]]",
    "source(file.path(root, 'R', 'acquisition_catalog.R'), local = globalenv())",
    "source(file.path(root, 'R', 'acquisition_queue.R'), local = globalenv())",
    "source(file.path(root, 'R', 'acquisition_worker_health.R'), local = globalenv())",
    "catalog <- acquisition_catalog_open(db)",
    "acquisition_worker_health_configure(catalog, 'crash-worker')",
    "acquisition_worker_health_start(catalog, 'crash-worker')",
    "writeLines(as.character(Sys.getpid()), pid_file)",
    "writeLines('ready', ready)",
    "repeat Sys.sleep(1)"
  ), worker)
  root <- normalizePath(testthat::test_path("..", ".."), mustWork = TRUE)
  system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", worker, root, db, pid_file, ready),
    wait = FALSE
  )
  deadline <- Sys.time() + 10
  while (!file.exists(ready)) {
    if (Sys.time() > deadline) testthat::fail("health worker child did not start")
    Sys.sleep(0.05)
  }
  pid <- as.integer(readLines(pid_file, n = 1L))
  withr::defer(try(tools::pskill(pid, signal = 9L), silent = TRUE))
  tools::pskill(pid, signal = 9L)
  Sys.sleep(0.5)
  catalog <- acquisition_catalog_open(db)
  withr::defer(catalog$close())
  status <- acquisition_worker_health_status(catalog, freshness_seconds = 0)
  testthat::expect_identical(status$workers$state[[1L]], "stale")
})
