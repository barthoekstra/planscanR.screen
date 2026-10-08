testthat::test_that("dashboard reports independent execution health and verified progress", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  withr::defer(acquisition_catalog_close(catalog))
  source <- acquisition_catalog_register_source(
    catalog,
    "nl",
    "fixture.example",
    "run4-project",
    source_url = "https://fixture.example/run4"
  )
  document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "run4-document",
    attachment_url = "https://fixture.example/run4.pdf"
  )
  pending_document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "run4-pending-document",
    attachment_url = "https://fixture.example/run4-pending.pdf"
  )
  acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id)
  acquisition_queue_enqueue(catalog, "download", "document", pending_document$document_ref_id)
  acquisition_worker_health_configure(catalog, "worker-1")
  acquisition_worker_health_start(catalog, "worker-1")

  idle <- acquisition_dashboard_summary(catalog)
  testthat::expect_identical(idle$worker_health$counts[["expected"]], 1L)
  testthat::expect_identical(idle$execution_health$status, "idle")
  testthat::expect_true(grepl("persisted", idle$queue_acknowledgement, fixed = TRUE))
  testthat::expect_null(idle$pending_warning)

  claim <- acquisition_queue_claim(catalog, "worker-1")
  testthat::expect_true(claim$ok)
  acquisition_worker_health_stop(catalog, "worker-1")
  stopped <- acquisition_dashboard_summary(catalog)
  testthat::expect_identical(stopped$execution_health$status, "offline")
  testthat::expect_match(stopped$pending_warning, "no available worker")
  testthat::expect_match(stopped$pending_warning, "action")

  completed <- acquisition_queue_complete(
    catalog,
    claim$job_id,
    claim$lease_token,
    status = "succeeded",
    result = list(
      status = "succeeded",
      size_bytes = 12L,
      sha256 = paste(rep("a", 64), collapse = ""),
      local_path = file.path(root, "run4.pdf")
    )
  )
  testthat::expect_true(completed$ok)
  acquisition_catalog_register_file(
    catalog,
    document$document_ref_id,
    root_id = "fixture",
    relative_path = "run4.pdf",
    sha256 = paste(rep("a", 64), collapse = ""),
    size_bytes = 12L,
    verification_status = "verified"
  )

  refreshed <- acquisition_dashboard_summary(catalog)
  testthat::expect_equal(refreshed$progress$verified_file_count, 1L)
  testthat::expect_equal(refreshed$progress$verified_bytes, 12)
  testthat::expect_true(nzchar(refreshed$progress$latest_success_at))
  testthat::expect_true(nzchar(refreshed$progress$verified_observed_at))
  testthat::expect_true(refreshed$progress$succeeded_jobs >= 1L)
  testthat::expect_true(all(c("running_jobs", "retry_jobs", "blocked_jobs", "failed_jobs") %in% names(refreshed$progress)))
})

testthat::test_that("fresh busy workers do not trigger a pending warning", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  catalog <- acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
  withr::defer(acquisition_catalog_close(catalog))
  source <- acquisition_catalog_register_source(
    catalog,
    "nl",
    "fixture.example",
    "busy-project",
    source_url = "https://fixture.example/busy"
  )
  documents <- lapply(seq_len(3L), function(index) {
    acquisition_catalog_register_document(
      catalog,
      source$source_ref_id,
      native_attachment_id = paste0("busy-document-", index),
      attachment_url = paste0("https://fixture.example/busy-", index, ".pdf")
    )
  })
  for (document in documents) {
    acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id)
  }
  workers <- c("busy-worker-1", "busy-worker-2")
  for (worker in workers) {
    acquisition_worker_health_configure(catalog, worker)
    acquisition_worker_health_start(catalog, worker)
    acquisition_worker_health_heartbeat(catalog, worker, state = "busy")
    claim <- acquisition_queue_claim(catalog, worker)
    testthat::expect_true(claim$ok)
  }

  summary <- acquisition_dashboard_summary(catalog)
  testthat::expect_identical(as.integer(summary$progress$pending_jobs), 1L)
  testthat::expect_identical(as.integer(summary$progress$running_jobs), 2L)
  testthat::expect_identical(summary$execution_health$status, "busy")
  testthat::expect_null(summary$pending_warning)
})

testthat::test_that("dashboard health poll token includes worker lifecycle updates", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  catalog <- acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
  withr::defer(acquisition_catalog_close(catalog))
  acquisition_worker_health_configure(catalog, "worker-1")
  acquisition_worker_health_start(catalog, "worker-1")
  first <- acquisition_dashboard_poll_token(catalog)
  acquisition_worker_health_heartbeat(catalog, "worker-1", state = "busy")
  second <- acquisition_dashboard_poll_token(catalog)
  testthat::expect_false(identical(first, second))
})

testthat::test_that("stale worker health is visible without a new heartbeat write", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  catalog <- acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
  withr::defer(acquisition_catalog_close(catalog))
  source <- acquisition_catalog_register_source(
    catalog,
    "nl",
    "fixture.example",
    "stale-project",
    source_url = "https://fixture.example/stale"
  )
  document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "stale-document",
    attachment_url = "https://fixture.example/stale.pdf"
  )
  stale_pending_document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "stale-pending-document",
    attachment_url = "https://fixture.example/stale-pending.pdf"
  )
  acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id)
  acquisition_queue_enqueue(catalog, "download", "document", stale_pending_document$document_ref_id)
  acquisition_worker_health_configure(catalog, "worker-stale")
  acquisition_worker_health_start(
    catalog,
    "worker-stale",
    now = Sys.time() - 60
  )
  stale_claim <- acquisition_queue_claim(catalog, "worker-stale")
  testthat::expect_true(stale_claim$ok)

  summary <- acquisition_dashboard_summary(catalog)
  testthat::expect_identical(summary$execution_health$status, "stale")
  testthat::expect_match(summary$pending_warning, "no available worker")
  first <- acquisition_dashboard_poll_token(
    catalog,
    now = as.POSIXct("2026-10-08 10:00:00", tz = "UTC")
  )
  second <- acquisition_dashboard_poll_token(
    catalog,
    now = as.POSIXct("2026-10-08 10:00:11", tz = "UTC")
  )
  testthat::expect_false(identical(first, second))
})

testthat::test_that("dashboard UI makes execution warning and verified progress prominent", {
  testthat::skip_if_not_installed("shiny")

  html <- as.character(acquisition_dashboard_ui("dashboard"))
  testthat::expect_match(html, "Execution health")
  testthat::expect_match(html, "dashboard-pending-warning")
  testthat::expect_match(html, "dashboard-queue-acknowledgement")
  testthat::expect_match(html, "verified")
  testthat::expect_match(html, "latest successful")
})
