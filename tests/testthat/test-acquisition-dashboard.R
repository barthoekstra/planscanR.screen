testthat::test_that("dashboard counts successful-empty, discovered, and unknown inventory separately", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  empty_source <- acquisition_catalog_register_source(
    catalog, "nl", "portal.example", "empty-project",
    source_url = "https://portal.example/empty"
  )
  discovered_source <- acquisition_catalog_register_source(
    catalog, "de", "portal.example", "discovered-project",
    source_url = "https://portal.example/discovered"
  )
  unknown_source <- acquisition_catalog_register_source(
    catalog, "se", "portal.example", "unknown-project",
    source_url = "https://portal.example/unknown"
  )
  partial_source <- acquisition_catalog_register_source(
    catalog, "nl", "portal.example", "partial-project",
    source_url = "https://portal.example/partial"
  )
  acquisition_catalog_register_document(
    catalog,
    unknown_source$source_ref_id,
    native_attachment_id = "attachment-without-file",
    attachment_url = "https://cdn.example/unknown.pdf"
  )
  document <- acquisition_catalog_register_document(
    catalog,
    discovered_source$source_ref_id,
    native_attachment_id = "attachment-1",
    attachment_url = "https://cdn.example/attachment-1.pdf"
  )
  acquisition_catalog_register_file(
    catalog,
    document$document_ref_id,
    root_id = "fixture",
    relative_path = "attachment-1.pdf",
    sha256 = paste(rep("a", 64), collapse = ""),
    size_bytes = 42L,
    verification_status = "verified"
  )
  partial_job <- acquisition_queue_enqueue(catalog, "discover", "source", partial_source$source_ref_id, priority = 10L)
  empty_job <- acquisition_queue_enqueue(catalog, "discover", "source", empty_source$source_ref_id, priority = 20L)
  failed_job <- acquisition_queue_enqueue(catalog, "discover", "source", discovered_source$source_ref_id, priority = 20L, max_attempts = 1L)
  empty_claim <- acquisition_queue_claim(catalog, "worker-empty")
  acquisition_queue_complete(catalog, empty_claim$job_id, empty_claim$lease_token, status = "succeeded", result = list(status = "successful-empty"))
  failed_claim <- acquisition_queue_claim(catalog, "worker-failed")
  acquisition_queue_complete(catalog, failed_claim$job_id, failed_claim$lease_token, status = "retryable", error_category = "portal", error_message = "fixture")
  partial_claim <- acquisition_queue_claim(catalog, "worker-partial")
  testthat::expect_identical(partial_claim$job_id, partial_job$job_id)
  partial_done <- acquisition_queue_complete(
    catalog,
    partial_claim$job_id,
    partial_claim$lease_token,
    status = "succeeded",
    result = list(
      status = "succeeded",
      documents = list(
        list(native_attachment_id = "partial-1", attachment_url = "https://cdn.example/partial-1.pdf", ordinal = 1L),
        list(native_attachment_id = "partial-2", attachment_url = "https://cdn.example/partial-2.pdf", ordinal = 2L)
      )
    )
  )
  acquisition_catalog_register_file(
    catalog,
    partial_done$document_ref_ids[[1L]],
    root_id = "fixture",
    relative_path = "partial-1.pdf",
    sha256 = paste(rep("b", 64), collapse = ""),
    size_bytes = 24L,
    verification_status = "verified"
  )

  summary <- acquisition_dashboard_summary(catalog)
  project <- summary$projects
  testthat::expect_identical(project$inventory_status[project$source_ref_id == empty_source$source_ref_id], "successful_empty")
  testthat::expect_identical(project$inventory_status[project$source_ref_id == discovered_source$source_ref_id], "error")
  testthat::expect_identical(project$inventory_status[project$source_ref_id == unknown_source$source_ref_id], "unknown")
  testthat::expect_identical(project$inventory_status[project$source_ref_id == partial_source$source_ref_id], "partial")
  testthat::expect_equal(project$verified_document_count[project$source_ref_id == partial_source$source_ref_id], 1L)
  testthat::expect_equal(project$document_count[project$source_ref_id == partial_source$source_ref_id], 2L)
  testthat::expect_equal(summary$unknown_totals$projects, 1L)
  testthat::expect_equal(summary$unknown_totals$inventory_errors, 1L)
  testthat::expect_equal(summary$unknown_totals$files, 2L)
  testthat::expect_true(any(summary$file_counts$verification_status == "verified"))
  testthat::expect_true(any(summary$project_counts$successful_empty_count == 1L))
  testthat::expect_true(any(summary$stage_counts$stage == "discovery" & summary$stage_counts$state == "succeeded"))
  testthat::expect_true(is.character(summary$refreshed_at) && nzchar(summary$refreshed_at))
  testthat::expect_identical(empty_job$status, "inserted")
  queue_page <- acquisition_dashboard_jobs(catalog, state = "queued", page = 1L, page_size = 1L)
  testthat::expect_equal(nrow(queue_page), 1L)
  testthat::expect_identical(attr(queue_page, "total_count"), 2L)
  stage_fixture <- data.frame(
    kind = c("discover", "download", "migrate_storage"),
    state = rep("queued", 3L),
    stringsAsFactors = FALSE
  )
  stage_fixture_counts <- planscanR.screen:::acquisition_dashboard_stage_counts(stage_fixture)
  testthat::expect_true(any(stage_fixture_counts$stage == "storage_migration"))
})

testthat::test_that("dashboard keeps successful discovery known while downloads are pending", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  source <- acquisition_catalog_register_source(
    catalog, "nl", "portal.example", "pending-project",
    source_url = "https://portal.example/pending"
  )
  job <- acquisition_queue_enqueue(catalog, "discover", "source", source$source_ref_id)
  claim <- acquisition_queue_claim(catalog, "worker-discovery")
  testthat::expect_identical(claim$job_id, job$job_id)
  completed <- acquisition_queue_complete(
    catalog,
    claim$job_id,
    claim$lease_token,
    status = "succeeded",
    result = list(
      status = "succeeded",
      documents = list(list(
        native_attachment_id = "pending-attachment",
        attachment_url = "https://cdn.example/pending.pdf",
        ordinal = 1L
      ))
    )
  )
  testthat::expect_true(completed$ok)

  summary <- acquisition_dashboard_summary(catalog)
  project <- summary$projects[summary$projects$source_ref_id == source$source_ref_id, , drop = FALSE]
  testthat::expect_identical(project$inventory_status, "pending")
  testthat::expect_equal(project$document_count, 1L)
  testthat::expect_equal(project$pending_document_count, 1L)
  project_count <- summary$project_counts[summary$project_counts$country == "nl", , drop = FALSE]
  testthat::expect_equal(project_count$discovered_count, 1L)
  testthat::expect_equal(project_count$complete_count, 0L)
  testthat::expect_equal(project_count$pending_count, 1L)
})

testthat::test_that("authenticated controls pause claims, cancel running work, retain audit, and retry attempts", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  source <- acquisition_catalog_register_source(
    catalog, "nl", "portal.example", "control-project",
    source_url = "https://portal.example/control"
  )
  job <- acquisition_queue_enqueue(catalog, "discover", "source", source$source_ref_id)
  admin <- curation_actor_resolver(function(request) curation_actor("admin-1", roles = "admin"))
  reviewer <- curation_actor_resolver(function(request) curation_actor("reviewer-1", roles = "reviewer"))

  testthat::expect_error(
    acquisition_control_pause(catalog, "job", job$job_id, reviewer),
    class = "planscanR_error_acquisition_forbidden"
  )
  paused <- acquisition_control_pause(catalog, "job", job$job_id, admin, reason = "fixture pause")
  testthat::expect_identical(paused$status, "paused")
  testthat::expect_identical(acquisition_queue_claim(catalog, "worker-paused")$status, "empty")
  testthat::expect_identical(acquisition_control_audit(catalog, action = "pause")$actor_id, "admin-1")
  acquisition_control_resume(catalog, "job", job$job_id, admin, reason = " ")
  claim <- acquisition_queue_claim(catalog, "worker-running")
  cancelled <- acquisition_control_cancel(catalog, "job", job$job_id, admin, reason = "fixture cancel")
  testthat::expect_identical(cancelled$status, "cancel_requested")
  completed <- acquisition_queue_complete(
    catalog,
    claim$job_id,
    claim$lease_token,
    status = "succeeded",
    result = list(status = "succeeded", size_bytes = 7L)
  )
  testthat::expect_true(completed$ok)
  testthat::expect_identical(completed$status, "cancelled")
  testthat::expect_identical(acquisition_queue_jobs(catalog)$state, "cancelled")
  attempts <- acquisition_queue_attempts(catalog, job$job_id)
  testthat::expect_identical(attempts$outcome, "cancelled")
  retried <- acquisition_control_retry(catalog, "job", job$job_id, admin)
  testthat::expect_identical(retried$status, "retry_requested")
  testthat::expect_identical(acquisition_queue_jobs(catalog)$state, "queued")
  testthat::expect_identical(acquisition_queue_jobs(catalog)$attempt_count, 1L)
  audit <- acquisition_control_audit(catalog, target_id = job$job_id)
  testthat::expect_true(all(c("pause", "resume", "cancel", "retry") %in% audit$action))

  exhausted_source <- acquisition_catalog_register_source(
    catalog,
    "nl",
    "portal.example",
    "control-exhausted",
    source_url = "https://portal.example/control-exhausted"
  )
  exhausted_job <- acquisition_queue_enqueue(
    catalog,
    "discover",
    "source",
    exhausted_source$source_ref_id,
    priority = 10L,
    max_attempts = 1L,
    idempotency_key = "control-exhausted-retry"
  )
  exhausted_claim <- acquisition_queue_claim(catalog, "worker-exhausted")
  acquisition_queue_complete(
    catalog,
    exhausted_claim$job_id,
    exhausted_claim$lease_token,
    status = "retryable",
    error_category = "fixture",
    error_message = "exhausted"
  )
  exhausted_before <- acquisition_queue_jobs(catalog)
  exhausted_before <- exhausted_before[exhausted_before$job_id == exhausted_job$job_id, , drop = FALSE]
  testthat::expect_identical(exhausted_before$state, "failed")
  testthat::expect_identical(exhausted_before$attempt_count, 1L)
  testthat::expect_identical(exhausted_before$max_attempts, 1L)
  exhausted_retry <- acquisition_control_retry(catalog, "job", exhausted_job$job_id, admin)
  testthat::expect_identical(exhausted_retry$retry_allowance, 1L)
  exhausted_after <- acquisition_queue_jobs(catalog)
  exhausted_after <- exhausted_after[exhausted_after$job_id == exhausted_job$job_id, , drop = FALSE]
  testthat::expect_identical(exhausted_after$state, "queued")
  testthat::expect_identical(exhausted_after$attempt_count, 1L)
  testthat::expect_identical(exhausted_after$max_attempts, 2L)
  exhausted_audit <- acquisition_control_audit(catalog, target_id = exhausted_job$job_id, action = "retry")
  exhausted_details <- acquisition_queue_unblob(exhausted_audit$details_blob[[1L]])
  testthat::expect_true(exhausted_job$job_id %in% exhausted_details$retry_allowance_job_ids)
  exhausted_retry_claim <- acquisition_queue_claim(catalog, "worker-exhausted-retry")
  testthat::expect_identical(exhausted_retry_claim$job_id, exhausted_job$job_id)
  exhausted_success <- acquisition_queue_complete(
    catalog,
    exhausted_retry_claim$job_id,
    exhausted_retry_claim$lease_token,
    status = "succeeded",
    result = list(status = "successful-empty")
  )
  testthat::expect_true(exhausted_success$ok)

  source_search <- acquisition_dashboard_jobs(catalog, search = source$source_ref_id)
  testthat::expect_true(any(source_search$job_id == job$job_id))
  document_search <- acquisition_dashboard_jobs(catalog, search = "unused-document-id")
  testthat::expect_equal(nrow(document_search), 0L)
  searched_document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "search-attachment",
    attachment_url = "https://cdn.example/search-document.pdf"
  )
  searched_document_job <- acquisition_queue_enqueue(
    catalog,
    "download",
    "document",
    searched_document$document_ref_id,
    idempotency_key = "search-document-job"
  )
  document_search <- acquisition_dashboard_jobs(catalog, search = "search-attachment")
  testthat::expect_true(any(document_search$job_id == searched_document_job$job_id))
})
