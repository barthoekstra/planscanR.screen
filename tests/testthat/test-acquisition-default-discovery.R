# Focused ACQ-RUN5a coverage for default source discovery and queue ordering.

testthat::test_that("default discovery refreshes persisted identity and forwards sections", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  catalog <- acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  source <- acquisition_catalog_register_source(
    catalog,
    country = "nl",
    portal = "commissiemer.nl",
    native_project_id = "3121",
    source_url = "https://www.commissiemer.nl/advies/project-3121/"
  )
  job <- acquisition_queue_enqueue(catalog, "discover", "source", source$source_ref_id)
  claim <- acquisition_queue_claim(catalog, "refresh-worker")
  seen <- NULL
  discovered_url <- "https://pas.commissiemer.nl/files/nl/3121/source-report.pdf"
  testthat::local_mocked_bindings(
    resolve_attachment_inventory = function(records, refresh = FALSE) {
      seen <<- list(records = records, refresh = refresh)
      tibble::tibble(
        country = "nl",
        document_id = "3121",
        status = "success",
        reason = NA_character_,
        authoritative = TRUE,
        checked_at = "2026-10-08T18:00:00Z",
        attachment_urls = list(discovered_url),
        attachment_urls_source = list(discovered_url)
      )
    },
    .package = "planscanR"
  )

  result <- planscanR.screen:::acquisition_queue_default_discover(claim, catalog)

  testthat::expect_true(isTRUE(seen$refresh))
  testthat::expect_identical(seen$records$url, "https://www.commissiemer.nl/advies/project-3121/")
  testthat::expect_identical(seen$records$source_portal, "commissiemer.nl")
  testthat::expect_identical(result$status, "succeeded")
  testthat::expect_identical(result$documents[[1L]]$attachment_url, discovered_url)
  testthat::expect_identical(result$documents[[1L]]$section, "source")
  testthat::expect_identical(result$documents[[1L]]$identity_status, "ambiguous")
  testthat::expect_identical(job$job_id, claim$job_id)
})

testthat::test_that("default discovery keeps a persisted authoritative empty cached", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  catalog <- acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  source <- acquisition_catalog_register_source(
    catalog,
    country = "nl",
    portal = "commissiemer.nl",
    native_project_id = "empty-project",
    source_url = "https://www.commissiemer.nl/advies/empty-project/",
    metadata = list(
      discovery_status = "empty",
      discovery_reason = "portal checked",
      discovery_checked_at = "2026-10-08T18:00:00Z"
    )
  )
  job <- acquisition_queue_enqueue(catalog, "discover", "source", source$source_ref_id)
  claim <- acquisition_queue_claim(catalog, "cached-empty-worker")
  seen_refresh <- NULL
  testthat::local_mocked_bindings(
    resolve_attachment_inventory = function(records, refresh = FALSE) {
      seen_refresh <<- refresh
      tibble::tibble(
        country = "nl",
        document_id = "empty-project",
        status = "empty",
        reason = "portal checked",
        authoritative = TRUE,
        checked_at = "2026-10-08T18:00:00Z",
        attachment_urls = list(character(0))
      )
    },
    .package = "planscanR"
  )

  result <- planscanR.screen:::acquisition_queue_default_discover(claim, catalog)

  testthat::expect_false(isTRUE(seen_refresh))
  testthat::expect_identical(result$status, "succeeded")
  testthat::expect_true(isTRUE(result$successful_empty))
  testthat::expect_identical(job$job_id, claim$job_id)
})

testthat::test_that("malformed default discovery inventory is retryable", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  catalog <- acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  source <- acquisition_catalog_register_source(
    catalog,
    country = "nl",
    portal = "commissiemer.nl",
    native_project_id = "malformed-project",
    source_url = "https://www.commissiemer.nl/advies/malformed-project/"
  )
  acquisition_queue_enqueue(catalog, "discover", "source", source$source_ref_id)
  claim <- acquisition_queue_claim(catalog, "malformed-discovery-worker")
  testthat::local_mocked_bindings(
    resolve_attachment_inventory = function(records, refresh = FALSE) {
      tibble::tibble(status = "success", attachment_urls = list("https://cdn.example/file.pdf"))
    },
    .package = "planscanR"
  )

  result <- planscanR.screen:::acquisition_queue_default_discover(claim, catalog)

  testthat::expect_identical(result$status, "retryable")
  testthat::expect_identical(result$error_category, "discovery-retryable")
})

testthat::test_that("equal-priority ready downloads claim before older discovery jobs", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  catalog <- acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  source <- acquisition_catalog_register_source(
    catalog, "nl", "portal.example", "priority-source",
    source_url = "https://portal.example/priority-source"
  )
  document <- acquisition_catalog_register_document(
    catalog, source$source_ref_id,
    native_attachment_id = "priority-document",
    attachment_url = "https://cdn.example/priority.pdf"
  )
  discovery <- acquisition_queue_enqueue(catalog, "discover", "source", source$source_ref_id, priority = 40L)
  download <- acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id, priority = 40L)

  claim <- acquisition_queue_claim(catalog, "priority-worker")

  testthat::expect_true(claim$ok)
  testthat::expect_identical(claim$job_id, download$job_id)
  testthat::expect_false(identical(claim$job_id, discovery$job_id))
})

testthat::test_that("higher-priority discovery still beats a lower-priority download", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  catalog <- acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  source <- acquisition_catalog_register_source(
    catalog, "nl", "portal.example", "priority-source-high",
    source_url = "https://portal.example/priority-source-high"
  )
  document <- acquisition_catalog_register_document(
    catalog, source$source_ref_id,
    native_attachment_id = "priority-document-low",
    attachment_url = "https://cdn.example/priority-low.pdf"
  )
  discovery <- acquisition_queue_enqueue(catalog, "discover", "source", source$source_ref_id, priority = 50L)
  acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id, priority = 40L)

  claim <- acquisition_queue_claim(catalog, "priority-worker-high")

  testthat::expect_true(claim$ok)
  testthat::expect_identical(claim$job_id, discovery$job_id)
})

testthat::test_that("a host-blocked download does not displace a ready discovery", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  catalog <- acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  source <- acquisition_catalog_register_source(
    catalog, "nl", "portal.example", "priority-source-blocked",
    source_url = "https://portal.example/priority-source-blocked"
  )
  document <- acquisition_catalog_register_document(
    catalog, source$source_ref_id,
    native_attachment_id = "priority-document-blocked",
    attachment_url = "https://cdn-blocked.example/priority.pdf"
  )
  discovery <- acquisition_queue_enqueue(catalog, "discover", "source", source$source_ref_id, priority = 40L)
  acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id, priority = 40L)
  acquisition_queue_set_host_limit(
    catalog,
    "cdn-blocked.example",
    max_concurrency = 1L,
    cooldown_until = "2999-01-01T00:00:00Z"
  )

  claim <- acquisition_queue_claim(catalog, "priority-worker-unblocked")

  testthat::expect_true(claim$ok)
  testthat::expect_identical(claim$job_id, discovery$job_id)
})
