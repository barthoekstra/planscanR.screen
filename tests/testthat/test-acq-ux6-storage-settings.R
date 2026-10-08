# ACQ-UX6: effective storage destinations and the one consumed setting.

testthat::test_that("effective storage reports registered roots, state fallback, and versioned setting", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  root <- file.path(withr::local_tempdir(), "downloads")
  state <- file.path(withr::local_tempdir(), "state")
  marker <- file.path(root, "existing.bin")
  dir.create(root, recursive = TRUE)
  dir.create(state, recursive = TRUE)
  writeLines("keep", marker)
  catalog <- acquisition_catalog_open(file.path(withr::local_tempdir(), "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  admin <- curation_actor("admin-ux6", roles = "admin")

  acquisition_storage_register_root(
    catalog, "downloads", root, admin,
    reserve_bytes = 1024, reserve_fraction = 0.1
  )
  saved <- acquisition_storage_set(catalog, "default_estimate_bytes", 2 * 1024^2, admin)
  effective <- acquisition_storage_effective(catalog, state_root = state)

  testthat::expect_identical(saved$key, "default_estimate_bytes")
  testthat::expect_equal(saved$version, 2L)
  testthat::expect_true(any(effective$destinations$purpose == "downloads"))
  testthat::expect_match(effective$destinations$path[effective$destinations$purpose == "downloads"], "downloads", fixed = TRUE)
  testthat::expect_identical(effective$destinations$status[effective$destinations$purpose == "state"], "Ready")
  missing_state <- acquisition_storage_effective(
    catalog,
    storage_context = list(state_root = file.path(withr::local_tempdir(), "missing-state"))
  )
  testthat::expect_identical(missing_state$destinations$status[missing_state$destinations$purpose == "state"], "Unavailable")
  fallback <- acquisition_storage_effective(catalog)
  testthat::expect_match(
    fallback$destinations$source[fallback$destinations$purpose == "catalog"],
    "catalog directory",
    fixed = TRUE
  )
  testthat::expect_equal(effective$settings$value[[1L]], 2 * 1024^2)
  testthat::expect_equal(effective$settings$version[[1L]], 2L)
  testthat::expect_true(file.exists(marker))
  testthat::expect_true(length(acquisition_storage_settings(catalog, "default_estimate_bytes")$changed_by) >= 2L)

  reviewer <- curation_actor("reviewer-ux6", roles = "reviewer")
  testthat::expect_error(
    acquisition_storage_set(catalog, "default_estimate_bytes", 3 * 1024^2, reviewer),
    class = "planscanR_error_acquisition_storage_authorization"
  )
  testthat::expect_error(
    acquisition_storage_set(catalog, "default_estimate_bytes", -1, admin),
    class = "planscanR_error_acquisition_storage"
  )
  testthat::expect_error(
    acquisition_storage_set(catalog, "default_estimate_bytes", NA_real_, admin),
    class = "planscanR_error_acquisition_storage"
  )
  testthat::expect_error(
    acquisition_storage_set(catalog, "default_estimate_bytes", Inf, admin),
    class = "planscanR_error_acquisition_storage"
  )
})

testthat::test_that("dashboard exposes effective settings and applies only future-job changes", {
  for (package in c("shiny", "bslib", "reactable", "plotly", "htmltools")) {
    testthat::skip_if_not_installed(package)
  }
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  root <- file.path(withr::local_tempdir(), "downloads")
  state <- file.path(withr::local_tempdir(), "state")
  catalog <- acquisition_catalog_open(file.path(withr::local_tempdir(), "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  admin <- curation_actor_resolver(function(request) curation_actor("admin-ux6", roles = "admin"))
  reviewer <- curation_actor_resolver(function(request) curation_actor("reviewer-ux6", roles = "reviewer"))
  acquisition_storage_register_root(catalog, "downloads", root, curation_actor("seed-ux6", roles = "admin"))
  estimate <- max(planscanR::storage_transfer_cap_bytes() * 2, planscanR::storage_transfer_cap_bytes() + 1)

  html <- htmltools::renderTags(
    acquisition_dashboard_ui("ux6", storage_context = list(state_root = state))
  )$html
  testthat::expect_match(html, "Effective storage and settings", fixed = TRUE)
  testthat::expect_match(html, "future download jobs", fixed = TRUE)
  testthat::expect_match(html, "restart", fixed = TRUE)
  testthat::expect_match(html, "overflow-x:auto", fixed = TRUE)
  testthat::expect_match(html, "min-width:760px", fixed = TRUE)
  testthat::expect_match(html, "overflow-wrap:anywhere", fixed = TRUE)

  shiny::testServer(
    acquisition_dashboard_server,
    args = list(
      id = "dashboard", catalog = catalog, actor_resolver = admin,
      storage_context = list(state_root = state), refresh_ms = 250L
    ),
    {
      session$flushReact()
      testthat::expect_match(as.character(output$`storage-effective`), "downloads", fixed = TRUE)
      testthat::expect_match(
        paste(as.character(output$`storage-controls`), collapse = " "),
        "built-in reservation floor",
        fixed = TRUE
      )
      session$setInputs(
        `storage-default-estimate` = planscanR::storage_transfer_cap_bytes() - 1,
        `storage-save` = 1L
      )
      session$flushReact()
      testthat::expect_match(as.character(output$`storage-result`), "at least", fixed = TRUE)
      session$setInputs(`storage-default-estimate` = estimate, `storage-save` = 1L)
      session$flushReact()
      testthat::expect_match(as.character(output$`storage-result`), "future download jobs", fixed = TRUE)
      testthat::expect_match(as.character(output$`storage-result`), "restart is not required", fixed = TRUE)
      testthat::expect_equal(
        acquisition_storage_settings(catalog, "default_estimate_bytes")$value[[2L]],
        estimate
      )
    }
  )

  shiny::testServer(
    acquisition_dashboard_server,
    args = list(
      id = "dashboard", catalog = catalog, actor_resolver = reviewer,
      storage_context = list(state_root = state), refresh_ms = 250L
    ),
    {
      session$flushReact()
      testthat::expect_match(paste(as.character(output$`storage-controls`), collapse = " "), "read-only", fixed = TRUE)
      session$setInputs(`storage-default-estimate` = estimate + 1, `storage-save` = 1L)
      session$flushReact()
      testthat::expect_match(as.character(output$`storage-result`), "administrator", fixed = TRUE)
      testthat::expect_equal(
        acquisition_storage_settings(catalog, "default_estimate_bytes")$value[[2L]],
        estimate
      )
    }
  )

  calls <- 0L
  expiring <- curation_actor_resolver(function(request) {
    calls <<- calls + 1L
    if (calls == 1L) curation_actor("admin-ux6", roles = "admin") else curation_actor("reviewer-ux6", roles = "reviewer")
  })
  before <- nrow(acquisition_storage_settings(catalog, "default_estimate_bytes"))
  shiny::testServer(
    acquisition_dashboard_server,
    args = list(
      id = "dashboard", catalog = catalog, actor_resolver = expiring,
      storage_context = list(state_root = state), refresh_ms = 250L
    ),
    {
      session$flushReact()
      session$setInputs(`storage-default-estimate` = estimate + 2, `storage-save` = 1L)
      session$flushReact()
      testthat::expect_match(paste(as.character(output$`storage-result`), collapse = " "), "administrator", fixed = TRUE)
    }
  )
  testthat::expect_equal(nrow(acquisition_storage_settings(catalog, "default_estimate_bytes")), before)
})

testthat::test_that("a larger estimate is consumed by future reservations without changing queued work", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  root <- file.path(withr::local_tempdir(), "downloads")
  catalog <- acquisition_catalog_open(file.path(withr::local_tempdir(), "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  admin <- curation_actor("admin-ux6", roles = "admin")
  acquisition_storage_register_root(catalog, "downloads", root, admin)
  capacity <- planscanR::storage_capacity(planscanR::storage_root_validate(root, create = TRUE, writable = TRUE))
  cap <- planscanR::storage_transfer_cap_bytes()
  estimate <- max(cap * 2, cap + 1)
  testthat::skip_if(capacity$free_bytes <= estimate * 2, "The test filesystem has too little headroom for a reservation-consumer check.")

  source <- acquisition_catalog_register_source(catalog, "nl", "portal-ux6", "project-ux6")
  document <- acquisition_catalog_register_document(
    catalog, source$source_ref_id,
    native_attachment_id = "document-ux6",
    attachment_url = "https://fixture.invalid/ux6.pdf"
  )
  acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id, estimated_bytes = 0)
  before <- acquisition_queue_jobs(catalog)
  acquisition_storage_set(catalog, "default_estimate_bytes", estimate, admin)
  after <- acquisition_queue_jobs(catalog)
  testthat::expect_equal(after[, c("job_id", "state", "estimated_bytes")], before[, c("job_id", "state", "estimated_bytes")])

  claim <- acquisition_queue_claim(catalog, "worker-ux6")
  testthat::expect_true(claim$ok)
  testthat::expect_equal(claim$storage_reservation_bytes, estimate)
  acquisition_queue_complete(catalog, claim$job_id, claim$lease_token, status = "retryable")
  testthat::expect_identical(acquisition_storage_reservations(catalog)$state[[1L]], "released")
})
