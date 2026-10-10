skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

storage_catalog <- function() {
  acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
}

test_that("storage root registration validates paths and requires an administrator", {
  catalog <- storage_catalog()
  on.exit(catalog$close(), add = TRUE)
  root <- file.path(withr::local_tempdir(), "downloads")
  reviewer <- curation_actor("reviewer", roles = "reviewer")
  admin <- curation_actor("admin", roles = "admin")

  expect_error(
    acquisition_storage_register_root(catalog, "downloads", root, reviewer),
    class = "planscanR_error_acquisition_storage_authorization"
  )
  registered <- acquisition_storage_register_root(catalog, "downloads", root, admin)
  expect_identical(registered$root_id, "downloads")
  expect_true(dir.exists(root))
  expect_identical(acquisition_storage_roots(catalog)$relative_policy[[1L]], "portable")
  expect_error(
    acquisition_storage_register_root(catalog, "downloads", file.path(withr::local_tempdir(), "moved"), admin),
    class = "planscanR_error_acquisition_storage_immutable"
  )
})

test_that("settings are versioned and reservation claims enforce a shared disk reserve", {
  root <- file.path(withr::local_tempdir(), "downloads")
  catalog <- storage_catalog()
  on.exit(catalog$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  acquisition_storage_register_root(catalog, "downloads", root, admin, reserve_bytes = 1L)
  acquisition_storage_set(catalog, "default_estimate_bytes", 2^60, admin)
  acquisition_queue_enqueue(catalog, "migrate_storage", "storage_root", "downloads")
  expect_false(acquisition_queue_claim(catalog, "worker-a")$ok)
  expect_identical(acquisition_queue_jobs(catalog)$state[[1L]], "queued")
  expect_equal(nrow(acquisition_storage_reservations(catalog)), 0L)

  acquisition_storage_set(catalog, "default_estimate_bytes", 1L, admin)
  claim <- acquisition_queue_claim(catalog, "worker-b")
  expect_true(claim$ok)
  expect_identical(claim$storage_root_id, "downloads")
  expect_identical(claim$storage_root_path, planscanR::storage_root_validate(root, writable = TRUE))
  held <- acquisition_storage_reservations(catalog)
  expect_identical(held$state[[1L]], "held")
  expect_equal(held$bytes[[1L]], 1)
  acquisition_queue_complete(catalog, claim$job_id, claim$lease_token, status = "retryable")
  expect_identical(acquisition_storage_reservations(catalog)$state[[1L]], "released")
  expect_equal(max(acquisition_storage_settings(catalog, "default_estimate_bytes")$version), 3L)
})

test_that("fractional reserve is measured against total filesystem capacity", {
  catalog <- storage_catalog()
  on.exit(catalog$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  root <- file.path(withr::local_tempdir(), "fraction-root")
  capacity <- planscanR::storage_capacity(planscanR::storage_root_validate(root, create = TRUE, writable = TRUE))
  cap <- planscanR::storage_transfer_cap_bytes()
  skip_if(
    capacity$free_bytes <= 2 * cap || capacity$total_bytes <= capacity$free_bytes,
    "The test filesystem has too little headroom for a stable total-capacity reserve check."
  )
  reserve_fraction <- (capacity$free_bytes - cap / 2) / capacity$total_bytes
  skip_if(
    reserve_fraction <= 0 || reserve_fraction >= 1 ||
      capacity$free_bytes * reserve_fraction > capacity$free_bytes - cap,
    "The test filesystem cannot distinguish total-capacity and free-capacity fractions."
  )
  acquisition_storage_register_root(
    catalog,
    "00-fraction",
    root,
    admin,
    reserve_fraction = reserve_fraction
  )
  acquisition_queue_enqueue(
    catalog, "migrate_storage", "storage_root", "00-fraction",
    estimated_bytes = cap
  )
  expect_false(acquisition_queue_claim(catalog, "fraction-worker")$ok)
  expect_equal(nrow(acquisition_storage_reservations(catalog)), 0L)
})

test_that("unknown-size downloads use live headroom instead of the leaf option", {
  skip_if_not_installed("filelock")
  catalog <- storage_catalog()
  on.exit(catalog$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  root <- file.path(withr::local_tempdir(), "cap-root")
  acquisition_storage_register_root(catalog, "00-cap", root, admin, reserve_bytes = 1L)
  source <- acquisition_catalog_register_source(catalog, "nl", "portal.example", "cap")
  document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "cap",
    attachment_url = "https://portal.example/cap.pdf"
  )
  acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id)
  claim <- acquisition_queue_claim(catalog, "cap-worker")
  expect_true(claim$ok)
  reserved <- as.numeric(claim$storage_reservation_bytes)
  expect_true(is.finite(reserved))
  observed <- new.env(parent = emptyenv())
  fake_download <- function(url, country, document_id, cache_dir, max_file_size_mb, overwrite = FALSE) {
    observed$cap <- max_file_size_mb * 1024^2
    observed$timeout <- getOption("planscanR.timeout")
    tibble::tibble(
      url = url,
      local_path = NA_character_,
      size_bytes = NA_real_,
      sha256 = NA_character_,
      status = "failed",
      reason = "offline cap fixture"
    )
  }
  testthat::local_mocked_bindings(download_to_cache = fake_download, .package = "planscanR")
  timeout_before <- getOption("planscanR.timeout")
  withr::local_options(list(planscanR.max_file_size_mb = 1))
  result <- acquisition_queue_default_download(claim, catalog)
  expect_identical(result$status, "retryable")
  expect_equal(observed$cap, reserved)
  expect_true(is.infinite(observed$timeout))
  expect_identical(getOption("planscanR.timeout"), timeout_before)
  acquisition_queue_complete(catalog, claim$job_id, claim$lease_token, status = "retryable")
})

test_that("two download claims overlap while the third remains queued", {
  catalog <- storage_catalog()
  on.exit(catalog$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  root <- file.path(withr::local_tempdir(), "contention-root")
  acquisition_storage_register_root(catalog, "00-contention", root, admin, reserve_bytes = 1L)
  source <- acquisition_catalog_register_source(catalog, "nl", "portal.example", "contention")
  first_document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "first",
    attachment_url = "https://portal-a.example/first.bin"
  )
  second_document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "second",
    attachment_url = "https://portal-b.example/second.bin"
  )
  third_document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "third",
    attachment_url = "https://portal-c.example/third.bin"
  )
  first_job <- acquisition_queue_enqueue(catalog, "download", "document", first_document$document_ref_id)
  second_job <- acquisition_queue_enqueue(catalog, "download", "document", second_document$document_ref_id)
  third_job <- acquisition_queue_enqueue(catalog, "download", "document", third_document$document_ref_id)
  first <- acquisition_queue_claim(catalog, "contention-worker-a")
  expect_true(first$ok)
  second <- acquisition_queue_claim(catalog, "contention-worker-b")
  expect_true(second$ok)
  third <- acquisition_queue_claim(catalog, "contention-worker-c")
  expect_false(third$ok)
  expect_identical(third$status, "empty")
  states <- acquisition_queue_jobs(catalog)
  expect_identical(states$state[match(first_job$job_id, states$job_id)], "running")
  expect_identical(states$state[match(second_job$job_id, states$job_id)], "running")
  expect_identical(states$state[match(third_job$job_id, states$job_id)], "queued")
  acquisition_queue_complete(
    catalog, first$job_id, first$lease_token,
    status = "retryable", retry_after_seconds = 0
  )
  expect_true(acquisition_queue_claim(catalog, "contention-worker-c")$ok)
  acquisition_queue_complete(catalog, second$job_id, second$lease_token, status = "retryable")
})

test_that("guarded queue downloads grow before underreported chunks and use no leaf cap", {
  skip_if_not_installed("filelock")
  catalog <- storage_catalog()
  on.exit(catalog$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  root <- file.path(withr::local_tempdir(), "guarded-root")
  acquisition_storage_register_root(catalog, "00-guarded", root, admin, reserve_bytes = 1L)
  source <- acquisition_catalog_register_source(catalog, "nl", "portal.example", "guarded")
  document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "guarded",
    attachment_url = "https://portal.example/guarded.bin"
  )
  job <- acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id)
  claim <- acquisition_queue_claim(catalog, "guarded-worker")
  expect_true(claim$ok)
  observed <- new.env(parent = emptyenv())
  fake_download <- function(
    url, country, document_id, cache_dir, max_file_size_mb, overwrite = FALSE,
    storage_guard
  ) {
    observed$cap <- max_file_size_mb
    observed$guard <- storage_guard
    body <- charToRaw("underreported body")
    first <- storage_guard(0, advertised_size = 1, phase = "stream", pending_bytes = 4)
    expect_true(first$ok)
    storage_guard(4, advertised_size = 1, phase = "stream", pending_bytes = length(body) - 4L)
    destination <- planscanR::cache_path(url, country, document_id, cache_dir = cache_dir)
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    writeBin(body, destination)
    sha <- planscanR:::file_sha256(destination)
    planscanR:::write_download_receipt(destination, url, "application/octet-stream", length(body), sha)
    storage_guard(length(body), advertised_size = 1, phase = "publish", pending_bytes = 0)
    tibble::tibble(
      url = url,
      local_path = destination,
      size_bytes = length(body),
      sha256 = sha,
      status = "downloaded",
      reason = NA_character_
    )
  }
  testthat::local_mocked_bindings(download_to_cache = fake_download, .package = "planscanR")
  result <- acquisition_queue_default_download(claim, catalog)
  expect_identical(result$status, "succeeded")
  expect_true(is.infinite(observed$cap))
  expect_true(is.function(observed$guard))
  acquisition_queue_complete(catalog, claim$job_id, claim$lease_token, status = "succeeded", result = result)
  expect_identical(acquisition_storage_reservations(catalog)$state[[1L]], "released")
  expect_identical(acquisition_queue_jobs(catalog)$state[[1L]], "succeeded")
  expect_true(job$job_id %in% acquisition_queue_jobs(catalog)$job_id)
})

test_that("guarded queue content-addressed downloads retain two verified copies", {
  skip_if_not_installed("filelock")
  withr::local_options(planscanR.content_addressed_downloads = TRUE)
  catalog <- storage_catalog()
  on.exit(catalog$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  root <- file.path(withr::local_tempdir(), "guarded-object-root")
  acquisition_storage_register_root(catalog, "00-object", root, admin, reserve_bytes = 1L)
  source <- acquisition_catalog_register_source(catalog, "nl", "portal.example", "object")
  document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "object",
    attachment_url = "https://portal.example/object.bin"
  )
  acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id)
  body <- charToRaw(strrep("q", 8 * 1024^2))
  local_mocked_bindings(
    head_content_length = function(url) 1,
    req_planscanr = function(base_url, path = NULL) structure(list(), class = "httr2_request"),
    .package = "planscanR"
  )
  local_mocked_bindings(
    req_perform_stream = function(req, callback, ...) {
      callback(body)
      structure(list(), class = "httr2_response")
    },
    resp_header = function(resp, header, ...) "application/octet-stream",
    .package = "httr2"
  )
  claim <- acquisition_queue_claim(catalog, "object-worker")
  expect_true(claim$ok)
  result <- acquisition_queue_default_download(claim, catalog)
  expect_identical(result$status, "succeeded")
  expect_true(file.exists(result$local_path))
  legacy <- planscanR::storage_path(root, result$legacy_path)
  expect_true(file.exists(legacy))
  expect_identical(
    planscanR:::file_sha256(result$local_path),
    planscanR:::file_sha256(legacy)
  )
  acquisition_queue_complete(catalog, claim$job_id, claim$lease_token, status = "succeeded", result = result)
  expect_identical(acquisition_storage_reservations(catalog)$state[[1L]], "released")
})

test_that("disk exhaustion rejects a guarded chunk before any partial publication", {
  skip_if_not_installed("filelock")
  catalog <- storage_catalog()
  on.exit(catalog$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  root <- file.path(withr::local_tempdir(), "exhausted-root")
  acquisition_storage_register_root(catalog, "00-exhausted", root, admin, reserve_bytes = 1L)
  source <- acquisition_catalog_register_source(catalog, "nl", "portal.example", "exhausted")
  document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "exhausted",
    attachment_url = "https://portal.example/exhausted.bin"
  )
  acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id)
  claim <- acquisition_queue_claim(catalog, "exhausted-worker")
  expect_true(claim$ok)
  testthat::local_mocked_bindings(storage_capacity = function(root) {
    list(total_bytes = 10 * 1024^2, free_bytes = 1 * 1024^2)
  }, .package = "planscanR")
  wrote <- FALSE
  fake_download <- function(
    url, country, document_id, cache_dir, max_file_size_mb, overwrite = FALSE,
    storage_guard
  ) {
    decision <- storage_guard(0, advertised_size = 100 * 1024^2,
      phase = "stream", pending_bytes = 100 * 1024^2)
    expect_false(decision$ok)
    wrote <<- TRUE
    tibble::tibble(
      url = url,
      local_path = NA_character_,
      size_bytes = NA_real_,
      sha256 = NA_character_,
      status = "failed",
      reason = decision$reason
    )
  }
  testthat::local_mocked_bindings(download_to_cache = fake_download, .package = "planscanR")
  result <- acquisition_queue_default_download(claim, catalog)
  expect_identical(result$status, "retryable")
  expect_true(wrote)
  files <- list.files(root, recursive = TRUE, all.files = TRUE, include.dirs = FALSE)
  expect_false(any(!grepl("\\.lock$", files)))
  acquisition_queue_complete(catalog, claim$job_id, claim$lease_token, status = "retryable", retry_after_seconds = 0)
})

test_that("a stale reservation guard cannot mutate a replacement lease", {
  catalog <- storage_catalog()
  on.exit(catalog$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  root <- file.path(withr::local_tempdir(), "stale-guard-root")
  acquisition_storage_register_root(catalog, "00-stale", root, admin, reserve_bytes = 1L)
  source <- acquisition_catalog_register_source(catalog, "nl", "portal.example", "stale")
  document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "stale",
    attachment_url = "https://portal.example/stale.bin"
  )
  acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id)
  first <- acquisition_queue_claim(catalog, "stale-worker-a", lease_seconds = 1L)
  expect_true(first$ok)
  acquisition_queue_complete(
    catalog, first$job_id, first$lease_token,
    status = "retryable", retry_after_seconds = 0
  )
  second <- acquisition_queue_claim(catalog, "stale-worker-b")
  expect_true(second$ok)
  stale <- acquisition_storage_reservation_update(
    catalog,
    first$job_id,
    first$lease_token,
    bytes_written = 1024,
    pending_bytes = 1024
  )
  expect_false(stale$ok)
  expect_identical(stale$reason, "lease-stale")
  held <- acquisition_storage_reservations(catalog, state = "held")
  expect_identical(held$lease_token[[1L]], second$lease_token)
  acquisition_queue_complete(catalog, second$job_id, second$lease_token, status = "retryable")
})

test_that("content-addressed reservations grow past the bounded initial lease", {
  withr::local_options(planscanR.content_addressed_downloads = TRUE)
  catalog <- storage_catalog()
  on.exit(catalog$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  root <- file.path(withr::local_tempdir(), "growth-root")
  acquisition_storage_register_root(catalog, "00-growth", root, admin, reserve_bytes = 1L)
  source <- acquisition_catalog_register_source(catalog, "nl", "portal.example", "growth")
  document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "growth",
    attachment_url = "https://portal.example/growth.bin"
  )
  acquisition_queue_enqueue(
    catalog,
    "download",
    "document",
    document$document_ref_id,
    estimated_bytes = 32 * 1024^2
  )
  claim <- acquisition_queue_claim(catalog, "growth-worker")
  expect_true(claim$ok)
  initial <- as.numeric(claim$storage_reservation_bytes)
  advertised <- 1
  first_pending <- 8 * 1024^2
  first <- acquisition_storage_reservation_update(
    catalog,
    claim$job_id,
    claim$lease_token,
    bytes_written = 0,
    advertised_bytes = advertised,
    content_addressed = TRUE,
    pending_bytes = first_pending
  )
  expect_true(first$ok)
  expect_gt(first$bytes, initial)
  expect_gte(first$stream_budget_bytes, first_pending)
  first_allowance <- first$stream_budget_bytes
  expect_lte(
    first_allowance + max(advertised, first_allowance),
    first$bytes
  )

  second <- acquisition_storage_reservation_update(
    catalog,
    claim$job_id,
    claim$lease_token,
    bytes_written = first_pending,
    advertised_bytes = advertised,
    content_addressed = TRUE,
    pending_bytes = 4 * 1024^2
  )
  expect_true(second$ok)
  expect_gte(second$stream_budget_bytes, 4 * 1024^2)
  second_allowance <- second$stream_budget_bytes
  expect_lte(
    second_allowance + max(advertised, first_pending + second_allowance),
    second$bytes
  )
  acquisition_queue_complete(catalog, claim$job_id, claim$lease_token, status = "retryable")
})

test_that("a renewed lease remains valid after the original claim expiry", {
  catalog <- storage_catalog()
  on.exit(catalog$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  root <- file.path(withr::local_tempdir(), "renewed-guard-root")
  acquisition_storage_register_root(catalog, "00-renewed", root, admin, reserve_bytes = 1L)
  source <- acquisition_catalog_register_source(catalog, "nl", "portal.example", "renewed")
  document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "renewed",
    attachment_url = "https://portal.example/renewed.bin"
  )
  acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id)
  claim <- acquisition_queue_claim(catalog, "renewed-worker", lease_seconds = 1L)
  expect_true(claim$ok)
  # Keep the original value stale on purpose. The queue's heartbeat updates the
  # authoritative database lease before the downloader invokes its guard.
  claim$lease_expires_at <- "2000-01-01T00:00:00Z"
  claim$heartbeat <- function(seconds = 10L) {
    acquisition_queue_heartbeat(catalog, claim$job_id, claim$lease_token, lease_seconds = seconds)
  }
  observed <- new.env(parent = emptyenv())
  fake_download <- function(
    url, country, document_id, cache_dir, max_file_size_mb, overwrite = FALSE,
    storage_guard
  ) {
    observed$decision <- storage_guard(
      0,
      advertised_size = 1,
      phase = "stream",
      pending_bytes = 1
    )
    tibble::tibble(
      url = url,
      local_path = NA_character_,
      size_bytes = NA_real_,
      sha256 = NA_character_,
      status = "failed",
      reason = "renewed lease fixture"
    )
  }
  testthat::local_mocked_bindings(download_to_cache = fake_download, .package = "planscanR")
  result <- acquisition_queue_default_download(claim, catalog)
  expect_identical(result$status, "retryable")
  expect_true(isTRUE(observed$decision$ok))
  acquisition_queue_complete(catalog, claim$job_id, claim$lease_token, status = "retryable")
})

test_that("the queued guard grows past its cache and rejects a replacement lease", {
  catalog <- storage_catalog()
  on.exit(catalog$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  root <- file.path(withr::local_tempdir(), "cached-guard-root")
  acquisition_storage_register_root(catalog, "00-cached", root, admin, reserve_bytes = 1L)
  source <- acquisition_catalog_register_source(catalog, "nl", "portal.example", "cached")
  document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "cached",
    attachment_url = "https://portal.example/cached.bin"
  )
  acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id)
  claim <- acquisition_queue_claim(catalog, "cached-worker", lease_seconds = 60L)
  expect_true(claim$ok)
  observed <- new.env(parent = emptyenv())
  fake_download <- function(
    url, country, document_id, cache_dir, max_file_size_mb, overwrite = FALSE,
    storage_guard
  ) {
    first <- storage_guard(0, advertised_size = 1, pending_bytes = 8 * 1024^2)
    observed$first <- first
    cached <- storage_guard(8 * 1024^2, advertised_size = 1, pending_bytes = 1 * 1024^2)
    observed$cached <- cached
    DBI::dbExecute(
      catalog$conn,
      "UPDATE acquisition_jobs SET lease_expires_at = ? WHERE job_id = ?",
      params = list("2000-01-01T00:00:00Z", claim$job_id)
    )
    replacement <- acquisition_queue_claim(catalog, "replacement-worker", lease_seconds = 60L)
    observed$replacement <- replacement
    observed$stale <- storage_guard(8 * 1024^2, advertised_size = 1, pending_bytes = 1 * 1024^2)
    tibble::tibble(
      url = url,
      local_path = NA_character_,
      size_bytes = NA_real_,
      sha256 = NA_character_,
      status = "failed",
      reason = observed$stale$reason
    )
  }
  testthat::local_mocked_bindings(download_to_cache = fake_download, .package = "planscanR")
  result <- acquisition_queue_default_download(claim, catalog)
  expect_identical(result$status, "retryable")
  expect_true(isTRUE(observed$first$ok))
  expect_true(isTRUE(observed$cached$ok))
  expect_true(isTRUE(observed$replacement$ok))
  expect_false(isTRUE(observed$stale$ok))
  expect_identical(observed$stale$reason, "lease-stale")
  held <- acquisition_storage_reservations(catalog, state = "held")
  expect_identical(held$lease_token[[1L]], observed$replacement$lease_token)
  expect_equal(held$written_bytes[[1L]], 0)
  acquisition_queue_complete(
    catalog,
    observed$replacement$job_id,
    observed$replacement$lease_token,
    status = "retryable"
  )
})

test_that("download estimates remain advisory above 50 MiB", {
  skip_if_not_installed("filelock")
  catalog <- storage_catalog()
  on.exit(catalog$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  root <- file.path(withr::local_tempdir(), "known-size-root")
  capacity <- planscanR::storage_capacity(
    planscanR::storage_root_validate(root, create = TRUE, writable = TRUE)
  )
  estimate <- 200 * 1024^2
  skip_if(
    capacity$free_bytes <= estimate + 1024^2,
    "The test filesystem has too little headroom for the advisory-estimate check."
  )
  acquisition_storage_register_root(catalog, "00-known-size", root, admin, reserve_bytes = 1L)
  source <- acquisition_catalog_register_source(catalog, "nl", "portal.example", "known-size")
  document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "known-size",
    attachment_url = "https://portal.example/known-size.bin"
  )
  acquisition_queue_enqueue(
    catalog,
    "download",
    "document",
    document$document_ref_id,
    estimated_bytes = estimate
  )
  claim <- acquisition_queue_claim(catalog, "known-size-worker")
  expect_true(claim$ok)
  expect_lt(claim$storage_reservation_bytes, estimate)
  expect_lte(claim$storage_reservation_bytes, planscanR::storage_transfer_cap_bytes())
  observed <- new.env(parent = emptyenv())
  fake_download <- function(url, country, document_id, cache_dir, max_file_size_mb, overwrite = FALSE) {
    observed$cap <- max_file_size_mb * 1024^2
    tibble::tibble(
      url = url,
      local_path = NA_character_,
      size_bytes = NA_real_,
      sha256 = NA_character_,
      status = "failed",
      reason = "offline known-size fixture"
    )
  }
  testthat::local_mocked_bindings(download_to_cache = fake_download, .package = "planscanR")
  withr::local_options(list(planscanR.max_file_size_mb = 1))
  result <- acquisition_queue_default_download(claim, catalog)
  expect_identical(result$status, "retryable")
  expect_equal(observed$cap, claim$storage_reservation_bytes)
  acquisition_queue_complete(catalog, claim$job_id, claim$lease_token, status = "retryable")
})

test_that("reservations permit two workers on separate connections", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  catalog_a <- acquisition_catalog_open(path)
  on.exit(catalog_a$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  root <- file.path(withr::local_tempdir(), "downloads-a")
  target_root <- file.path(withr::local_tempdir(), "downloads-b")
  dir.create(root, recursive = TRUE)
  dir.create(target_root, recursive = TRUE)
  free <- planscanR::storage_free_bytes(root)
  target_free <- planscanR::storage_free_bytes(target_root)
  headroom <- 100 * 1024^2
  target_headroom <- 90 * 1024^2
  estimate <- 50 * 1024^2
  skip_if(
    min(free, target_free) <= 2 * headroom,
    "The test filesystem has too little free space for a stable reservation margin."
  )
  acquisition_storage_register_root(
    catalog_a,
    "00-downloads",
    root,
    admin,
    reserve_bytes = free - headroom
  )
  acquisition_storage_register_root(
    catalog_a,
    "01-target",
    target_root,
    admin,
    purpose = "staging",
    reserve_bytes = target_free - target_headroom
  )
  source_one <- acquisition_catalog_register_source(catalog_a, "nl", "portal.example", "one")
  document_one <- acquisition_catalog_register_document(catalog_a, source_one$source_ref_id, native_attachment_id = "one")
  acquisition_queue_enqueue(catalog_a, "download", "document", document_one$document_ref_id, estimated_bytes = estimate)

  catalog_b <- acquisition_catalog_open(path)
  on.exit(catalog_b$close(), add = TRUE)
  first <- acquisition_queue_claim(catalog_a, "worker-a")
  expect_true(first$ok)
  relocation <- acquisition_storage_enqueue_relocation(
    catalog_a,
    "00-downloads",
    "01-target",
    admin,
    estimated_bytes = estimate
  )
  second <- acquisition_queue_claim(catalog_b, "worker-b")
  expect_true(second$ok)
  expect_equal(sum(acquisition_storage_reservations(catalog_b)$state == "held"), 2L)
  acquisition_queue_complete(catalog_a, first$job_id, first$lease_token, status = "retryable")
  acquisition_queue_complete(catalog_b, second$job_id, second$lease_token, status = "retryable")
})

test_that("relocation is an audited queued job and does not move files inline", {
  catalog <- storage_catalog()
  on.exit(catalog$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  source_root <- file.path(withr::local_tempdir(), "source")
  second_source_root <- file.path(withr::local_tempdir(), "source-two")
  target_root <- file.path(withr::local_tempdir(), "target")
  acquisition_storage_register_root(catalog, "source", source_root, admin, purpose = "downloads")
  acquisition_storage_register_root(catalog, "source-two", second_source_root, admin, purpose = "downloads")
  acquisition_storage_register_root(catalog, "target", target_root, admin, purpose = "staging")
  marker <- file.path(source_root, "keep.txt")
  writeLines("keep", marker)
  second_marker <- file.path(second_source_root, "keep.txt")
  writeLines("keep", second_marker)

  relocation <- acquisition_storage_enqueue_relocation(
    catalog, "source", "target", admin,
    estimated_bytes = 1L
  )
  replay <- acquisition_storage_enqueue_relocation(
    catalog, "source", "target", admin,
    estimated_bytes = 1L
  )
  second_relocation <- acquisition_storage_enqueue_relocation(
    catalog, "source-two", "target", admin,
    estimated_bytes = 1L
  )
  expect_true(relocation$job_id %in% acquisition_queue_jobs(catalog)$job_id)
  expect_identical(replay$status, "replayed")
  expect_identical(replay$job_id, relocation$job_id)
  expect_true(second_relocation$job_id %in% acquisition_queue_jobs(catalog)$job_id)
  expect_false(identical(relocation$job_id, second_relocation$job_id))
  expect_equal(sum(acquisition_queue_jobs(catalog)$kind == "migrate_storage"), 2L)
  expect_true(file.exists(marker))
  expect_true(file.exists(second_marker))
  expect_true(all(acquisition_storage_relocations(catalog)$state == "queued"))
})

test_that("storage migration preserves populated v3 and campaign queue rows", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  conn <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(conn, "PRAGMA journal_mode = WAL")
  DBI::dbExecute(conn, "PRAGMA foreign_keys = ON")
  planscanR.screen:::acquisition_catalog_migrate(conn, busy_timeout = 5000L)
  now <- planscanR.screen:::acquisition_catalog_now()
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO acquisition_sources (source_ref_id, country, portal, native_project_id, created_at)",
      "VALUES ('source-v3', 'nl', 'portal.example', 'project-v3', ?)"
    ),
    params = list(now)
  )
  for (column in c("campaign_id TEXT", "campaign_item_key TEXT", "priority_rule_id TEXT", "priority_explanation TEXT")) {
    name <- strsplit(column, " ", fixed = TRUE)[[1L]][[1L]]
    if (!name %in% DBI::dbListFields(conn, "acquisition_jobs")) {
      DBI::dbExecute(conn, paste("ALTER TABLE acquisition_jobs ADD COLUMN", column))
    }
  }
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO acquisition_jobs (job_id, kind, target_type, target_ref_id, idempotency_key,",
      "host, priority, state, available_at, attempt_count, max_attempts, lease_token, worker_id,",
      "lease_expires_at, last_error_category, last_error, completed_at, created_at, updated_at,",
      "campaign_id, campaign_item_key, priority_rule_id, priority_explanation)",
      "VALUES ('job-v3', 'discover', 'source', 'source-v3', 'v3-key', ?, 7, 'running', ?, 1, 3,",
      "'token-v3', 'worker-v3', ?, NULL, NULL, NULL, ?, ?, 'campaign-v4', 'item-v4', 'rule-v4', 'reason-v4')"
    ),
    params = list("portal.example", now, now, now, now)
  )
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO acquisition_job_attempts (job_id, attempt_no, lease_token, worker_id, host,",
      "claimed_at, lease_expires_at, heartbeat_at, outcome) VALUES ('job-v3', 1, 'token-v3',",
      "'worker-v3', ?, ?, ?, ?, 'running')"
    ),
    params = list("portal.example", now, now, now)
  )
  DBI::dbDisconnect(conn)

  catalog <- acquisition_catalog_open(path)
  on.exit(catalog$close(), add = TRUE)
  jobs <- acquisition_queue_jobs(catalog)
  attempts <- acquisition_queue_attempts(catalog)
  expect_identical(jobs$job_id, "job-v3")
  expect_identical(jobs$campaign_id, "campaign-v4")
  expect_identical(jobs$priority_rule_id, "rule-v4")
  expect_identical(attempts$lease_token, "token-v3")
  schema_sql <- DBI::dbGetQuery(
    catalog$conn,
    "SELECT sql FROM sqlite_master WHERE name = 'acquisition_jobs'"
  )$sql[[1L]]
  expect_true(all(vapply(
    c("migrate_storage", "storage_root", "estimated_bytes"),
    grepl,
    logical(1),
    x = schema_sql,
    fixed = TRUE
  )))
  expect_equal(nrow(DBI::dbGetQuery(catalog$conn, "PRAGMA foreign_key_check")), 0L)
})

test_that("guarded queue downloads honor an explicit MiB cap without disabling the guard", {
  skip_if_not_installed("filelock")
  catalog <- storage_catalog()
  on.exit(catalog$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  root <- file.path(withr::local_tempdir(), "guarded-root")
  acquisition_storage_register_root(catalog, "00-guarded", root, admin, reserve_bytes = 1L)
  source <- acquisition_catalog_register_source(catalog, "nl", "portal.example", "guarded")
  document <- acquisition_catalog_register_document(
    catalog,
    source$source_ref_id,
    native_attachment_id = "guarded",
    attachment_url = "https://portal.example/guarded.bin"
  )
  job <- acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id)
  claim <- acquisition_queue_claim(catalog, "guarded-worker")
  expect_true(claim$ok)
  observed <- new.env(parent = emptyenv())
  fake_download <- function(
    url, country, document_id, cache_dir, max_file_size_mb, overwrite = FALSE,
    storage_guard
  ) {
    observed$cap <- max_file_size_mb
    observed$guard <- storage_guard
    body <- charToRaw("underreported body")
    first <- storage_guard(0, advertised_size = 1, phase = "stream", pending_bytes = 4)
    expect_true(first$ok)
    storage_guard(4, advertised_size = 1, phase = "stream", pending_bytes = length(body) - 4L)
    destination <- planscanR::cache_path(url, country, document_id, cache_dir = cache_dir)
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    writeBin(body, destination)
    sha <- planscanR:::file_sha256(destination)
    planscanR:::write_download_receipt(destination, url, "application/octet-stream", length(body), sha)
    storage_guard(length(body), advertised_size = 1, phase = "publish", pending_bytes = 0)
    tibble::tibble(
      url = url,
      local_path = destination,
      size_bytes = length(body),
      sha256 = sha,
      status = "downloaded",
      reason = NA_character_
    )
  }
  testthat::local_mocked_bindings(download_to_cache = fake_download, .package = "planscanR")
  result <- acquisition_queue_default_download(claim, catalog, max_file_size_mb = 50)
  expect_identical(result$status, "succeeded")
  expect_identical(observed$cap, 50)
  expect_true(is.function(observed$guard))
  acquisition_queue_complete(catalog, claim$job_id, claim$lease_token, status = "succeeded", result = result)
  expect_identical(acquisition_storage_reservations(catalog)$state[[1L]], "released")
  expect_identical(acquisition_queue_jobs(catalog)$state[[1L]], "succeeded")
  expect_true(job$job_id %in% acquisition_queue_jobs(catalog)$job_id)
})


test_that("oversized guarded downloads are deferred for later retry", {
  skip_if_not_installed("filelock")
  catalog <- storage_catalog()
  on.exit(catalog$close(), add = TRUE)
  root <- file.path(withr::local_tempdir(), "capped-root")
  acquisition_storage_register_root(catalog, "00-capped", root,
                                   curation_actor("admin", roles = "admin"), reserve_bytes = 1L)
  source <- acquisition_catalog_register_source(catalog, "nl", "portal.example", "capped")
  document <- acquisition_catalog_register_document(catalog, source$source_ref_id,
    native_attachment_id = "large", attachment_url = "https://portal.example/large.pdf")
  acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id)
  claim <- acquisition_queue_claim(catalog, "capped-worker")
  fake_download <- function(url, country, document_id, cache_dir, max_file_size_mb,
                            overwrite = FALSE, storage_guard) {
    expect_identical(max_file_size_mb, 50)
    expect_identical(getOption("planscanR.max_file_size_mb"), 50)
    expect_true(is.function(storage_guard))
    tibble::tibble(status = "skipped_size", reason = "size exceeds 50 MiB")
  }
  testthat::local_mocked_bindings(download_to_cache = fake_download, .package = "planscanR")
  result <- acquisition_queue_default_download(claim, catalog, max_file_size_mb = 50)
  expect_identical(result$status, "blocked")
  expect_identical(result$error_category, "download-size-limit")
  acquisition_queue_complete(catalog, claim$job_id, claim$lease_token,
                             status = result$status, result = result,
                             error_category = result$error_category,
                             error_message = result$error_message)
  expect_identical(acquisition_queue_jobs(catalog)$state[[1L]], "blocked")
  expect_identical(acquisition_storage_reservations(catalog)$state[[1L]], "released")
})
