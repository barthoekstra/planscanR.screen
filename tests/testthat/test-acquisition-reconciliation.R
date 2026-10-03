skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

reconcile_source_document <- function(catalog, portal = "portal.example", native_project_id = "project-1") {
  source <- acquisition_catalog_register_source(
    catalog,
    country = "nl",
    portal = portal,
    native_project_id = native_project_id,
    source_url = paste0("https://", portal, "/", native_project_id)
  )
  document <- acquisition_catalog_register_document(
    catalog,
    source_ref_id = source$source_ref_id,
    native_attachment_id = "attachment-1",
    attachment_url = paste0("https://", portal, "/", native_project_id, "/file.pdf")
  )
  list(source = source, document = document)
}

reconcile_sidecar_record <- function(root, portal, native_project_id, url, status = "pending") {
  downloads <- tibble::tibble(
    url = url,
    local_path = NA_character_,
    status = status,
    size_bytes = NA_real_,
    sha256 = NA_character_,
    reason = NA_character_
  )
  record <- tibble::tibble(
    country = "nl",
    source_portal = portal,
    document_id = native_project_id,
    url = paste0("https://", portal, "/", native_project_id),
    retrieved_at = as.POSIXct("2026-10-03 12:00:00", tz = "UTC"),
    attachment_urls = list(url),
    local_path = list(NA_character_),
    title = "Fixture project",
    summary = "Fixture summary",
    relevance_score_wind = 0.88,
    relevance_model = "fixture-relevance",
    class_label = "fresh",
    class_score = 0.91,
    class_relevant = TRUE,
    class_model = "fixture-classifier",
    translation_title_en = "Fresh translated title",
    translation_summary_en = "Fresh translated summary",
    translation_src_lang = "nld_Latn",
    translation_service = "fixture-translation",
    download_status = list(downloads)
  )
  planscanR::write_record_sidecar(record, downloads = downloads, root = root)
  invisible(record)
}

reconcile_publish_fixture <- function(root, source_ref_id, document_ref_id, url, text = "verified fixture") {
  cache_document_id <- acquisition_queue_cache_document_id(source_ref_id, document_ref_id)
  path <- planscanR::cache_path(url, "nl", cache_document_id, cache_dir = root)
  writeBin(charToRaw(text), path)
  size <- file.info(path)$size
  sha <- unclass(as.character(openssl::sha256(readBin(path, what = "raw", n = size))))
  receipt <- planscanR::reconcile_cached_download(
    path,
    expected_size_bytes = size,
    expected_sha256 = sha,
    url = url,
    repair = TRUE
  )
  expect_identical(receipt$status, "repaired")
  list(
    path = path,
    size_bytes = size,
    sha256 = sha,
    local_path = path,
    relative_path = acquisition_queue_relative_path(path, root),
    document_ref_id = document_ref_id,
    root_id = "planscan-cache",
    verification_status = "verified",
    provenance = "planscanR-download",
    url = url,
    download_status = "downloaded"
  )
}

test_that("published receipt is reused after a lease dies before catalog completion", {
  withr::with_tempdir({
    cache <- file.path(getwd(), "cache")
    db <- file.path(getwd(), "acquisition.sqlite")
    withr::local_options(planscanR.cache_dir = cache)
    catalog <- acquisition_catalog_open(db)
    on.exit(catalog$close(), add = TRUE)
    refs <- reconcile_source_document(catalog)
    url <- "https://portal.example/project-1/file.pdf"
    reconcile_sidecar_record(cache, "portal.example", "project-1", url)
    counters <- new.env(parent = emptyenv())
    counters$fetches <- 0L
    fake_download <- function(url, country, document_id, cache_dir, overwrite = FALSE) {
      path <- planscanR::cache_path(url, country, document_id, cache_dir = cache_dir)
      existed <- file.exists(path) && identical(
        planscanR::reconcile_cached_download(path)$status,
        "verified"
      )
      if (!existed) {
        counters$fetches <- counters$fetches + 1L
        writeBin(charToRaw("crash-safe fixture"), path)
        size <- file.info(path)$size
        sha <- unclass(as.character(openssl::sha256(readBin(path, "raw", n = size))))
        expect_identical(
          planscanR::reconcile_cached_download(
            path,
            expected_size_bytes = size,
            expected_sha256 = sha,
            url = url,
            repair = TRUE
          )$status,
          "repaired"
        )
      }
      size <- file.info(path)$size
      sha <- unclass(as.character(openssl::sha256(readBin(path, "raw", n = size))))
      tibble::tibble(
        url = url,
        local_path = path,
        status = if (existed) "exists" else "downloaded",
        size_bytes = size,
        sha256 = sha,
        reason = NA_character_
      )
    }
    testthat::local_mocked_bindings(download_to_cache = fake_download, .package = "planscanR")

    job <- acquisition_queue_enqueue(catalog, "download", "document", refs$document$document_ref_id)
    first <- acquisition_queue_claim(catalog, "publisher-a", lease_seconds = 60L)
    first_result <- acquisition_queue_default_download(first, catalog)
    expect_identical(first_result$status, "succeeded")
    DBI::dbExecute(
      catalog$conn,
      "UPDATE acquisition_jobs SET lease_expires_at = ? WHERE job_id = ?",
      params = list("2000-01-01T00:00:00Z", job$job_id)
    )

    second <- acquisition_queue_claim(catalog, "publisher-b", lease_seconds = 60L)
    second_result <- acquisition_queue_default_download(second, catalog)
    expect_identical(second_result$status, "succeeded")
    expect_identical(second_result$download_status, "cached")
    expect_equal(counters$fetches, 1L)
    expect_true(acquisition_queue_complete(
      catalog,
      second$job_id,
      second$lease_token,
      status = second_result$status,
      result = second_result
    )$ok)
    file_row <- acquisition_catalog_files(catalog, refs$document$document_ref_id)
    expect_identical(file_row$verification_status, "verified")
    sidecar <- planscanR::read_record_sidecar(
      file.path(cache, "files", "nl", "project-1", "project-1.meta.json")
    )
    expect_identical(sidecar$class_label, "fresh")
    expect_identical(sidecar$translation_title_en, "Fresh translated title")
    expect_identical(sidecar$relevance_score_wind, 0.88)
    expect_identical(sidecar$download_status[[1]]$status, "cached")
  })
})

test_that("an interrupted sidecar sync retries while retaining verified bytes", {
  withr::with_tempdir({
    cache <- file.path(getwd(), "cache")
    db <- file.path(getwd(), "acquisition.sqlite")
    withr::local_options(planscanR.cache_dir = cache)
    catalog <- acquisition_catalog_open(db)
    on.exit(catalog$close(), add = TRUE)
    refs <- reconcile_source_document(catalog)
    url <- "https://portal.example/project-1/file.pdf"
    reconcile_sidecar_record(cache, "portal.example", "project-1", url)
    counters <- new.env(parent = emptyenv())
    counters$fetches <- 0L
    fake_download <- function(url, country, document_id, cache_dir, overwrite = FALSE) {
      path <- planscanR::cache_path(url, country, document_id, cache_dir = cache_dir)
      existed <- file.exists(path) && identical(
        planscanR::reconcile_cached_download(path)$status,
        "verified"
      )
      if (!existed) {
        counters$fetches <- counters$fetches + 1L
        writeBin(charToRaw("retry-safe fixture"), path)
        size <- file.info(path)$size
        sha <- unclass(as.character(openssl::sha256(readBin(path, "raw", n = size))))
        planscanR::reconcile_cached_download(
          path,
          expected_size_bytes = size,
          expected_sha256 = sha,
          url = url,
          repair = TRUE
        )
      }
      size <- file.info(path)$size
      sha <- unclass(as.character(openssl::sha256(readBin(path, "raw", n = size))))
      tibble::tibble(
        url = url,
        local_path = path,
        status = if (existed) "exists" else "downloaded",
        size_bytes = size,
        sha256 = sha,
        reason = NA_character_
      )
    }
    testthat::local_mocked_bindings(download_to_cache = fake_download, .package = "planscanR")

    job <- acquisition_queue_enqueue(
      catalog,
      "download",
      "document",
      refs$document$document_ref_id,
      max_attempts = 3L
    )
    first <- acquisition_queue_claim(catalog, "sync-a", lease_seconds = 60L)
    interrupted <- acquisition_queue_default_download(
      first,
      catalog,
      sidecar_sync_fn = function(...) stop("fixture sidecar interruption", call. = FALSE)
    )
    expect_identical(interrupted$status, "retryable")
    expect_identical(interrupted$error_category, "sidecar-sync")
    expect_identical(interrupted$verification_status, "verified")
    expect_true(acquisition_queue_complete(
      catalog,
      first$job_id,
      first$lease_token,
      status = interrupted$status,
      result = interrupted
    )$ok)
    file_row <- acquisition_catalog_files(catalog, refs$document$document_ref_id)
    expect_identical(file_row$verification_status, "verified")
    DBI::dbExecute(
      catalog$conn,
      "UPDATE acquisition_jobs SET available_at = ? WHERE job_id = ?",
      params = list("2000-01-01T00:00:00Z", job$job_id)
    )

    second <- acquisition_queue_claim(catalog, "sync-b", lease_seconds = 60L)
    retried <- acquisition_queue_default_download(second, catalog)
    expect_identical(retried$status, "succeeded")
    expect_identical(retried$download_status, "cached")
    expect_equal(counters$fetches, 1L)
    expect_true(acquisition_queue_complete(
      catalog,
      second$job_id,
      second$lease_token,
      status = retried$status,
      result = retried
    )$ok)
    sidecar <- planscanR::read_record_sidecar(
      file.path(cache, "files", "nl", "project-1", "project-1.meta.json")
    )
    expect_identical(sidecar$class_label, "fresh")
    expect_identical(sidecar$translation_summary_en, "Fresh translated summary")
    expect_identical(sidecar$relevance_score_wind, 0.88)
    expect_identical(sidecar$download_status[[1]]$status, "cached")
  })
})

test_that("same-country portal collisions block sidecar writes", {
  withr::with_tempdir({
    cache <- file.path(getwd(), "cache")
    db <- file.path(getwd(), "acquisition.sqlite")
    withr::local_options(planscanR.cache_dir = cache)
    catalog <- acquisition_catalog_open(db)
    on.exit(catalog$close(), add = TRUE)
    first <- reconcile_source_document(catalog, portal = "portal-a.example")
    second <- reconcile_source_document(catalog, portal = "portal-b.example")
    url <- "https://portal-b.example/project-1/file.pdf"
    reconcile_sidecar_record(cache, "portal-a.example", "project-1", "https://portal-a.example/project-1/file.pdf")
    result <- reconcile_publish_fixture(
      cache,
      second$source$source_ref_id,
      second$document$document_ref_id,
      url,
      text = "portal b bytes"
    )

    synced <- acquisition_queue_sync_sidecar(
      catalog,
      second$document$document_ref_id,
      result,
      cache_root = cache
    )
    expect_identical(synced$status, "blocked")
    expect_identical(synced$error_category, "sidecar-identity-conflict")
    sidecar <- planscanR::read_record_sidecar(
      file.path(cache, "files", "nl", "project-1", "project-1.meta.json")
    )
    expect_identical(sidecar$source_portal, "portal-a.example")
    expect_identical(sidecar$download_status[[1]]$status, "pending")
  })
})

test_that("reconciliation audits old partials and refuses unverified claims", {
  withr::with_tempdir({
    cache <- file.path(getwd(), "cache")
    db <- file.path(getwd(), "acquisition.sqlite")
    withr::local_options(planscanR.cache_dir = cache)
    catalog <- acquisition_catalog_open(db)
    on.exit(catalog$close(), add = TRUE)
    refs <- reconcile_source_document(catalog)
    reconcile_sidecar_record(cache, "portal.example", "project-1", "https://portal.example/project-1/file.pdf")
    old_dir <- file.path(cache, "files", "nl", "project-1--legacy")
    dir.create(old_dir, recursive = TRUE, showWarnings = FALSE)
    partial <- file.path(old_dir, "report.pdf.part")
    legacy <- file.path(old_dir, "legacy.pdf")
    writeBin(charToRaw("partial"), partial)
    writeBin(charToRaw("legacy"), legacy)
    claim <- acquisition_catalog_register_file(
      catalog,
      refs$document$document_ref_id,
      root_id = "planscan-cache",
      relative_path = "files/nl/project-1--legacy/legacy.pdf",
      sha256 = unclass(as.character(openssl::sha256(readBin(legacy, "raw", n = file.info(legacy)$size)))),
      size_bytes = file.info(legacy)$size,
      verification_status = "unverified",
      provenance = "legacy-sidecar-v1"
    )

    audit <- acquisition_queue_audit_cache(cache, country = "nl")
    expect_true(any(audit$status == "partial"))
    expect_true(any(audit$status == "unverified"))
    reconciled <- acquisition_queue_reconcile_file(catalog, claim$file_ref_id, cache_root = cache)
    expect_identical(reconciled$status, "blocked")
    expect_identical(reconciled$error_category, "unverified-file-claim")
    legacy_audit <- audit[audit$path == legacy, , drop = FALSE]
    expect_identical(legacy_audit$status, "unverified")
    expect_false(file.exists(legacy_audit$receipt_path))
  })
})

test_that("receipt-only reconciliation recovers a crash on the final allowed attempt", {
  cache <- withr::local_tempdir()
  withr::local_options(planscanR.cache_dir = cache)
  catalog <- acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
  on.exit(catalog$close(), add = TRUE)
  refs <- reconcile_source_document(catalog)
  url <- "https://portal.example/project-1/file.pdf"
  reconcile_sidecar_record(cache, "portal.example", "project-1", url)
  job <- acquisition_queue_enqueue(catalog, "download", "document", refs$document$document_ref_id, max_attempts = 1L)
  claim <- acquisition_queue_claim(catalog, "crashing-publisher")
  reconcile_publish_fixture(cache, refs$source$source_ref_id, refs$document$document_ref_id, url)
  expect_identical(acquisition_queue_reconcile_job(catalog, job$job_id, cache)$status, "not-terminal")
  DBI::dbExecute(catalog$conn, "UPDATE acquisition_jobs SET lease_expires_at = '2000-01-01T00:00:00Z' WHERE job_id = ?", params = list(job$job_id))
  acquisition_queue_recover(catalog)
  expect_identical(acquisition_queue_jobs(catalog)$state, "failed")
  expect_equal(nrow(acquisition_catalog_files(catalog)), 0L)
  testthat::local_mocked_bindings(download_to_cache = function(...) stop("Receipt reconciliation must not fetch"), .package = "planscanR")
  recovered <- acquisition_queue_reconcile_job(catalog, job$job_id, cache)
  expect_true(recovered$ok)
  expect_identical(recovered$job_state, "succeeded")
  expect_equal(acquisition_queue_jobs(catalog)$max_attempts, 1L)
  expect_setequal(acquisition_queue_attempts(catalog)$outcome, c("expired", "succeeded"))
  expect_true(any(acquisition_queue_attempts(catalog)$worker_id == "receipt-only-reconciliation"))
  expect_identical(acquisition_catalog_files(catalog)$verification_status, "verified")
  expect_identical(acquisition_queue_complete(catalog, job$job_id, claim$lease_token)$status, "stale")
  sidecar <- planscanR::read_record_sidecar(file.path(cache, "files", "nl", "project-1", "project-1.meta.json"))
  expect_identical(sidecar$download_status[[1]]$status, "cached")
  expect_identical(sidecar$translation_title_en, "Fresh translated title")
})

test_that("receipt-only recovery refuses missing proof without changing terminal state", {
  cache <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
  on.exit(catalog$close(), add = TRUE)
  refs <- reconcile_source_document(catalog)
  job <- acquisition_queue_enqueue(catalog, "download", "document", refs$document$document_ref_id, max_attempts = 1L)
  claim <- acquisition_queue_claim(catalog, "failed-publisher")
  acquisition_queue_complete(catalog, job$job_id, claim$lease_token, status = "failed")
  before <- acquisition_queue_attempts(catalog)
  expect_identical(acquisition_queue_reconcile_job(catalog, job$job_id, cache)$error_category, "missing-publication")
  path <- planscanR::cache_path("https://portal.example/project-1/file.pdf", "nl",
    acquisition_queue_cache_document_id(refs$source$source_ref_id, refs$document$document_ref_id),
    cache_dir = cache
  )
  writeBin(charToRaw("legacy unverified bytes"), path)
  expect_identical(acquisition_queue_reconcile_job(catalog, job$job_id, cache)$error_category, "unverified-publication")
  expect_identical(acquisition_queue_attempts(catalog), before)
  expect_identical(acquisition_queue_jobs(catalog)$state, "failed")
})

test_that("a newly discovered attachment is appended to an identity-matched sidecar", {
  cache <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
  on.exit(catalog$close(), add = TRUE)
  refs <- reconcile_source_document(catalog)
  url <- "https://portal.example/project-1/file.pdf"
  reconcile_sidecar_record(cache, "portal.example", "project-1", "https://portal.example/old.pdf")
  publication <- reconcile_publish_fixture(cache, refs$source$source_ref_id, refs$document$document_ref_id, url)
  result <- acquisition_queue_sync_sidecar(catalog, refs$document$document_ref_id, publication, cache)
  expect_identical(result$status, "succeeded")
  sidecar <- planscanR::read_record_sidecar(file.path(cache, "files", "nl", "project-1", "project-1.meta.json"))
  expect_setequal(sidecar$download_status[[1]]$url, c("https://portal.example/old.pdf", url))
  expect_identical(sidecar$class_label, "fresh")
})
