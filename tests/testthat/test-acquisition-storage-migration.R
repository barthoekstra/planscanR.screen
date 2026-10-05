skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

migration_catalog <- function() {
  acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
}

test_that("default relocation handler verifies and registers a shared object per document", {
  catalog <- migration_catalog()
  on.exit(catalog$close(), add = TRUE)
  admin <- curation_actor("admin", roles = "admin")
  cache <- file.path(withr::local_tempdir(), "cache")
  source_root <- file.path(withr::local_tempdir(), "source")
  target_root <- file.path(withr::local_tempdir(), "target")
  dir.create(cache, recursive = TRUE)
  acquisition_storage_register_root(catalog, "source", source_root, admin)
  acquisition_storage_register_root(catalog, "target", target_root, admin, purpose = "staging")
  withr::local_options(list(
    planscanR.cache_dir = cache,
    planscanR.storage_roots = c(source = source_root, target = target_root)
  ))

  bytes <- charToRaw("%PDF-1.4 shared content")
  sha <- digest::digest(bytes, algo = "sha256", serialize = FALSE)
  source_path <- file.path(source_root, "files", "nl", "project-1", "report.pdf")
  dir.create(dirname(source_path), recursive = TRUE)
  writeBin(bytes, source_path)
  source_url <- "https://portal.example/project-1/report.pdf"
  planscanR:::write_download_receipt(source_path, source_url, "application/pdf", length(bytes), sha)

  documents <- lapply(c("project-1", "project-2", sprintf("project-%02d", 3:26)), function(project) {
    source <- acquisition_catalog_register_source(catalog, "nl", "portal.example", project)
    document <- acquisition_catalog_register_document(catalog, source$source_ref_id,
      native_attachment_id = "report", attachment_url = paste0("https://portal.example/", project, "/report.pdf"))
    acquisition_catalog_register_file(catalog, document$document_ref_id, "source",
      "files/nl/project-1/report.pdf", sha, length(bytes), "verified", "test-import", "report.pdf")
    list(source = source, document = document)
  })
  relocation <- acquisition_storage_enqueue_relocation(catalog, "source", "target", admin,
    estimated_bytes = length(bytes) * 4)
  reservation <- acquisition_catalog_transaction(catalog, function(conn) {
    acquisition_storage_try_reserve_connection(conn, list(kind = "migrate_storage",
      job_id = relocation$job_id, target_type = "storage_root", target_ref_id = "target"))
  })
  expect_true(reservation$ok)
  acquisition_storage_mark_running_connection(catalog$conn, relocation$job_id)
  job <- list(
    job_id = relocation$job_id,
    storage_reservation_bytes = length(bytes) * 4
  )
  low_reserve <- acquisition_queue_default_migrate_storage(
    utils::modifyList(job, list(storage_reservation_bytes = length(bytes))), catalog)
  expect_identical(low_reserve$error_category, "migration-reservation-too-small")
  expect_false(dir.exists(file.path(target_root, "objects")))
  first_batch <- acquisition_queue_default_migrate_storage(job, catalog)
  expect_identical(first_batch$status, "retryable")
  expect_identical(first_batch$remaining, 1L)
  expect_true(file.exists(file.path(cache, "projects", documents[[26]]$source$source_ref_id,
    "manifest.json")))
  acquisition_storage_mark_complete_connection(catalog$conn, relocation$job_id, "retry_wait")
  acquisition_storage_mark_running_connection(catalog$conn, relocation$job_id)
  result <- acquisition_queue_default_migrate_storage(job, catalog)

  object_relative <- file.path("objects", "sha256", substr(sha, 1, 2), substr(sha, 3, 4), sha)
  object_path <- file.path(target_root, object_relative)
  expect_identical(result$status, "succeeded")
  expect_true(file.exists(object_path))
  expect_identical(digest::digest(file = object_path, algo = "sha256"), sha)
  claims <- DBI::dbGetQuery(catalog$conn,
    "SELECT document_ref_id, root_id, relative_path FROM acquisition_files WHERE root_id = 'target'")
  expect_equal(nrow(claims), 26L)
  expect_setequal(claims$document_ref_id, vapply(documents, function(x) x$document$document_ref_id, character(1)))
  expect_true(all(file.exists(c(source_path, object_path))))
  for (item in documents) {
    manifest <- file.path(cache, "projects", item$source$source_ref_id, "manifest.json")
    expect_true(file.exists(manifest))
    payload <- jsonlite::fromJSON(manifest, simplifyVector = FALSE)
    expect_true(any(vapply(payload$documents[[1]]$versions, function(version) {
      identical(version$location$storage_root_id, "target") && identical(version$sha256, sha)
    }, logical(1))))
  }

  # A retained catalog claim cannot make a missing or corrupt object active.
  writeBin(charToRaw("corrupt"), object_path)
  corrupt <- acquisition_queue_default_migrate_storage(job, catalog)
  expect_identical(corrupt$error_category, "target-verification-failed")
  writeBin(bytes, object_path)
  repaired <- acquisition_queue_default_migrate_storage(job, catalog)
  expect_identical(repaired$status, "succeeded")

  rollback_worker <- acquisition_queue_claim(catalog, "rollback-worker", lease_seconds = 60L)
  expect_true(rollback_worker$ok)
  rolled_back <- acquisition_storage_rollback_relocation(catalog,
    relocation$relocation_id, admin, cache_root = cache)
  expect_identical(rolled_back$status, "succeeded")
  expect_identical(acquisition_storage_reservations(catalog, "released")$state[[1L]], "released")
  expect_identical(acquisition_queue_attempts(catalog)$outcome[[1L]], "cancelled")
  expect_true(file.exists(object_path))
  fenced <- acquisition_queue_default_migrate_storage(job, catalog)
  expect_identical(fenced$status, "cancelled")
  for (item in documents) {
    manifest <- jsonlite::fromJSON(file.path(cache, "projects", item$source$source_ref_id,
      "manifest.json"), simplifyVector = FALSE)
    versions <- manifest$documents[[1]]$versions
    preferred <- Filter(function(version) isTRUE(version$preferred_location), versions)
    expect_true(length(preferred) > 0L)
    expect_true(all(vapply(preferred, function(version) {
      identical(version$location$storage_root_id, "source")
    }, logical(1))))
  }
})
