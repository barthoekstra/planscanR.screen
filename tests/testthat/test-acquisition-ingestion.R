skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

ingestion_test_catalog <- function() {
  acquisition_catalog_open(tempfile(fileext = ".sqlite"))
}

ingestion_test_source_document <- function(catalog) {
  source <- acquisition_catalog_register_source(
    catalog, "nl", "portal.example", "project-1",
    source_url = "https://portal.example/project-1"
  )
  document <- acquisition_catalog_register_document(
    catalog, source$source_ref_id, "attachment-1",
    attachment_url = "https://portal.example/project-1/file.pdf"
  )
  list(source = source, document = document)
}

test_that("verified immutable file versions enqueue separately from normal workers", {
  catalog <- ingestion_test_catalog()
  refs <- ingestion_test_source_document(catalog)
  expect_equal(acquisition_dashboard_summary(catalog)$ingestion$backlog, 0L)
  root <- tempfile("ingestion-cache-")
  dir.create(root)
  path <- file.path(root, "report.pdf")
  writeBin(charToRaw("verified bytes"), path)
  digest <- as.character(openssl::sha256(readBin(path, "raw", n = file.info(path)$size)))
  acquisition_storage_register_root(catalog, "fixture-root", root, list(id = "test", roles = "admin"))
  file <- acquisition_catalog_register_file(
    catalog,
    refs$document$document_ref_id,
    root_id = "fixture-root",
    relative_path = "report.pdf",
    sha256 = digest,
    size_bytes = file.info(path)$size,
    verification_status = "verified",
    provenance = "fixture"
  )
  planscanR::reconcile_cached_download(
    path, expected_size_bytes = file.info(path)$size,
    expected_sha256 = digest,
    url = "https://portal.example/project-1/file.pdf", repair = TRUE
  )
  queued <- acquisition_queue_enqueue_ingestion(
    catalog,
    file_ref_id = file$file_ref_id
  )
  replay <- acquisition_queue_enqueue_ingestion(
    catalog,
    file_ref_id = file$file_ref_id
  )
  download <- acquisition_queue_enqueue(
    catalog, "download", "document", refs$document$document_ref_id
  )

  expect_identical(queued$status, "inserted")
  expect_identical(replay$status, "replayed")
  default_claim <- acquisition_queue_claim(catalog, "download-worker")
  expect_true(default_claim$ok)
  expect_identical(default_claim$kind, "download")
  expect_identical(
    acquisition_queue_claim(catalog, "ingestion-worker", kinds = "ingest")$kind,
    "ingest"
  )
})

test_that("ingestion rechecks the file and records callback output under a fenced attempt", {
  catalog <- ingestion_test_catalog()
  refs <- ingestion_test_source_document(catalog)
  root <- tempfile("ingestion-cache-")
  dir.create(root)
  path <- file.path(root, "report.pdf")
  writeBin(charToRaw("verified bytes"), path)
  digest <- as.character(openssl::sha256(readBin(path, "raw", n = file.info(path)$size)))
  planscanR::reconcile_cached_download(
    path, expected_size_bytes = file.info(path)$size,
    expected_sha256 = digest,
    url = "https://portal.example/project-1/file.pdf", repair = TRUE
  )
  acquisition_storage_register_root(catalog, "fixture-root", root, list(id = "test", roles = "admin"))
  file <- acquisition_catalog_register_file(
    catalog, refs$document$document_ref_id, "fixture-root", "report.pdf",
    sha256 = digest, size_bytes = file.info(path)$size,
    verification_status = "verified", provenance = "fixture"
  )
  acquisition_queue_enqueue_ingestion(
    catalog, file$file_ref_id
  )
  seen <- NULL
  callback <- function(file_path, sha256, document_id, country, version_id = NULL,
                       cache_dir, embedding_model, ...) {
    seen <<- list(file_path = file_path, sha256 = sha256,
                  document_id = document_id, country = country,
                  version_id = version_id, cache_dir = cache_dir)
    list(status = "succeeded", result = data.frame(chunks = 1L),
         provenance = list(sha256 = sha256, version_id = version_id))
  }
  result <- acquisition_worker_run(
    catalog, "ingestion-worker", ingest_fn = callback,
    job_kinds = "ingest"
  )

  expect_true(result[[1L]]$completion$ok)
  expect_identical(seen$document_id, refs$source$source_ref_id)
  expect_identical(seen$country, "nl")
  expect_identical(seen$version_id, file$file_ref_id)
  expect_identical(acquisition_queue_jobs(catalog)$state, "succeeded")
  expect_true(nzchar(acquisition_queue_attempts(catalog)$result_blob[[1L]]))
  metrics <- acquisition_dashboard_summary(catalog)$ingestion
  expect_equal(metrics$succeeded, 1L)
  expect_equal(metrics$observed_completed_bytes, as.numeric(file.info(path)$size))
})

test_that("unverified file claims cannot enter the ingestion queue", {
  catalog <- ingestion_test_catalog()
  refs <- ingestion_test_source_document(catalog)
  file <- acquisition_catalog_register_file(
    catalog, refs$document$document_ref_id, "fixture-root", "report.pdf",
    sha256 = paste(rep("a", 64), collapse = ""), size_bytes = 14L,
    verification_status = "unverified", provenance = "legacy"
  )

  expect_error(
    acquisition_queue_enqueue_ingestion(
      catalog, file$file_ref_id
    ),
    class = "planscanR_error_acquisition_queue_unverified"
  )
  expect_equal(nrow(acquisition_queue_jobs(catalog)), 0L)
})

test_that("schema v5 preserves attempts and dependent queue references when reopening v4", {
  path <- tempfile(fileext = ".sqlite")
  catalog <- acquisition_catalog_open(path)
  refs <- ingestion_test_source_document(catalog)
  root <- tempfile("migration-root-")
  dir.create(root)
  acquisition_storage_register_root(catalog, "migration-root", root, list(id = "test", roles = "admin"))
  job <- acquisition_queue_enqueue(
    catalog, "download", "document", refs$document$document_ref_id
  )
  claim <- acquisition_queue_claim(catalog, "migration-worker")
  acquisition_controls_ensure_schema(catalog)
  now <- acquisition_catalog_now()
  DBI::dbExecute(
    catalog$conn,
    paste(
      "INSERT INTO acquisition_control_pauses (job_id, target_type, target_id,",
      "previous_state, previous_available_at, paused_at) VALUES (?, 'job', ?, 'running', ?, ?)"
    ),
    params = list(job$job_id, job$job_id, now, now)
  )
  expect_equal(DBI::dbGetQuery(
    catalog$conn,
    "SELECT COUNT(*) AS n FROM acquisition_storage_reservations WHERE job_id = ?",
    params = list(job$job_id)
  )$n, 1L)
  DBI::dbExecute(catalog$conn, "PRAGMA writable_schema = ON")
  DBI::dbExecute(
    catalog$conn,
    paste(
      "UPDATE sqlite_master SET sql = replace(sql, ', ''ingest''', '')",
      "WHERE type = 'table' AND name = 'acquisition_jobs'"
    )
  )
  DBI::dbExecute(catalog$conn, "PRAGMA writable_schema = OFF")
  DBI::dbExecute(catalog$conn, "PRAGMA schema_version = 99")
  DBI::dbExecute(catalog$conn, "DELETE FROM acquisition_schema_migrations WHERE version = 5")
  DBI::dbExecute(catalog$conn, "PRAGMA user_version = 4")
  catalog$close()

  reopened <- acquisition_catalog_open(path)
  on.exit(reopened$close(), add = TRUE)
  expect_equal(nrow(acquisition_queue_attempts(reopened, job$job_id)), 1L)
  expect_equal(DBI::dbGetQuery(reopened$conn, "SELECT COUNT(*) AS n FROM acquisition_control_pauses")$n, 1L)
  expect_equal(DBI::dbGetQuery(reopened$conn, "SELECT COUNT(*) AS n FROM acquisition_storage_reservations")$n, 1L)
  expect_equal(nrow(DBI::dbGetQuery(reopened$conn, "PRAGMA foreign_key_check")), 0L)
})
