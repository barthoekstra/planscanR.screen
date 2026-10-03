test_that("the acquisition catalog is a separate, repeatable WAL database", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  path <- withr::local_tempfile(fileext = ".sqlite")
  catalog <- acquisition_catalog_open(path)
  expect_s3_class(catalog, "planscanR_acquisition_catalog")
  expect_identical(catalog$journal_mode, "wal")
  expect_true(catalog$foreign_keys)
  expect_identical(catalog$schema_version, 3L)
  expect_true(all(c(
    "acquisition_schema_migrations", "acquisition_sources",
    "acquisition_documents", "acquisition_files", "acquisition_import_ledger",
    "acquisition_models", "acquisition_selections", "acquisition_jobs",
    "acquisition_job_attempts", "acquisition_host_limits"
  ) %in% DBI::dbListTables(catalog$conn)))
  catalog$close()

  reopened <- acquisition_catalog_open(path)
  expect_identical(reopened$schema_version, 3L)
  expect_equal(
    DBI::dbGetQuery(reopened$conn, "SELECT COUNT(*) AS n FROM acquisition_schema_migrations")$n,
    3
  )
  reopened$close()
})

test_that("a review database is never accepted as an acquisition catalog", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  path <- withr::local_tempfile(fileext = ".sqlite")
  conn <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(conn, "CREATE TABLE curation_reviews (country TEXT)")
  DBI::dbDisconnect(conn)

  expect_error(
    acquisition_catalog_open(path),
    class = "planscanR_error_acquisition_review_database"
  )
})

test_that("record import keeps colliding source IDs distinct and is idempotent", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  catalog <- acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
  records <- tibble::tibble(
    country = c("nl", "nl"),
    source_portal = c("portal-a", "portal-b"),
    document_id = c("same-native-id", "same-native-id"),
    url = c("https://a/project", "https://b/project"),
    title = c("A", "B"),
    attachment_urls = list("https://files/a.pdf", "https://files/a.pdf")
  )

  first <- acquisition_catalog_import_records(
    catalog,
    records,
    import_source = "fixture-v1"
  )
  expect_equal(first$sources, 2L)
  expect_equal(first$documents, 2L)
  expect_equal(
    DBI::dbGetQuery(catalog$conn, "SELECT COUNT(*) AS n FROM acquisition_sources")$n,
    2
  )
  expect_equal(
    DBI::dbGetQuery(catalog$conn, "SELECT COUNT(*) AS n FROM acquisition_documents")$n,
    2
  )
  expect_equal(
    DBI::dbGetQuery(catalog$conn, paste(
      "SELECT COUNT(*) AS n FROM acquisition_documents",
      "WHERE identity_status = 'ambiguous'"
    ))$n,
    2
  )

  replay <- acquisition_catalog_import_records(
    catalog,
    records,
    import_source = "fixture-v1"
  )
  expect_equal(replay$imported, 0L)
  expect_equal(replay$replayed, 2L)
  reordered <- records[2:1, , drop = FALSE]
  expect_equal(
    acquisition_catalog_import_records(
      catalog,
      reordered,
      import_source = "fixture-v1"
    )$replayed,
    2L
  )
  expect_error(
    acquisition_catalog_import_records(
      catalog,
      transform(records, title = "changed"),
      import_source = "fixture-v1"
    ),
    class = "planscanR_error_acquisition_import_conflict"
  )
  catalog$close()
})

test_that("legacy file claims are portable and remain unverified", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  root <- withr::local_tempdir()
  existing <- file.path(root, "files", "nl", "p1", "report.pdf")
  dir.create(dirname(existing), recursive = TRUE)
  writeBin(charToRaw("legacy bytes"), existing)
  records <- tibble::tibble(
    country = "nl",
    source_portal = "portal-a",
    document_id = "p1",
    url = "https://portal/p1",
    attachment_urls = list("https://portal/report.pdf"),
    local_path = list(existing)
  )
  state <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(state, "catalog.sqlite"))
  imported <- acquisition_catalog_import_records(
    catalog,
    records,
    import_source = "legacy-sidecar-v1",
    file_root = root
  )
  expect_equal(imported$files, 1L)
  file_row <- DBI::dbGetQuery(
    catalog$conn,
    "SELECT relative_path, root_id, verification_status, provenance FROM acquisition_files"
  )
  expect_identical(file_row$relative_path, "files/nl/p1/report.pdf")
  expect_identical(file_row$root_id, "legacy-root")
  expect_identical(file_row$verification_status, "unverified")
  expect_match(file_row$provenance, "legacy-sidecar-v1")
  document_ref_id <- imported$document_ref_ids[[1L]]
  acquisition_catalog_register_file(
    catalog,
    document_ref_id = document_ref_id,
    root_id = "legacy-root",
    relative_path = "files/nl/p1/report.pdf",
    sha256 = "verified-hash",
    size_bytes = 99,
    verification_status = "verified",
    provenance = "integrity-check"
  )
  acquisition_catalog_register_file(
    catalog,
    document_ref_id = document_ref_id,
    root_id = "legacy-root",
    relative_path = "files/nl/p1/report.pdf",
    sha256 = "stale-legacy-hash",
    size_bytes = 1,
    verification_status = "unverified",
    provenance = "legacy-replay"
  )
  preserved <- DBI::dbGetQuery(
    catalog$conn,
    "SELECT sha256, size_bytes, verification_status, provenance FROM acquisition_files"
  )
  expect_identical(preserved$sha256, "verified-hash")
  expect_equal(preserved$size_bytes, 99L)
  expect_identical(preserved$verification_status, "verified")
  expect_identical(preserved$provenance, "integrity-check")
  catalog$close()
})

test_that("model and selection references are immutable and retain artifacts", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("openssl")

  root <- withr::local_tempdir()
  artifact <- file.path(root, "selection-model.rds")
  saveRDS(list(model = "fixture"), artifact)
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  model <- acquisition_catalog_register_model(
    catalog,
    model_id = "model-1",
    artifact_path = artifact,
    config_id = "policy.v1",
    input_snapshot_id = "snapshot-1",
    metadata = list(creator = "test")
  )
  expect_identical(model$model_id, "model-1")
  expect_true(file.exists(artifact))
  expect_true(nzchar(model$artifact_sha256))
  expect_true(file.exists(model$artifact_path))
  expect_identical(
    paste0(openssl::sha256(readBin(
      model$artifact_path,
      what = "raw",
      n = file.info(model$artifact_path)$size
    ))),
    model$artifact_sha256
  )
  writeBin(charToRaw("corrupt managed copy"), model$artifact_path)
  repaired <- acquisition_catalog_register_model(
    catalog,
    model_id = "model-1",
    artifact_path = artifact,
    config_id = "policy.v1",
    input_snapshot_id = "snapshot-1",
    metadata = list(creator = "test")
  )
  expect_identical(repaired$status, "replayed")
  expect_identical(
    paste0(openssl::sha256(readBin(
      model$artifact_path,
      what = "raw",
      n = file.info(model$artifact_path)$size
    ))),
    model$artifact_sha256
  )
  second_artifact <- file.path(root, "second-model.rds")
  third_artifact <- file.path(root, "third-model.rds")
  saveRDS(list(model = "second"), second_artifact)
  saveRDS(list(model = "third"), third_artifact)
  colliding_id <- acquisition_catalog_register_model(
    catalog,
    model_id = "m/a",
    artifact_path = second_artifact,
    config_id = "policy.v1",
    input_snapshot_id = "snapshot-1"
  )
  distinct_id <- acquisition_catalog_register_model(
    catalog,
    model_id = "m_a",
    artifact_path = third_artifact,
    config_id = "policy.v1",
    input_snapshot_id = "snapshot-1"
  )
  expect_false(identical(colliding_id$artifact_path, distinct_id$artifact_path))
  expect_true(file.exists(colliding_id$artifact_path))
  expect_true(file.exists(distinct_id$artifact_path))
  expect_identical(
    acquisition_catalog_register_model(
      catalog,
      model_id = "model-1",
      artifact_path = artifact,
      config_id = "policy.v1",
      input_snapshot_id = "snapshot-1",
      metadata = list(creator = "test")
    )$status,
    "replayed"
  )
  expect_error(
    acquisition_catalog_register_model(
      catalog,
      model_id = "model-1",
      artifact_path = artifact,
      config_id = "policy.v2",
      input_snapshot_id = "snapshot-1"
    ),
    class = "planscanR_error_acquisition_immutable"
  )

  first <- acquisition_catalog_register_selection(
    catalog,
    selection_id = "selection-1",
    input_snapshot_id = "snapshot-1",
    model_id = "model-1",
    metadata = list(threshold = 0.5)
  )
  expect_identical(first$status, "inserted")
  expect_identical(
    acquisition_catalog_register_selection(
      catalog,
      selection_id = "selection-1",
      input_snapshot_id = "snapshot-1",
      model_id = "model-1",
      metadata = list(threshold = 0.5)
    )$status,
    "replayed"
  )
  expect_error(
    acquisition_catalog_register_selection(
      catalog,
      selection_id = "selection-1",
      input_snapshot_id = "snapshot-2",
      model_id = "model-1",
      metadata = list(threshold = 0.5)
    ),
    class = "planscanR_error_acquisition_immutable"
  )
  catalog$close()
})
