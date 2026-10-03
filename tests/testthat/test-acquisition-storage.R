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

  source <- acquisition_catalog_register_source(
    catalog, "nl", "portal.example", "project-1",
    source_url = "https://portal.example/project-1"
  )
  document <- acquisition_catalog_register_document(
    catalog, source$source_ref_id,
    native_attachment_id = "file-1",
    attachment_url = "https://portal.example/project-1/file.pdf"
  )
  job <- acquisition_queue_enqueue(
    catalog, "download", "document", document$document_ref_id
  )
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
  expect_equal(held$bytes[[1L]], planscanR::storage_transfer_cap_bytes())
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
  source <- acquisition_catalog_register_source(catalog, "nl", "portal.example", "fraction")
  document <- acquisition_catalog_register_document(catalog, source$source_ref_id, native_attachment_id = "fraction")
  acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id)
  expect_false(acquisition_queue_claim(catalog, "fraction-worker")$ok)
  expect_equal(nrow(acquisition_storage_reservations(catalog)), 0L)
})

test_that("a claimed download keeps its reservation cap when settings increase", {
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
  withr::local_options(list(planscanR.max_file_size_mb = reserved / (1024^2) * 2))
  result <- acquisition_queue_default_download(claim, catalog)
  expect_identical(result$status, "retryable")
  expect_equal(observed$cap, reserved)
  acquisition_queue_complete(catalog, claim$job_id, claim$lease_token, status = "retryable")
})

test_that("reservations serialize across two worker connections", {
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
  expect_false(second$ok)
  expect_identical(second$status, "storage-reserve-unavailable")
  expect_equal(sum(acquisition_storage_reservations(catalog_b)$state == "held"), 1L)
  acquisition_queue_complete(catalog_a, first$job_id, first$lease_token, status = "retryable")
  expect_true(acquisition_queue_claim(catalog_b, "worker-b")$ok)
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
