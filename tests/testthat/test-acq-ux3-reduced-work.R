# ACQ-UX3: keep dashboard and preview work bounded and independent.

testthat::test_that("ingestion metrics aggregate successful attempts in SQL", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("openssl")

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  source <- acquisition_catalog_register_source(
    catalog, "nl", "portal.example", "aggregate-project",
    source_url = "https://portal.example/aggregate-project"
  )
  document <- acquisition_catalog_register_document(
    catalog, source$source_ref_id, "aggregate-document",
    attachment_url = "https://portal.example/aggregate-project/file.pdf"
  )
  file <- acquisition_catalog_register_file(
    catalog, document$document_ref_id, "fixture-root", "aggregate.pdf",
    sha256 = paste(rep("a", 64), collapse = ""), size_bytes = 1L,
    verification_status = "verified"
  )
  job <- acquisition_queue_enqueue(
    catalog, "ingest", "file", file$file_ref_id,
    idempotency_key = "ux3-aggregate-ingestion"
  )
  claim <- acquisition_queue_claim(catalog, "aggregate-worker", kinds = "ingest")
  acquisition_queue_complete(
    catalog, claim$job_id, claim$lease_token,
    status = "succeeded",
    result = list(size_bytes = 1L)
  )
  DBI::dbExecute(
    catalog$conn,
    paste(
      "UPDATE acquisition_job_attempts SET claimed_at = '2026-01-01T00:00:00.000000Z',",
      "lease_expires_at = '2026-01-01T00:05:00.000000Z', heartbeat_at = '2026-01-01T00:00:00.500000Z',",
      "finished_at = '2026-01-01T00:00:01.000000Z', bytes = 1 WHERE job_id = ?"
    ),
    params = list(job$job_id)
  )
  DBI::dbExecute(
    catalog$conn,
    paste(
      "WITH RECURSIVE seq(i) AS (SELECT 2 UNION ALL SELECT i + 1 FROM seq WHERE i < 10001)",
      "INSERT INTO acquisition_job_attempts (job_id, attempt_no, lease_token, worker_id, host, claimed_at,",
      "lease_expires_at, heartbeat_at, finished_at, outcome, bytes) SELECT ?, i, printf('fixture-%d', i), 'fixture',",
      "'fixture', '2026-01-01T00:00:00.000000Z', '2026-01-01T00:05:00.000000Z',",
      "'2026-01-01T00:00:01.000000Z', '2026-01-01T00:00:01.000000Z', 'succeeded', i FROM seq"
    ),
    params = list(job$job_id)
  )

  DBI::dbExecute(
    catalog$conn,
    paste(
      "UPDATE acquisition_job_attempts SET bytes = CASE attempt_no",
      "WHEN 2 THEN NULL WHEN 3 THEN 0 WHEN 4 THEN -5 WHEN 5 THEN 7",
      "WHEN 6 THEN 11 WHEN 7 THEN 13 WHEN 8 THEN 17 WHEN 9 THEN 19",
      "WHEN 10 THEN 23 WHEN 11 THEN 29 WHEN 12 THEN 31 ELSE bytes END,",
      "claimed_at = CASE attempt_no",
      "WHEN 5 THEN 'invalid-time' WHEN 6 THEN 'invalid-time' WHEN 8 THEN '2026-01-01T00:00:01.000000Z'",
      "WHEN 10 THEN '2026-01-01T00:00:00.000000Z' ELSE claimed_at END,",
      "finished_at = CASE attempt_no",
      "WHEN 7 THEN 'invalid-time' WHEN 8 THEN '2026-01-01T00:00:00.500000Z'",
      "WHEN 10 THEN '2026-01-01T00:00:00.000000Z' WHEN 11 THEN '2026-01-01T00:00:01.750000Z'",
      "WHEN 9 THEN '2026-01-01T00:00:00.250000Z' WHEN 12 THEN NULL ELSE finished_at END",
      "WHERE job_id = ? AND attempt_no BETWEEN 2 AND 12"
    ),
    params = list(job$job_id)
  )

  reference <- DBI::dbGetQuery(
    catalog$conn,
    paste(
      "SELECT a.bytes, a.claimed_at, a.finished_at",
      "FROM acquisition_job_attempts a JOIN acquisition_jobs j ON j.job_id = a.job_id",
      "WHERE j.kind = 'ingest' AND a.outcome = 'succeeded' AND a.finished_at IS NOT NULL"
    )
  )
  reference_seconds <- if (nrow(reference) == 0L) {
    numeric(0)
  } else {
    suppressWarnings(
      as.numeric(
        as.POSIXct(reference$finished_at, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC") -
          as.POSIXct(reference$claimed_at, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
      )
    )
  }
  reference_bytes <- if (nrow(reference) == 0L) numeric(0) else as.numeric(reference$bytes)
  reference_measured <- is.finite(reference_seconds) & reference_seconds > 0 & !is.na(reference_bytes)
  expected_completed_bytes <- if (any(!is.na(reference_bytes))) sum(reference_bytes, na.rm = TRUE) else NA_real_
  expected_mean_seconds <- if (any(reference_measured)) mean(reference_seconds[reference_measured]) else NA_real_
  expected_bytes_per_second <- if (any(reference_measured)) {
    sum(reference_bytes[reference_measured]) / sum(reference_seconds[reference_measured])
  } else {
    NA_real_
  }

  trace_env <- new.env(parent = emptyenv())
  trace_env$fetched <- integer(0)
  trace_name <- "acq_ux3_ingestion_trace"
  assign(trace_name, trace_env, envir = .GlobalEnv)
  on.exit(rm(list = trace_name, envir = .GlobalEnv), add = TRUE)
  trace(
    "dbFetch",
    signature = "SQLiteResult",
    where = getNamespace("RSQLite"),
    exit = quote({
      env <- get("acq_ux3_ingestion_trace", envir = .GlobalEnv)
      if (grepl("acquisition_job_attempts", res@sql, fixed = TRUE)) {
        env$fetched <- c(env$fetched, nrow(returnValue()))
      }
    }),
    print = FALSE
  )
  on.exit(untrace("dbFetch", signature = "SQLiteResult", where = getNamespace("RSQLite")), add = TRUE)

  metrics <- acquisition_dashboard_ingestion_metrics(catalog)

  testthat::expect_equal(metrics$observed_completed_bytes, expected_completed_bytes)
  testthat::expect_equal(metrics$observed_mean_seconds, expected_mean_seconds, tolerance = 1e-3)
  testthat::expect_equal(metrics$observed_bytes_per_second, expected_bytes_per_second, tolerance = 1e-3)
  testthat::expect_true(length(trace_env$fetched) >= 1L)
  testthat::expect_lte(max(trace_env$fetched), 1L)
})

testthat::test_that("dashboard campaign target choices are bounded by SQL", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("openssl")

  root <- withr::local_tempdir()
  artifact <- file.path(root, "model.rds")
  saveRDS(list(model = "fixture"), artifact)
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  source <- acquisition_catalog_register_source(
    catalog, "nl", "portal.example", "bounded-target",
    source_url = "https://portal.example/bounded-target"
  )
  model <- acquisition_catalog_register_model(
    catalog, "bounded-target-model", artifact, "policy.v1", "snapshot.v1"
  )
  selection <- data.frame(source_ref_id = source$source_ref_id, selected = TRUE, stringsAsFactors = FALSE)
  rules <- list(list(rule_id = "fallback", priority = 1L))
  acquisition_campaign_submit(
    catalog, selection, model$model_id, "snapshot.v1", 0.5, "policy.v1", rules,
    submitted_by = "fixture", campaign_id = "bounded-target-a"
  )
  acquisition_campaign_submit(
    catalog, selection, model$model_id, "snapshot.v1", 0.5, "policy.v1", rules,
    submitted_by = "fixture", campaign_id = "bounded-target-b"
  )

  choices <- acquisition_dashboard_target_choices(catalog, "campaign", limit = 1L)

  testthat::expect_length(choices, 1L)
  testthat::expect_named(choices)
  testthat::expect_identical(unname(choices), "bounded-target-b")
})

testthat::test_that("model thresholding reuses probabilities without prediction", {
  probabilities <- data.frame(
    select_prob = c(0.2, 0.6, NA_real_),
    selected_model = c(FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  low <- curation_apply_model_threshold(probabilities, 0.5)
  high <- curation_apply_model_threshold(probabilities, 0.8)

  testthat::expect_identical(low$select_prob, probabilities$select_prob)
  testthat::expect_identical(high$select_prob, probabilities$select_prob)
  testthat::expect_identical(low$selected_model, c(FALSE, TRUE, FALSE))
  testthat::expect_identical(high$selected_model, c(FALSE, FALSE, FALSE))
})

testthat::test_that("inactive acquisition preview does not resolve or rebuild", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  calls <- 0L
  preview <- function() {
    calls <<- calls + 1L
    list(
      selection = data.frame(source_ref_id = character(0), selected = logical(0)),
      model_id = "model", input_snapshot_id = "snapshot", threshold = 0.5,
      policy_version = "policy", priority_rules = list(list(rule_id = "fallback", priority = 1L))
    )
  }
  resolver <- curation_actor_resolver(function(request) curation_actor("admin", roles = "admin"))
  active <- shiny::reactiveVal(FALSE)
  builder_calls <- 0L
  testthat::local_mocked_bindings(
    acquisition_campaign_preview = function(...) {
      builder_calls <<- builder_calls + 1L
      list(
        model_id = "model", input_snapshot_id = "snapshot", threshold = 0.5,
        policy_version = "policy", counts = list(projects = 0L, selected_projects = 0L, jobs = 0L),
        items = data.frame(stringsAsFactors = FALSE)
      )
    },
    .package = "planscanR.screen"
  )

  shiny::testServer(
    acquisition_campaign_panel_server,
    args = list(
      id = "campaign", catalog = catalog, preview = preview,
      actor_resolver = resolver, active = active
    ),
    {
      session$setInputs(`campaign-threshold` = 0.8)
      session$flushReact()
      testthat::expect_identical(calls, 0L)
      active(TRUE)
      session$setInputs(`campaign-threshold` = 0.8)
      session$flushReact()
      invisible(output$`campaign-status`)
      testthat::expect_identical(calls, 1L)
      testthat::expect_identical(builder_calls, 1L)
      testthat::expect_match(paste(as.character(output$`campaign-status`), collapse = " "), "No jobs created")
      active(FALSE)
      session$flushReact()
      active(TRUE)
      session$flushReact()
      invisible(output$`campaign-status`)
      testthat::expect_identical(calls, 1L)
      testthat::expect_identical(builder_calls, 1L)
      session$setInputs(`campaign-threshold` = 0.9)
      session$flushReact()
      invisible(output$`campaign-status`)
      testthat::expect_identical(builder_calls, 2L)
    }
  )
})

testthat::test_that("inactive acquisition dashboard pauses summary polling", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  resolver <- curation_actor_resolver(function(request) curation_actor("admin", roles = "admin"))
  summary_calls <- 0L
  real_summary <- get("acquisition_dashboard_summary", asNamespace("planscanR.screen"))
  testthat::local_mocked_bindings(
    acquisition_dashboard_summary = function(...) {
      summary_calls <<- summary_calls + 1L
      real_summary(...)
    },
    .package = "planscanR.screen"
  )

  shiny::testServer(
    acquisition_dashboard_server,
    args = list(
      id = "dashboard", catalog = catalog, actor_resolver = resolver,
      active = function() FALSE, refresh_ms = 250L
    ),
    {
      session$flushReact()
      testthat::expect_identical(summary_calls, 0L)
      testthat::expect_match(as.character(output$`dashboard-progress`), "inactive")
    }
  )

  shiny::testServer(
    acquisition_dashboard_server,
    args = list(
      id = "dashboard", catalog = catalog, actor_resolver = resolver,
      active = function() TRUE, refresh_ms = 250L
    ),
    {
      session$flushReact()
      testthat::expect_gt(summary_calls, 0L)
    }
  )
})

testthat::test_that("active acquisition dashboard recovers after a transient summary error", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  resolver <- curation_actor_resolver(function(request) curation_actor("admin", roles = "admin"))
  summary_calls <- 0L
  fail_once <- TRUE
  real_summary <- get("acquisition_dashboard_summary", asNamespace("planscanR.screen"))
  testthat::local_mocked_bindings(
    acquisition_dashboard_summary = function(...) {
      summary_calls <<- summary_calls + 1L
      if (fail_once) {
        fail_once <<- FALSE
        stop("fixture transient summary failure")
      }
      real_summary(...)
    },
    .package = "planscanR.screen"
  )

  shiny::testServer(
    acquisition_dashboard_server,
    args = list(
      id = "dashboard", catalog = catalog, actor_resolver = resolver,
      active = function() TRUE, refresh_ms = 250L
    ),
    {
      session$flushReact()
      testthat::expect_match(as.character(output$`dashboard-progress`), "error")
      session$elapse(250L)
      session$flushReact()
      testthat::expect_gte(summary_calls, 2L)
      testthat::expect_false(grepl("error", as.character(output$`dashboard-progress`), fixed = TRUE))
    }
  )
})

testthat::test_that("active acquisition dashboard recovers after a polling-token error", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  resolver <- curation_actor_resolver(function(request) curation_actor("admin", roles = "admin"))
  poll_calls <- 0L
  summary_calls <- 0L
  fail_token <- TRUE
  real_token <- get("acquisition_dashboard_poll_token", asNamespace("planscanR.screen"))
  real_summary <- get("acquisition_dashboard_summary", asNamespace("planscanR.screen"))
  testthat::local_mocked_bindings(
    acquisition_dashboard_poll_token = function(catalog) {
      poll_calls <<- poll_calls + 1L
      if (fail_token) stop("fixture transient polling failure")
      real_token(catalog)
    },
    acquisition_dashboard_summary = function(...) {
      summary_calls <<- summary_calls + 1L
      real_summary(...)
    },
    .package = "planscanR.screen"
  )

  shiny::testServer(
    acquisition_dashboard_server,
    args = list(
      id = "dashboard", catalog = catalog, actor_resolver = resolver,
      active = function() TRUE, refresh_ms = 250L
    ),
    {
      session$flushReact()
      testthat::expect_match(as.character(output$`dashboard-progress`), "error")
      fail_token <<- FALSE
      session$elapse(1000L)
      session$flushReact()
      session$flushReact()
      testthat::expect_gte(poll_calls, 2L)
      testthat::expect_gte(summary_calls, 1L)
      testthat::expect_false(grepl("error", as.character(output$`dashboard-progress`), fixed = TRUE))
    }
  )
})
