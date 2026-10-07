# ACQ-UX1 is an offline characterization runner.  It deliberately measures
# the current contracts and keeps future-behaviour assertions in
# tests/characterization rather than changing the application.

acq_ux1_ns <- function(name) {
  get(name, envir = asNamespace("planscanR.screen"), inherits = FALSE)
}

acq_ux1_load_screen_package <- function(package_root) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("pkgload is required to load planscanR.screen for the fixture.", call. = FALSE)
  }
  # Always bind the source checkout for this baseline. A separately installed
  # namespace could otherwise make the measurements silently describe another
  # commit than the files and contracts recorded beside the evidence.
  pkgload::load_all(package_root, quiet = TRUE)
  invisible(TRUE)
}

acq_ux1_fixture_config <- function(root = tempfile("acq-ux1-state-")) {
  labels <- c(relevant = "Relevant", irrelevant = "Irrelevant")
  attr(labels, "relevant") <- "relevant"
  acq_ux1_ns("curation_config")(
    project_id = "acq-ux1-fixture",
    corpus_id = "synthetic-128k",
    config_id = "fixture.v1",
    topics = c(energy = "Energy infrastructure"),
    labels = labels,
    lexicon = list(energy = c("energy", "wind", "solar")),
    select_fn = function(records, controls) {
      threshold <- as.numeric(controls$relevance_threshold %||% 0.5)
      kw_min <- as.numeric(controls$kw_min %||% 1)
      records$selected <- !is.na(records$cosine_max) &
        records$cosine_max >= threshold &
        records$kw_total >= kw_min
      records
    },
    selection_controls = list(relevance_threshold = 0.5, kw_min = 1),
    funnel_stages = list(
      list(
        key = "indexed",
        label = "Indexed",
        fn = function(records, context) rep(TRUE, nrow(records))
      ),
      list(
        key = "selected",
        label = "Selected",
        fn = function(records, context) records$selected %in% TRUE
      )
    ),
    branding = list(title = "ACQ-UX1 fixture", primary = "#175A7A"),
    artifact_paths = list(root = root)
  )
}

acq_ux1_fixture_records <- function(n = 128000L) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 1L || n %% 4L != 0L) {
    stop("The synthetic fixture size must be a positive multiple of four.", call. = FALSE)
  }
  index <- seq_len(n)
  country <- rep(c("be", "de", "dk", "nl"), each = n %/% 4L)
  native_id <- sprintf("native-%05d", ((index - 1L) %% 32000L) + 1L)
  records <- data.frame(
    country = country,
    document_id = native_id,
    title = sprintf("Energy planning record %06d", index),
    summary = ifelse(index %% 3L == 0L, "Wind and solar energy project", "Planning record"),
    native_type = rep(c("permit", "plan", "decision", "notice"), length.out = n),
    relevance_score_energy = ((index * 37L) %% 1000L) / 1000,
    class_score_relevant = ((index * 17L) %% 1000L) / 1000,
    class_score_irrelevant = 1 - (((index * 17L) %% 1000L) / 1000),
    class_label = ifelse(index %% 2L == 0L, "Relevant", "Irrelevant"),
    class_relevant = index %% 2L == 0L,
    class_score = ((index * 17L) %% 1000L) / 1000,
    n_attachments = index %% 4L,
    has_attachments = index %% 4L > 0L,
    n_downloaded = index %% 3L,
    has_downloaded = index %% 3L > 0L,
    selected = index %% 5L == 0L,
    selected_model = index %% 7L == 0L,
    url = sprintf("https://fixture.invalid/record/%06d", index),
    internal_note = sprintf("fixture-private-%06d", index),
    source_payload = sprintf("payload-%06d", index),
    stringsAsFactors = FALSE
  )
  records
}

acq_ux1_fixture_provider <- function(records) {
  state <- new.env(parent = emptyenv())
  state$reads <- 0L
  state$refreshes <- 0L
  read_fn <- function(corpus_id, countries = NULL) {
    state$reads <- state$reads + 1L
    out <- records
    if (!is.null(countries)) {
      out <- out[out$country %in% countries, , drop = FALSE]
    }
    out
  }
  refresh_fn <- function(corpus_id, countries = NULL) {
    state$refreshes <- state$refreshes + 1L
    read_fn(corpus_id = corpus_id, countries = countries)
  }
  provider <- acq_ux1_ns("curation_records_provider")(read_fn, refresh_fn)
  attr(provider, "acq_ux1_state") <- state
  provider
}

acq_ux1_current_display_cap <- function(records) {
  if (nrow(records) <= 5000L) {
    return(records)
  }
  attr(records, "truncated") <- nrow(records)
  records[seq_len(5000L), , drop = FALSE]
}

acq_ux1_empty_reviews <- function() {
  data.frame(
    country = character(0),
    document_id = character(0),
    reviewer_id = character(0),
    decision = character(0),
    source = character(0),
    note = character(0),
    reviewed_at = as.POSIXct(character(0), tz = "UTC"),
    sidecar_path = character(0),
    revision = integer(0),
    deleted = logical(0),
    stringsAsFactors = FALSE
  )
}

acq_ux1_empty_review_repository <- function() {
  acq_ux1_ns("curation_review_repository")(
    read_fn = function(scope, key, actor) NULL,
    list_fn = function(scope, filter, actor, limit, offset) acq_ux1_empty_reviews(),
    upsert_fn = function(scope, decision, actor, expected_revision) {
      list(ok = TRUE, revision = as.integer(expected_revision) + 1L)
    },
    clear_fn = function(scope, key, actor, expected_revision) {
      list(ok = TRUE, revision = as.integer(expected_revision) + 1L)
    },
    bulk_upsert_fn = function(scope, decisions, actor, expected_revision) {
      list(ok = TRUE, revision = as.integer(expected_revision) + 1L)
    },
    import_fn = function(scope, rows, actor, expected_revision) {
      list(ok = TRUE, revision = as.integer(expected_revision) + 1L)
    },
    export_fn = function(scope, actor, format) acq_ux1_empty_reviews()
  )
}

acq_ux1_empty_artifact_store <- function() {
  acq_ux1_ns("curation_artifact_store")(
    paths = list(root = tempfile("acq-ux1-app-artifacts-")),
    snapshot_read_fn = function(scope, version, actor) NULL,
    snapshot_write_fn = function(scope, snapshot, actor, expected_revision) {
      list(ok = TRUE, snapshot_id = "fixture-snapshot", version = "fixture-version", revision = 1L)
    },
    sample_list_fn = function(scope, reviewer_id, actor) {
      data.frame(
        sample_id = character(0), reviewer_id = character(0), revision = integer(0),
        stringsAsFactors = FALSE
      )
    },
    sample_read_fn = function(scope, sample_id, actor) NULL,
    sample_write_fn = function(scope, sample, reviewer_id, actor, expected_revision) {
      list(ok = TRUE, sample_id = "fixture-sample", revision = as.integer(expected_revision) + 1L)
    },
    model_list_fn = function(scope, actor) {
      data.frame(model_id = character(0), revision = integer(0), stringsAsFactors = FALSE)
    },
    model_read_fn = function(scope, model_id, actor) NULL,
    model_write_fn = function(scope, model, provenance, actor, expected_revision) {
      list(ok = TRUE, model_id = "fixture-model", revision = as.integer(expected_revision) + 1L)
    },
    model_publish_fn = function(scope, model_id, actor, expected_active_revision) {
      list(ok = TRUE, active_revision = as.integer(expected_active_revision) + 1L)
    },
    active_model_fn = function(scope, actor) NULL
  )
}

acq_ux1_app_context <- function(records, config = acq_ux1_fixture_config()) {
  provider <- acq_ux1_fixture_provider(records)
  snapshot <- acq_ux1_ns("curation_records_snapshot")(provider, config)
  metadata <- attr(snapshot, "curation_snapshot_metadata", exact = TRUE) %||% list()
  metadata$schema_version <- "curation-snapshot.v1"
  snapshot_version <- acq_ux1_ns("curation_snapshot_version")(
    snapshot,
    config$identity,
    config = config
  )
  app_state <- new.env(parent = emptyenv())
  app_state$snapshot <- list(
    records = snapshot,
    id = "fixture-snapshot",
    version = snapshot_version,
    revision = 1L,
    metadata = metadata
  )
  actor <- acq_ux1_ns("curation_actor")(
    "fixture-reviewer",
    roles = "reviewer",
    display_name = "Fixture reviewer"
  )
  list(
    config = config,
    records_provider = provider,
    review_repository = acq_ux1_empty_review_repository(),
    artifact_store = acq_ux1_empty_artifact_store(),
    actor_resolver = acq_ux1_ns("curation_actor_resolver")(function(request) actor),
    request = function(session) list(session = session),
    app_state = app_state
  )
}

acq_ux1_app_server <- function(context) {
  function(input, output, session) {
    acq_ux1_ns("curation_app_server")(input, output, session, context)
  }
}

acq_ux1_memory <- function() {
  if (!requireNamespace("ps", quietly = TRUE)) {
    return(list(rss_bytes = NA_real_, available = FALSE))
  }
  info <- tryCatch(ps::ps_memory_info(), error = function(error) NULL)
  if (is.null(info) || is.null(info[["rss"]])) {
    return(list(rss_bytes = NA_real_, available = FALSE))
  }
  list(rss_bytes = as.numeric(info[["rss"]]), available = TRUE)
}

acq_ux1_measure <- function(label, fn, detail_fn = NULL) {
  invisible(gc())
  before <- acq_ux1_memory()
  started <- proc.time()[["elapsed"]]
  value <- fn()
  elapsed <- proc.time()[["elapsed"]] - started
  after <- acq_ux1_memory()
  detail <- if (is.null(detail_fn)) list() else detail_fn(value)
  list(
    label = label,
    elapsed_seconds = unname(as.numeric(elapsed)),
    server_rss_before_bytes = before$rss_bytes,
    server_rss_after_bytes = after$rss_bytes,
    server_rss_delta_bytes = after$rss_bytes - before$rss_bytes,
    detail = detail
  )
}

acq_ux1_widget_metrics <- function(data, config) {
  widget <- acq_ux1_ns("curation_app_table")(
    data = data,
    reviews = acq_ux1_empty_reviews(),
    config = config,
    reviewer_id = "fixture-reviewer"
  )
  payload <- as.character(widget$x$tag$attribs$data)
  parsed <- tryCatch(
    jsonlite::fromJSON(payload, simplifyVector = FALSE),
    error = function(error) NULL
  )
  payload_columns <- if (is.list(parsed)) {
    names(parsed)
  } else {
    character(0)
  }
  list(
    input_rows = nrow(data),
    input_columns = ncol(data),
    payload_rows = if (is.list(parsed) && length(parsed) > 0L) {
      length(parsed[[1L]])
    } else {
      NA_integer_
    },
    payload_bytes = length(charToRaw(payload)),
    payload_columns = payload_columns,
    payload_column_count = length(payload_columns),
    hidden_fixture_fields_in_payload = intersect(
      c("internal_note", "source_payload"),
      payload_columns
    ),
    page_size = widget$x$tag$attribs$defaultPageSize,
    page_size_options = widget$x$tag$attribs$pageSizeOptions,
    selection = widget$x$tag$attribs$selection,
    searchable = widget$x$tag$attribs$searchable,
    filterable = widget$x$tag$attribs$filterable
  )
}

acq_ux1_browser_tooling <- function() {
  candidates <- c(
    "chromium", "chromium-browser", "google-chrome", "google-chrome-stable",
    "firefox", "agent-browser", "playwright"
  )
  paths <- Sys.which(candidates)
  paths <- paths[nzchar(paths)]
  managed_playwright <- "/tmp/acq-c1-browser/node_modules/playwright/index.mjs"
  if (length(paths) == 0L && file.exists(managed_playwright)) {
    return(list(
      status = "managed_playwright_available",
      executables = managed_playwright,
      note = "A managed Playwright module is available; the host owns the disposable Chromium probe and no signed-in production session was used by this runner."
    ))
  }
  list(
    status = if (length(paths) == 0L) "unavailable" else "available",
    executables = unname(paths),
    note = if (length(paths) == 0L) {
      "No local browser automation executable was available; browser memory and signed-in landing were not measured."
    } else {
      "A browser executable is available; this offline runner still does not sign in or write production state."
    }
  )
}

acq_ux1_installed_api <- function() {
  package_root <- getOption("acq_ux1.package_root", getwd())
  workspace_root <- normalizePath(file.path(package_root, ".."), mustWork = FALSE)
  library_roots <- c(
    workspace = file.path(workspace_root, ".Rlib"),
    curation_service = file.path(workspace_root, ".curation-Rlib"),
    acquisition_service = file.path(workspace_root, ".acquisition-Rlib")
  )
  lapply(library_roots, acq_ux1_probe_r_library) |>
    stats::setNames(names(library_roots))
}

acq_ux1_probe_r_library <- function(library_root) {
  script <- paste(
    "library_root <- commandArgs(trailingOnly = TRUE)[[1L]]",
    "result <- list(library_root = library_root, exists = dir.exists(library_root))",
    "if (isTRUE(result$exists)) {",
    "  available <- requireNamespace('reactable', quietly = TRUE, lib.loc = library_root)",
    "  result$reactable_available <- available",
    "  if (isTRUE(available)) {",
    "    namespace <- asNamespace('reactable')",
    "    result$version <- as.character(utils::packageVersion('reactable', lib.loc = library_root))",
    "    result$reactable_formals <- names(formals(get('reactable', envir = namespace)))",
    "    exports <- getNamespaceExports(namespace)",
    "    result$server_exports <- intersect(exports, c('reactableOutput', 'renderReactable', 'getReactableState', 'updateReactable'))",
    "    result$server_capabilities <- list(",
    "      renderReactable = 'renderReactable' %in% exports,",
    "      reactableOutput = 'reactableOutput' %in% exports,",
    "      getReactableState = 'getReactableState' %in% exports,",
    "      updateReactable = 'updateReactable' %in% exports",
    "    )",
    "  }",
    "}",
    "cat(jsonlite::toJSON(result, auto_unbox = TRUE, null = 'null'))",
    sep = "\n"
  )
  output <- tryCatch(
    system2(
      file.path(R.home("bin"), "Rscript"),
      args = c("-e", shQuote(script), shQuote(library_root)),
      stdout = TRUE,
      stderr = TRUE,
      env = paste0("R_LIBS_USER=", library_root)
    ),
    error = function(error) character()
  )
  parsed <- tryCatch(
    jsonlite::fromJSON(tail(output, 1L), simplifyVector = FALSE),
    error = function(error) {
      list(
        library_root = library_root,
        exists = dir.exists(library_root),
        probe_error = conditionMessage(error)
      )
    }
  )
  parsed
}

acq_ux1_isolate_cache <- function() {
  cache_dir <- tempfile("acq-ux1-cache-")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  previous_env <- Sys.getenv("PLANSCANR_CACHE", unset = NA_character_)
  previous_option <- getOption("planscanR.cache_dir")
  Sys.setenv(PLANSCANR_CACHE = cache_dir)
  options(planscanR.cache_dir = cache_dir)
  list(
    path = cache_dir,
    restore = function() {
      if (is.na(previous_env)) {
        Sys.unsetenv("PLANSCANR_CACHE")
      } else {
        Sys.setenv(PLANSCANR_CACHE = previous_env)
      }
      options(planscanR.cache_dir = previous_option)
    }
  )
}

acq_ux1_dashboard_read_probe <- function() {
  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE)) {
    return(list(status = "unavailable", reason = "DBI and RSQLite are required"))
  }
  cache_scope <- acq_ux1_isolate_cache()
  on.exit(cache_scope$restore(), add = TRUE)
  catalog_path <- tempfile("acq-ux1-dashboard-", fileext = ".sqlite")
  catalog <- acq_ux1_ns("acquisition_catalog_open")(catalog_path)
  on.exit(acq_ux1_ns("acquisition_catalog_close")(catalog), add = TRUE)
  source <- acq_ux1_ns("acquisition_catalog_register_source")(
    catalog,
    "nl",
    "fixture.portal",
    "acq-ux1-dashboard-fixture"
  )
  job <- acq_ux1_ns("acquisition_queue_enqueue")(
    catalog,
    kind = "ingest",
    target_type = "source",
    target_ref_id = source$source_ref_id,
    idempotency_key = "acq-ux1-dashboard-attempts"
  )
  now <- acq_ux1_ns("acquisition_catalog_now")()
  insert_attempt <- paste(
    "INSERT INTO acquisition_job_attempts (job_id, attempt_no, lease_token, worker_id, host,",
    "claimed_at, lease_expires_at, heartbeat_at, finished_at, outcome, bytes)",
    "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, 'succeeded', ?)"
  )
  DBI::dbWithTransaction(catalog$conn, {
    for (index in seq_len(1000L)) {
      DBI::dbExecute(
        catalog$conn,
        insert_attempt,
        params = list(
          job$job_id,
          index,
          paste0("acq-ux1-attempt-token-", index),
          "acq-ux1-synthetic",
          NA_character_,
          now,
          now,
          now,
          now,
          as.numeric(index)
        )
      )
    }
    DBI::dbExecute(
      catalog$conn,
      paste(
        "UPDATE acquisition_jobs SET state = 'succeeded', attempt_count = ?,",
        "lease_token = NULL, worker_id = NULL, lease_expires_at = NULL,",
        "completed_at = ?, updated_at = ? WHERE job_id = ?"
      ),
      params = list(1000L, now, now, job$job_id)
    )
  })
  trace_env <- new.env(parent = emptyenv())
  trace_env$queries <- character()
  trace_env$fetch_rows <- integer()
  trace_name <- "acq_ux1_dashboard_db_trace_env"
  assign(trace_name, trace_env, envir = .GlobalEnv)
  on.exit(rm(list = trace_name, envir = .GlobalEnv), add = TRUE)
  trace(
    DBI::dbSendQuery,
    tracer = quote({
      env <- get("acq_ux1_dashboard_db_trace_env", envir = .GlobalEnv)
      env$queries <- c(env$queries, as.character(statement))
    }),
    print = FALSE
  )
  on.exit(untrace("dbSendQuery", where = getNamespace("DBI")), add = TRUE)
  trace(
    "dbFetch",
    signature = "SQLiteResult",
    where = getNamespace("RSQLite"),
    tracer = quote({}),
    exit = quote({
      env <- get("acq_ux1_dashboard_db_trace_env", envir = .GlobalEnv)
      if (grepl("SELECT a.bytes, a.claimed_at, a.finished_at", res@sql, fixed = TRUE)) {
        env$fetch_rows <- c(env$fetch_rows, nrow(returnValue()))
      }
    }),
    print = FALSE
  )
  on.exit(
    untrace("dbFetch", signature = "SQLiteResult", where = getNamespace("RSQLite")),
    add = TRUE
  )
  summary_fn <- acq_ux1_ns("acquisition_dashboard_summary")
  first <- summary_fn(catalog)
  second <- summary_fn(catalog)
  attempts_queries <- trace_env$queries[
    grepl("SELECT a.bytes, a.claimed_at, a.finished_at", trace_env$queries, fixed = TRUE)
  ]
  fetched_rows <- trace_env$fetch_rows
  first_ingestion <- first$ingestion
  second_ingestion <- second$ingestion
  list(
    status = "measured",
    summary_calls = 2L,
    synthetic_attempt_rows = 1000L,
    first_observed_completed_bytes = first_ingestion$observed_completed_bytes,
    second_observed_completed_bytes = second_ingestion$observed_completed_bytes,
    repeated_summary_parity = identical(first_ingestion, second_ingestion),
    ingestion_attempts_query_count = length(attempts_queries),
    ingestion_attempts_rows_fetched = fetched_rows,
    unaggregated_attempt_rows_materialized = sum(fetched_rows),
    query_count_within_summary_bound = length(attempts_queries) <= 2L,
    query_pattern = "SELECT a.bytes, a.claimed_at, a.finished_at FROM acquisition_job_attempts"
  )
}

acq_ux1_reactable_payload_metrics <- function(widget) {
  payload <- as.character(widget$x$tag$attribs$data %||% "")
  parsed <- tryCatch(
    jsonlite::fromJSON(payload, simplifyVector = FALSE),
    error = function(error) NULL
  )
  list(
    rows = if (is.list(parsed) && length(parsed) > 0L) {
      length(parsed[[1L]])
    } else {
      0L
    },
    bytes = length(charToRaw(payload)),
    columns = if (is.list(parsed)) names(parsed) else character()
  )
}

acq_ux1_surface_payloads <- function(config) {
  if (!requireNamespace("DBI", quietly = TRUE) ||
    !requireNamespace("RSQLite", quietly = TRUE) ||
    !requireNamespace("shiny", quietly = TRUE)) {
    return(list(status = "unavailable", reason = "DBI, RSQLite, and shiny are required"))
  }
  cache_scope <- acq_ux1_isolate_cache()
  on.exit(cache_scope$restore(), add = TRUE)
  root <- tempfile("acq-ux1-surfaces-")
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  catalog <- acq_ux1_ns("acquisition_catalog_open")(file.path(root, "catalog.sqlite"))
  on.exit(acq_ux1_ns("acquisition_catalog_close")(catalog), add = TRUE)
  artifact_path <- file.path(root, "fixture-model.rds")
  saveRDS(list(model = "fixture"), artifact_path)
  source_a <- acq_ux1_ns("acquisition_catalog_register_source")(
    catalog,
    "nl",
    "portal-a",
    "fixture-project-1",
    source_url = "https://portal-a.invalid/fixture-project-1"
  )
  source_b <- acq_ux1_ns("acquisition_catalog_register_source")(
    catalog,
    "de",
    "portal-b",
    "fixture-project-2",
    source_url = "https://portal-b.invalid/fixture-project-2"
  )
  selection <- data.frame(
    source_ref_id = c(source_a$source_ref_id, source_b$source_ref_id),
    country = c("nl", "de"),
    portal = c("portal-a", "portal-b"),
    native_project_id = c("fixture-project-1", "fixture-project-2"),
    probability = c(0.91, 0.31),
    selected = c(TRUE, FALSE),
    energy_tags = I(list("wind", "solar")),
    stringsAsFactors = FALSE
  )
  preview_args <- list(
    selection = selection,
    model_id = "acq-ux1-preview-model",
    input_snapshot_id = "acq-ux1-preview-snapshot",
    threshold = 0.5,
    policy_version = "acq-ux1-policy.v1",
    priority_rules = list(
      list(rule_id = "wind-first", countries = "nl", energy = "wind", priority = 90),
      list(rule_id = "fallback", priority = 10)
    ),
    artifact_path = artifact_path,
    config_id = config$config_id
  )
  preview <- do.call(
    acq_ux1_ns("acquisition_campaign_preview"),
    c(list(catalog = catalog), preview_args)
  )
  historical_links <- data.frame(
    import_row_id = c("hist-1", "hist-2"),
    country = c("nl", "de"),
    document_id = c("hist-1", "hist-2"),
    original_source = c("legacy-a", "legacy-b"),
    imported_source = c("legacy-a", "legacy-b"),
    historical_reviewer = c("legacy-reviewer", "legacy-reviewer"),
    historical_linkage = c("legacy_exact", "legacy_exact"),
    current_match_count = c(0L, 1L),
    current_linkage = c("historical_only", "current_unique"),
    linkage_status = c("historical_only", "current_unique"),
    mutation_allowed = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  current_records <- data.frame(
    country = "de",
    document_id = "hist-2",
    title = "Current fixture record",
    stringsAsFactors = FALSE
  )
  reviews <- data.frame(
    country = "de",
    document_id = "hist-2",
    reviewer_id = "fixture-reviewer",
    decision = "relevant",
    source = "browse",
    note = NA_character_,
    reviewed_at = as.POSIXct(0, origin = "1970-01-01", tz = "UTC"),
    sidecar_path = NA_character_,
    revision = 1L,
    deleted = FALSE,
    stringsAsFactors = FALSE
  )
  historical_widget <- acq_ux1_ns("curation_app_historical_only_table")(
    historical_links,
    current_records = current_records,
    reviews = reviews
  )
  admin_widget <- acq_ux1_ns("curation_app_admin_table")(
    reviews,
    historical_links = historical_links
  )
  campaign_render <- new.env(parent = emptyenv())
  campaign_render$value <- NULL
  shiny::testServer(
    acq_ux1_ns("acquisition_campaign_panel_server"),
    args = list(
      id = "campaign",
      catalog = catalog,
      preview = preview_args,
      actor_resolver = acq_ux1_ns("curation_actor_resolver")(
        function(request) acq_ux1_ns("curation_actor")("acq-ux1-fixture-admin", roles = "admin")
      )
    ),
    {
      session$setInputs(`campaign-threshold` = 0.5)
      session$flushReact()
      campaign_render$value <- output$`campaign-items`
    }
  )
  campaign_html <- if (is.character(campaign_render$value)) {
    paste(campaign_render$value, collapse = "")
  } else {
    as.character(campaign_render$value %||% "")
  }
  campaign_tr <- gregexpr("<tr", campaign_html, fixed = TRUE)[[1L]]
  campaign_html_rows <- if (identical(campaign_tr, -1L)) {
    0L
  } else {
    max(0L, length(campaign_tr) - 1L)
  }
  preview_json_bytes <- length(charToRaw(jsonlite::toJSON(
    preview$items,
    dataframe = "rows",
    auto_unbox = TRUE
  )))
  list(
    status = "measured",
    acquisition_preview = list(
      rows = campaign_html_rows,
      bytes = length(charToRaw(campaign_html)),
      html_bytes = length(charToRaw(campaign_html)),
      html_rows = campaign_html_rows,
      json_rows = nrow(preview$items),
      json_bytes = preview_json_bytes,
      render = "acquisition_campaign_panel_server output[['campaign-items']] via shiny::testServer (renderTable HTML)",
      selected_rows = preview$counts$selected_projects,
      jobs = preview$counts$jobs
    ),
    historical_table = acq_ux1_reactable_payload_metrics(historical_widget),
    admin_table = acq_ux1_reactable_payload_metrics(admin_widget)
  )
}

acq_ux1_representative_corpus <- function(cache_dir) {
  sidecar_root <- file.path(cache_dir, "files")
  if (!dir.exists(sidecar_root)) {
    return(list(status = "unavailable", cache_dir = cache_dir))
  }
  metadata_files <- list.files(
    sidecar_root,
    pattern = "\\.meta\\.json$",
    recursive = TRUE,
    full.names = TRUE
  )
  info <- file.info(metadata_files)
  countries <- if (length(metadata_files) == 0L) {
    integer(0)
  } else {
    sort(table(basename(dirname(dirname(metadata_files)))))
  }
  list(
    status = "available",
    cache_dir = cache_dir,
    metadata_sidecar_count = length(metadata_files),
    metadata_sidecar_bytes = sum(info$size, na.rm = TRUE),
    country_sidecar_counts = as.list(countries),
    contents_read = FALSE,
    note = "Only file names, sizes, and country directory counts were inspected; sidecar contents were not read."
  )
}

acq_ux1_run <- function(
  output_json,
  output_markdown = sub("\\.json$", ".md", output_json),
  cache_dir = "/data/biogain-docs/data/biogain/cache",
  package_root = getOption("acq_ux1.package_root", NA_character_)
) {
  dir.create(dirname(output_json), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(output_markdown), recursive = TRUE, showWarnings = FALSE)
  records <- acq_ux1_fixture_records()
  config <- acq_ux1_fixture_config(root = tempfile("acq-ux1-state-"))
  provider <- acq_ux1_fixture_provider(records)
  snapshot <- acq_ux1_ns("curation_records_snapshot")
  apply_selection <- acq_ux1_ns("curation_apply_selection")
  poll_token <- acq_ux1_ns("acquisition_dashboard_poll_token")
  dashboard_read_probe <- acq_ux1_dashboard_read_probe()
  surface_payloads <- acq_ux1_surface_payloads(config)

  cold <- acq_ux1_measure(
    "cold_landing_snapshot",
    function() snapshot(provider, config),
    function(value) list(rows = nrow(value), columns = ncol(value))
  )
  warm <- acq_ux1_measure(
    "warm_landing_snapshot",
    function() snapshot(provider, config),
    function(value) list(rows = nrow(value), columns = ncol(value))
  )
  cold_snapshot <- snapshot(provider, config)
  cap <- function(value) acq_ux1_current_display_cap(value)
  widget_detail <- function(value) value
  actions <- list(
    acq_ux1_measure(
      "first_tab",
      function() acq_ux1_widget_metrics(cap(cold_snapshot), config),
      widget_detail
    ),
    acq_ux1_measure(
      "repeat_tab",
      function() acq_ux1_widget_metrics(cap(cold_snapshot), config),
      widget_detail
    ),
    acq_ux1_measure(
      "first_page_helper_simulation",
      function() acq_ux1_widget_metrics(cold_snapshot[seq_len(25L), , drop = FALSE], config),
      widget_detail
    ),
    acq_ux1_measure(
      "repeat_page_helper_simulation",
      function() acq_ux1_widget_metrics(cold_snapshot[seq_len(25L), , drop = FALSE], config),
      widget_detail
    ),
    acq_ux1_measure(
      "first_filter_country_nl",
      function() acq_ux1_widget_metrics(cap(cold_snapshot[cold_snapshot$country == "nl", , drop = FALSE]), config),
      widget_detail
    ),
    acq_ux1_measure(
      "repeat_filter_country_nl",
      function() acq_ux1_widget_metrics(cap(cold_snapshot[cold_snapshot$country == "nl", , drop = FALSE]), config),
      widget_detail
    ),
    acq_ux1_measure(
      "first_threshold",
      function() sum(apply_selection(config, cold_snapshot, list(relevance_threshold = 0.7))$selected),
      function(value) list(selected_rows = as.integer(value))
    ),
    acq_ux1_measure(
      "repeat_threshold",
      function() sum(apply_selection(config, cold_snapshot, list(relevance_threshold = 0.7))$selected),
      function(value) list(selected_rows = as.integer(value))
    )
  )
  names(actions) <- vapply(actions, `[[`, character(1), "label")

  token_one <- poll_token(NULL)
  Sys.sleep(0.01)
  token_two <- poll_token(NULL)
  report <- list(
    schema = "planscanR.acq-ux1-characterization.v1",
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    execution = list(
      mode = "offline_disposable_fixture",
      writes_to_production = FALSE,
      secrets_read = FALSE,
      browser = acq_ux1_browser_tooling(),
      source_checkout = list(
        root = package_root,
        load_mode = "pkgload::load_all"
      )
    ),
    synthetic_fixture = list(
      rows = nrow(records),
      columns = ncol(records),
      countries = sort(unique(records$country)),
      native_document_id_count = length(unique(records$document_id)),
      collision_cases = sum(duplicated(records$document_id)),
      composite_key_count = length(unique(paste(records$country, records$document_id, sep = "::"))),
      raw_object_bytes = as.numeric(object.size(records)),
      hidden_fields = c("internal_note", "source_payload")
    ),
    representative_corpus = acq_ux1_representative_corpus(cache_dir),
    landing = list(cold = cold, warm = warm),
    actions = actions,
    ux_surface_payloads = surface_payloads,
    provider_calls = list(
      read_calls = attr(provider, "acq_ux1_state")$reads,
      refresh_calls = attr(provider, "acq_ux1_state")$refreshes
    ),
    server_memory = acq_ux1_memory(),
    acquisition_polling = list(
      token_one = token_one,
      token_two = token_two,
      unchanged_source_tokens_identical = identical(token_one, token_two),
      worker_idle_poll_seconds = 5,
      dashboard_default_refresh_ms = 2000L,
      implementation = "acquisition_dashboard_poll_token() returns Sys.time(); no database change was made.",
      dashboard_read_probe = dashboard_read_probe
    ),
    installed_widget_apis = acq_ux1_installed_api(),
    source_contracts = list(
      screen_table = "R/curation_app.R:980-1050",
      display_cap = "R/curation_app.R:2789-2795 (5000 rows)",
      display_identity = "R/curation_app.R:511-524 (country/document/revision fields)",
      acquisition_poll_token = "R/acquisition_dashboard.R:330-334"
    )
  )
  jsonlite::write_json(report, output_json, auto_unbox = TRUE, pretty = TRUE, null = "null")
  acq_ux1_write_markdown(report, output_markdown)
  invisible(report)
}

acq_ux1_write_markdown <- function(report, path) {
  fmt <- function(value) format(value, big.mark = ",", scientific = FALSE, trim = TRUE)
  actions <- report$actions
  lines <- c(
    "# ACQ-UX1 sanitized characterization baseline",
    "",
    paste0("Generated: `", report$generated_at_utc, "`"),
    "",
    "This report is an offline, disposable fixture measurement. It does not read sidecar contents, read secrets, sign in, or write production state.",
    "",
    "## Fixture and corpus",
    "",
    paste0("- Synthetic fixture: **", fmt(report$synthetic_fixture$rows), " rows**, ", report$synthetic_fixture$columns, " columns; ", report$synthetic_fixture$native_document_id_count, " native document IDs across ", report$synthetic_fixture$composite_key_count, " country/document keys."),
    paste0("- Native-ID collision cases across countries: **", fmt(report$synthetic_fixture$collision_cases), "**."),
    paste0("- Representative cache status: **", report$representative_corpus$status, "**; metadata sidecars: ", fmt(report$representative_corpus$metadata_sidecar_count %||% NA_integer_), "."),
    "- Representative cache inspection used names, file sizes, and country directory counts only.",
    "",
    "## Timings and payloads",
    "",
    "| Action | Seconds | RSS after (bytes) | Rows in payload | Payload bytes |",
    "| --- | ---: | ---: | ---: | ---: |",
    paste(vapply(actions, function(action) {
      detail <- action$detail
      paste0(
        "| ", action$label, " | ", sprintf("%.3f", action$elapsed_seconds),
        " | ", fmt(action$server_rss_after_bytes), " | ",
        fmt(detail$payload_rows %||% detail$selected_rows %||% NA_integer_), " | ",
        fmt(detail$payload_bytes %||% NA_integer_), " |"
      )
    }, character(1)), collapse = "\n"),
    "",
    paste0("Cold landing snapshot: **", sprintf("%.3f", report$landing$cold$elapsed_seconds), " s**; warm landing: **", sprintf("%.3f", report$landing$warm$elapsed_seconds), " s**."),
    paste0("The current screen app caps the displayed frame at 5,000 rows; this fixture therefore reports the cap in table actions while retaining the 128,000-row server snapshot."),
    "Page timings in this headless runner are helper simulations that pass a 25-row frame directly to the widget. The browser fixture measures actual page navigation separately; the current app initially sends the capped 5,000-row display frame.",
    "",
    "## Browser and polling status",
    "",
    paste0("- Browser tooling: **", report$execution$browser$status, "**. ", report$execution$browser$note),
    paste0("- No-change polling tokens identical: **", report$acquisition_polling$unchanged_source_tokens_identical, "**; this is a token-only observation. The separate dashboard probe issued **", report$acquisition_polling$dashboard_read_probe$ingestion_attempts_query_count %||% NA_integer_, "** ingestion-attempt reads across two unchanged summaries and materialized **", report$acquisition_polling$dashboard_read_probe$unaggregated_attempt_rows_materialized %||% NA_integer_, "** attempt rows from a 1,000-row synthetic set; configured BIOGAIN worker poll interval is recorded separately."),
    "- The installed widget API inventory is stored in the JSON report; the current table sends its full data frame as widget JSON while marking non-display columns hidden.",
    "",
    "## Additional UX surface payloads",
    "",
    paste0("- Acquisition preview item render: **", report$ux_surface_payloads$acquisition_preview$html_rows %||% report$ux_surface_payloads$acquisition_preview$rows %||% NA_integer_, " HTML data rows / ", fmt(report$ux_surface_payloads$acquisition_preview$html_bytes %||% report$ux_surface_payloads$acquisition_preview$bytes %||% NA_integer_), " bytes** from `output[['campaign-items']]` via `shiny::testServer` (`renderTable`); the same preview JSON is ", fmt(report$ux_surface_payloads$acquisition_preview$json_bytes %||% NA_integer_), " bytes across ", report$ux_surface_payloads$acquisition_preview$json_rows %||% NA_integer_, " rows. Selected projects: ", report$ux_surface_payloads$acquisition_preview$selected_rows %||% NA_integer_, "."),
    paste0("- Historical-only table render: **", report$ux_surface_payloads$historical_table$rows %||% NA_integer_, " rows / ", fmt(report$ux_surface_payloads$historical_table$bytes %||% NA_integer_), " bytes**."),
    paste0("- Admin review table render: **", report$ux_surface_payloads$admin_table$rows %||% NA_integer_, " rows / ", fmt(report$ux_surface_payloads$admin_table$bytes %||% NA_integer_), " bytes**."),
    "These are offline synthetic payload measurements of the existing preview, historical, and admin table builders; no submit or review mutation was performed.",
    "",
    "## Prior representative production record",
    "",
    "The existing C4 deployment evidence records a 34,423-row, 98-column, 141,545,328-byte frame and approximately 490,576 KiB process RSS high-water during a prior production load. That historical observation is kept separate from this offline fixture timing and is not a new production measurement.",
    "",
    "## Deliberate future regressions",
    "",
    "The runnable suite under `tests/characterization/test-acq-ux1-future.R` intentionally remains outside the default test directory. It exercises the real app server for the current 5,000-row cap, records the existing country/document/revision identity as a passing collision test, and retains red evidence for bounded page payloads, hidden-field exclusion, stable no-change poll tokens, and repeated unchanged dashboard reads."
  )
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

`%||%` <- function(left, right) {
  if (is.null(left) || length(left) == 0L) right else left
}

if (!interactive() && any(grepl("^--output=", commandArgs(trailingOnly = TRUE)))) {
  args <- commandArgs(trailingOnly = TRUE)
  value_for <- function(name, default = NULL) {
    hit <- grep(paste0("^--", name, "="), args, value = TRUE)
    if (length(hit) == 0L) default else sub(paste0("^--", name, "="), "", hit[[1L]])
  }
  script_file <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[[1L]])
  package_root <- normalizePath(file.path(dirname(script_file), "..", ".."), mustWork = TRUE)
  acq_ux1_load_screen_package(package_root)
  options(acq_ux1.package_root = package_root)
  output_json <- value_for("output")
  output_markdown <- value_for("markdown", sub("\\.json$", ".md", output_json))
  cache_dir <- value_for("cache-dir", "/data/biogain-docs/data/biogain/cache")
  acq_ux1_run(output_json, output_markdown, cache_dir, package_root = package_root)
}
