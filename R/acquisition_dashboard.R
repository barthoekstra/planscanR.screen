# ACQ-2b: database-backed acquisition status projections and a small UI.

acquisition_dashboard_require_ui <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    cli::cli_abort(
      "The acquisition dashboard needs the {.pkg shiny} package.",
      class = "planscanR_error_missing_dep"
    )
  }
  invisible(TRUE)
}

acquisition_dashboard_empty <- function(columns) {
  out <- lapply(columns, function(type) {
    switch(type,
      character = character(0),
      integer = integer(0),
      numeric = numeric(0),
      logical = logical(0)
    )
  })
  names(out) <- names(columns)
  as.data.frame(out, stringsAsFactors = FALSE)
}

acquisition_dashboard_unblob <- function(value) {
  if (exists("acquisition_queue_unblob", mode = "function", inherits = TRUE)) {
    return(acquisition_queue_unblob(value))
  }
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]]) || !nzchar(value[[1L]])) {
    return(NULL)
  }
  hex <- as.character(value[[1L]])
  bytes <- tryCatch(
    as.raw(strtoi(
      substring(hex, seq(1L, nchar(hex), by = 2L), seq(2L, nchar(hex), by = 2L)),
      base = 16L
    )),
    error = function(error) NULL
  )
  if (is.null(bytes)) {
    return(NULL)
  }
  tryCatch(unserialize(bytes), error = function(error) NULL)
}

acquisition_dashboard_project_columns <- function() {
  c(
    source_ref_id = "character", country = "character", portal = "character",
    native_project_id = "character", document_count = "integer",
    verified_document_count = "integer", claimed_document_count = "integer",
    blocked_document_count = "integer", pending_document_count = "integer",
    job_id = "character", queue_state = "character", last_error_category = "character",
    last_error = "character", attempt_outcome = "character", attempt_finished_at = "character",
    attempt_heartbeat_at = "character", energy_kind = "character", inventory_status = "character"
  )
}

acquisition_dashboard_project_base_sql <- function() {
  paste(
    "WITH campaign_energy AS (",
    "SELECT source_ref_id, CASE WHEN COUNT(DISTINCT energy_kind) > 1 THEN 'mixed'",
    "ELSE COALESCE(MAX(energy_kind), 'unknown') END AS energy_kind",
    "FROM acquisition_campaign_items GROUP BY source_ref_id",
    "), project_rows AS (",
    "SELECT s.source_ref_id, s.country, s.portal, s.native_project_id,",
    "COUNT(DISTINCT d.document_ref_id) AS document_count,",
    "COUNT(DISTINCT CASE WHEN f.verification_status = 'verified' THEN d.document_ref_id END) AS verified_document_count,",
    "COUNT(DISTINCT CASE WHEN f.file_ref_id IS NOT NULL THEN d.document_ref_id END) AS claimed_document_count,",
    "COUNT(DISTINCT CASE WHEN dj.state IN ('blocked', 'failed', 'cancelled') THEN d.document_ref_id END) AS blocked_document_count,",
    "COUNT(DISTINCT CASE WHEN dj.state IN ('queued', 'running', 'retry_wait') THEN d.document_ref_id END) AS pending_document_count,",
    "j.job_id, j.state AS queue_state, j.last_error_category, j.last_error,",
    "a.outcome AS attempt_outcome, a.finished_at AS attempt_finished_at,",
    "a.heartbeat_at AS attempt_heartbeat_at, COALESCE(ce.energy_kind, 'unknown') AS energy_kind",
    "FROM acquisition_sources s",
    "LEFT JOIN acquisition_documents d ON d.source_ref_id = s.source_ref_id",
    "LEFT JOIN acquisition_files f ON f.document_ref_id = d.document_ref_id",
    "LEFT JOIN acquisition_jobs dj ON dj.kind = 'download' AND dj.target_type = 'document'",
    "AND dj.target_ref_id = d.document_ref_id",
    "LEFT JOIN acquisition_jobs j ON j.kind = 'discover' AND j.target_type = 'source'",
    "AND j.target_ref_id = s.source_ref_id",
    "LEFT JOIN acquisition_job_attempts a ON a.job_id = j.job_id",
    "AND a.attempt_no = (SELECT MAX(a2.attempt_no) FROM acquisition_job_attempts a2 WHERE a2.job_id = j.job_id)",
    "LEFT JOIN campaign_energy ce ON ce.source_ref_id = s.source_ref_id",
    "GROUP BY s.source_ref_id, s.country, s.portal, s.native_project_id,",
    "j.job_id, j.state, j.last_error_category, j.last_error,",
    "a.outcome, a.finished_at, a.heartbeat_at, ce.energy_kind",
    "), classified AS (",
    "SELECT project_rows.*, CASE",
    "WHEN attempt_outcome = 'succeeded' AND document_count = 0 THEN 'successful_empty'",
    "WHEN attempt_outcome = 'succeeded' AND document_count > 0 AND verified_document_count = document_count THEN 'complete'",
    "WHEN attempt_outcome = 'succeeded' AND document_count > 0 AND blocked_document_count > 0 THEN 'blocked'",
    "WHEN attempt_outcome = 'succeeded' AND document_count > 0 AND (verified_document_count > 0 OR claimed_document_count > 0) THEN 'partial'",
    "WHEN attempt_outcome = 'succeeded' AND document_count > 0 AND pending_document_count > 0 THEN 'pending'",
    "WHEN attempt_outcome = 'succeeded' AND document_count > 0 THEN 'discovered'",
    "WHEN queue_state IN ('failed', 'blocked', 'cancelled') OR attempt_outcome IN ('failed', 'blocked', 'cancelled') THEN 'error'",
    "ELSE 'unknown' END AS inventory_status",
    "FROM project_rows)",
    "SELECT * FROM classified"
  )
}

acquisition_dashboard_projects <- function(catalog, page = 1L, page_size = 50L) {
  page_values <- acquisition_dashboard_validate_page(page, page_size)
  conn <- acquisition_catalog_connection(catalog)
  base <- acquisition_dashboard_project_base_sql()
  count <- DBI::dbGetQuery(conn, paste("SELECT COUNT(*) AS n FROM (", base, ") AS project_rows"))$n[[1L]]
  rows <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT * FROM (", base, ") AS project_rows",
      "ORDER BY country, portal, native_project_id LIMIT ? OFFSET ?"
    ),
    params = list(page_values$page_size, (page_values$page - 1L) * page_values$page_size)
  )
  if (nrow(rows) == 0L) {
    rows <- acquisition_dashboard_empty(acquisition_dashboard_project_columns())
  } else {
    for (column in c("document_count", "verified_document_count", "claimed_document_count", "blocked_document_count", "pending_document_count")) {
      rows[[column]] <- as.integer(rows[[column]])
    }
  }
  attr(rows, "total_count") <- as.integer(count)
  attr(rows, "page") <- page_values$page
  attr(rows, "page_size") <- page_values$page_size
  rows
}

acquisition_dashboard_project_counts <- function(catalog) {
  conn <- acquisition_catalog_connection(catalog)
  base <- acquisition_dashboard_project_base_sql()
  rows <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT country, energy_kind, COUNT(*) AS project_count,",
      "SUM(CASE WHEN attempt_outcome = 'succeeded' AND document_count > 0 THEN 1 ELSE 0 END) AS discovered_count,",
      "SUM(CASE WHEN inventory_status = 'complete' THEN 1 ELSE 0 END) AS complete_count,",
      "SUM(CASE WHEN inventory_status = 'partial' THEN 1 ELSE 0 END) AS partial_count,",
      "SUM(CASE WHEN inventory_status = 'blocked' THEN 1 ELSE 0 END) AS blocked_count,",
      "SUM(CASE WHEN inventory_status = 'pending' THEN 1 ELSE 0 END) AS pending_count,",
      "SUM(CASE WHEN inventory_status = 'discovered' THEN 1 ELSE 0 END) AS discovered_only_count,",
      "SUM(CASE WHEN inventory_status = 'successful_empty' THEN 1 ELSE 0 END) AS successful_empty_count,",
      "SUM(CASE WHEN inventory_status = 'unknown' THEN 1 ELSE 0 END) AS unknown_count,",
      "SUM(CASE WHEN inventory_status = 'error' THEN 1 ELSE 0 END) AS error_count",
      "FROM (", base, ") AS project_rows GROUP BY country, energy_kind",
      "ORDER BY country, energy_kind"
    )
  )
  if (nrow(rows) == 0L) {
    return(acquisition_dashboard_empty(c(
      country = "character", energy_kind = "character", project_count = "integer",
      discovered_count = "integer", complete_count = "integer", partial_count = "integer",
      blocked_count = "integer", pending_count = "integer", discovered_only_count = "integer",
      successful_empty_count = "integer", unknown_count = "integer", error_count = "integer"
    )))
  }
  for (column in setdiff(names(rows), c("country", "energy_kind"))) rows[[column]] <- as.integer(rows[[column]])
  rows
}

acquisition_dashboard_files <- function(catalog, page = 1L, page_size = 50L) {
  page_values <- acquisition_dashboard_validate_page(page, page_size)
  conn <- acquisition_catalog_connection(catalog)
  from <- paste(
    "FROM acquisition_documents d JOIN acquisition_sources s ON s.source_ref_id = d.source_ref_id",
    "LEFT JOIN acquisition_files f ON f.document_ref_id = d.document_ref_id"
  )
  count <- DBI::dbGetQuery(conn, paste("SELECT COUNT(*) AS n", from))$n[[1L]]
  rows <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT d.document_ref_id, d.source_ref_id, s.country, s.portal,",
      "f.file_ref_id, COALESCE(f.verification_status, 'unknown') AS verification_status, f.size_bytes",
      from,
      "ORDER BY s.country, s.portal, d.document_ref_id, f.file_ref_id LIMIT ? OFFSET ?"
    ),
    params = list(page_values$page_size, (page_values$page - 1L) * page_values$page_size)
  )
  if (nrow(rows) == 0L) {
    rows <- acquisition_dashboard_empty(c(
      document_ref_id = "character", source_ref_id = "character", country = "character",
      portal = "character", file_ref_id = "character", verification_status = "character",
      size_bytes = "numeric"
    ))
  } else {
    rows$size_bytes <- suppressWarnings(as.numeric(rows$size_bytes))
  }
  attr(rows, "total_count") <- as.integer(count)
  attr(rows, "page") <- page_values$page
  attr(rows, "page_size") <- page_values$page_size
  rows
}

acquisition_dashboard_file_counts <- function(catalog) {
  rows <- DBI::dbGetQuery(
    acquisition_catalog_connection(catalog),
    paste(
      "SELECT s.country, COALESCE(f.verification_status, 'unknown') AS verification_status,",
      "COUNT(*) AS file_count,",
      "COALESCE(SUM(CASE WHEN f.size_bytes IS NOT NULL THEN f.size_bytes ELSE 0 END), 0) AS known_bytes,",
      "SUM(CASE WHEN f.size_bytes IS NULL THEN 1 ELSE 0 END) AS unknown_bytes",
      "FROM acquisition_documents d JOIN acquisition_sources s ON s.source_ref_id = d.source_ref_id",
      "LEFT JOIN acquisition_files f ON f.document_ref_id = d.document_ref_id",
      "GROUP BY s.country, COALESCE(f.verification_status, 'unknown')",
      "ORDER BY s.country, verification_status"
    )
  )
  if (nrow(rows) == 0L) {
    return(acquisition_dashboard_empty(c(
      country = "character", verification_status = "character", file_count = "integer",
      known_bytes = "numeric", unknown_bytes = "integer"
    )))
  }
  for (column in c("file_count", "unknown_bytes")) rows[[column]] <- as.integer(rows[[column]])
  rows$known_bytes <- as.numeric(rows$known_bytes)
  rows
}

acquisition_dashboard_jobs_raw <- function(catalog, page = 1L, page_size = 50L) {
  acquisition_dashboard_jobs(catalog, page = page, page_size = page_size)
}

acquisition_dashboard_stage_label <- function(kind) {
  ifelse(kind == "discover", "discovery", ifelse(kind == "migrate_storage", "storage_migration", "download"))
}

acquisition_dashboard_stage_counts <- function(jobs) {
  if (inherits(jobs, "planscanR_acquisition_catalog")) {
    rows <- DBI::dbGetQuery(
      acquisition_catalog_connection(jobs),
      paste(
        "SELECT CASE WHEN kind = 'discover' THEN 'discovery'",
        "WHEN kind = 'migrate_storage' THEN 'storage_migration' ELSE 'download' END AS stage,",
        "state, COUNT(*) AS job_count FROM acquisition_jobs",
        "GROUP BY stage, state ORDER BY stage, state"
      )
    )
    if (nrow(rows) == 0L) {
      return(acquisition_dashboard_empty(c(stage = "character", state = "character", job_count = "integer")))
    }
    rows$job_count <- as.integer(rows$job_count)
    return(rows)
  }
  if (nrow(jobs) == 0L) {
    return(acquisition_dashboard_empty(c(stage = "character", state = "character", job_count = "integer")))
  }
  jobs$stage <- acquisition_dashboard_stage_label(jobs$kind)
  keys <- unique(jobs[, c("stage", "state"), drop = FALSE])
  out <- lapply(seq_len(nrow(keys)), function(i) {
    data.frame(
      stage = keys$stage[[i]],
      state = keys$state[[i]],
      job_count = sum(jobs$stage == keys$stage[[i]] & jobs$state == keys$state[[i]]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

acquisition_dashboard_workers <- function(catalog, page = 1L, page_size = 50L) {
  page_values <- acquisition_dashboard_validate_page(page, page_size)
  conn <- acquisition_catalog_connection(catalog)
  count <- DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM acquisition_jobs WHERE state = 'running'")$n[[1L]]
  rows <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT j.job_id, j.kind, j.target_type, j.target_ref_id, j.campaign_id,",
      "j.worker_id, j.lease_expires_at, a.attempt_no, a.heartbeat_at, a.claimed_at,",
      "a.host, j.updated_at",
      "FROM acquisition_jobs j JOIN acquisition_job_attempts a ON a.job_id = j.job_id",
      "AND a.lease_token = j.lease_token",
      "WHERE j.state = 'running' ORDER BY a.heartbeat_at DESC, j.job_id LIMIT ? OFFSET ?"
    ),
    params = list(page_values$page_size, (page_values$page - 1L) * page_values$page_size)
  )
  attr(rows, "total_count") <- as.integer(count)
  attr(rows, "page") <- page_values$page
  attr(rows, "page_size") <- page_values$page_size
  rows
}

acquisition_dashboard_progress <- function(catalog) {
  conn <- acquisition_catalog_connection(catalog)
  scalar_or_zero <- function(value, numeric = FALSE) {
    value <- suppressWarnings(as.numeric(value[[1L]]))
    if (length(value) == 0L || is.na(value)) {
      return(if (numeric) 0 else 0L)
    }
    if (numeric) value else as.integer(value)
  }
  jobs <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT COUNT(*) AS total_jobs,",
      "SUM(CASE WHEN state = 'succeeded' THEN 1 ELSE 0 END) AS succeeded_jobs,",
      "SUM(CASE WHEN state = 'running' THEN 1 ELSE 0 END) AS running_jobs,",
      "SUM(CASE WHEN state IN ('queued', 'retry_wait') THEN 1 ELSE 0 END) AS pending_jobs,",
      "SUM(CASE WHEN state IN ('failed', 'blocked', 'cancelled') THEN 1 ELSE 0 END) AS failed_jobs",
      "FROM acquisition_jobs"
    )
  )
  files <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT COALESCE(SUM(CASE WHEN f.verification_status = 'verified' AND f.size_bytes IS NOT NULL THEN f.size_bytes ELSE 0 END), 0) AS verified_bytes,",
      "SUM(CASE WHEN f.verification_status = 'verified' AND f.size_bytes IS NULL THEN 1 ELSE 0 END) AS unknown_file_bytes",
      "FROM acquisition_files f"
    )
  )
  total <- scalar_or_zero(jobs$total_jobs)
  succeeded <- scalar_or_zero(jobs$succeeded_jobs)
  running <- scalar_or_zero(jobs$running_jobs)
  pending <- scalar_or_zero(jobs$pending_jobs)
  failed <- scalar_or_zero(jobs$failed_jobs)
  completed <- succeeded + failed
  list(
    total_jobs = total,
    completed_jobs = completed,
    succeeded_jobs = succeeded,
    running_jobs = running,
    pending_jobs = pending,
    failed_jobs = failed,
    completion_fraction = if (total == 0L) NA_real_ else completed / total,
    verified_bytes = scalar_or_zero(files$verified_bytes, numeric = TRUE),
    unknown_file_bytes = scalar_or_zero(files$unknown_file_bytes),
    unknown_projects = NA_integer_
  )
}

acquisition_dashboard_poll_token <- function(catalog) {
  # The bounded reactivePoll interval is the refresh clock.  The token changes
  # only when a persisted acquisition timestamp changes, so an inactive tab can
  # stay quiet and an active tab does not rebuild its summary on every timer
  # tick.  Each branch returns one scalar; attempt history is never fetched.
  if (is.null(catalog)) {
    return("inactive")
  }
  conn <- acquisition_catalog_connection(catalog)
  token_row <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT MAX(token) AS token FROM (",
      "SELECT MAX(updated_at) AS token FROM acquisition_jobs",
      "UNION ALL SELECT MAX(heartbeat_at) FROM acquisition_job_attempts",
      "UNION ALL SELECT MAX(finished_at) FROM acquisition_job_attempts",
      "UNION ALL SELECT MAX(updated_at) FROM acquisition_campaign_items",
      "UNION ALL SELECT MAX(submitted_at) FROM acquisition_campaigns",
      "UNION ALL SELECT MAX(created_at) FROM acquisition_sources",
      "UNION ALL SELECT MAX(created_at) FROM acquisition_documents",
      "UNION ALL SELECT MAX(observed_at) FROM acquisition_files",
      ") AS acquisition_changes"
    )
  )
  token <- token_row$token[[1L]]
  token <- as.character(token %||% "")
  if (length(token) == 0L || is.na(token)) "" else token
}

#' Return database-derived acquisition status, progress, and heartbeat data.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param project_page One-based project detail page.
#' @param file_page One-based file detail page.
#' @param job_page One-based queue detail page.
#' @param page_size Rows per detail page.
#' @return A list containing project/file/stage counts, progress, heartbeats,
#'   and explicit unknown totals.
#' @export
acquisition_dashboard_summary <- function(
  catalog,
  project_page = 1L,
  file_page = 1L,
  job_page = 1L,
  page_size = 50L
) {
  acquisition_catalog_connection(catalog)
  acquisition_controls_ensure_schema(catalog)
  page_values <- acquisition_dashboard_validate_page(1L, page_size)
  projects <- acquisition_dashboard_projects(catalog, page = project_page, page_size = page_values$page_size)
  files <- acquisition_dashboard_files(catalog, page = file_page, page_size = page_values$page_size)
  jobs <- acquisition_dashboard_jobs_raw(catalog, page = job_page, page_size = page_values$page_size)
  project_counts <- acquisition_dashboard_project_counts(catalog)
  file_counts <- acquisition_dashboard_file_counts(catalog)
  stages <- acquisition_dashboard_stage_counts(catalog)
  unknown_projects <- sum(project_counts$unknown_count)
  inventory_errors <- sum(project_counts$error_count)
  unknown_file_count <- sum(file_counts$file_count[file_counts$verification_status == "unknown"])
  unknown_bytes <- sum(file_counts$unknown_bytes)
  progress <- acquisition_dashboard_progress(catalog)
  progress$unknown_projects <- as.integer(unknown_projects)
  list(
    refreshed_at = acquisition_catalog_now(),
    projects = projects,
    project_counts = project_counts,
    files = files,
    file_counts = file_counts,
    stages = stages,
    stage_counts = stages,
    ingestion = acquisition_dashboard_ingestion_metrics(catalog),
    jobs = jobs,
    workers = acquisition_dashboard_workers(catalog),
    progress = progress,
    unknown_totals = list(
      projects = as.integer(unknown_projects),
      inventory_errors = as.integer(inventory_errors),
      files = as.integer(unknown_file_count),
      bytes = as.integer(unknown_bytes)
    )
  )
}

acquisition_dashboard_ingestion_metrics <- function(catalog) {
  conn <- acquisition_catalog_connection(catalog)
  states <- DBI::dbGetQuery(
    conn,
    "SELECT state, COUNT(*) AS jobs FROM acquisition_jobs WHERE kind = 'ingest' GROUP BY state"
  )
  counts <- stats::setNames(as.integer(states$jobs), as.character(states$state))
  state_count <- function(name) {
    value <- counts[name]
    if (length(value) == 0L || is.na(value[[1L]])) 0L else as.integer(value[[1L]])
  }
  # Keep attempt history in SQLite.  The dashboard only needs aggregate
  # counters and rates; selecting every successful attempt here made a large
  # ingestion history part of every poll's R memory footprint.
  aggregate <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT COUNT(a.bytes) AS byte_count,",
      "SUM(a.bytes) AS completed_bytes,",
      "SUM(CASE WHEN a.bytes IS NOT NULL AND a.claimed_at IS NOT NULL",
      "AND a.finished_at IS NOT NULL AND julianday(a.finished_at) > julianday(a.claimed_at)",
      "THEN 1 ELSE 0 END) AS measured_count,",
      "SUM(CASE WHEN a.bytes IS NOT NULL AND a.claimed_at IS NOT NULL",
      "AND a.finished_at IS NOT NULL AND julianday(a.finished_at) > julianday(a.claimed_at)",
      "THEN a.bytes ELSE 0 END) AS measured_bytes,",
      "SUM(CASE WHEN a.bytes IS NOT NULL AND a.claimed_at IS NOT NULL",
      "AND a.finished_at IS NOT NULL AND julianday(a.finished_at) > julianday(a.claimed_at)",
      "THEN (julianday(a.finished_at) - julianday(a.claimed_at)) * 86400.0 ELSE 0 END) AS measured_seconds",
      "FROM acquisition_job_attempts a JOIN acquisition_jobs j ON j.job_id = a.job_id",
      "WHERE j.kind = 'ingest' AND a.outcome = 'succeeded' AND a.finished_at IS NOT NULL"
    )
  )
  byte_count <- suppressWarnings(as.numeric(aggregate$byte_count[[1L]]))
  measured_count <- suppressWarnings(as.numeric(aggregate$measured_count[[1L]]))
  measured_seconds <- suppressWarnings(as.numeric(aggregate$measured_seconds[[1L]]))
  measured_bytes <- suppressWarnings(as.numeric(aggregate$measured_bytes[[1L]]))
  list(
    backlog = state_count("queued") + state_count("retry_wait"),
    queued = state_count("queued"),
    retry_wait = state_count("retry_wait"),
    running = state_count("running"),
    succeeded = state_count("succeeded"),
    failed = state_count("failed") + state_count("blocked"),
    observed_completed_bytes = if (is.finite(byte_count) && byte_count > 0) {
      as.numeric(aggregate$completed_bytes[[1L]])
    } else {
      NA_real_
    },
    observed_mean_seconds = if (is.finite(measured_count) && measured_count > 0 && is.finite(measured_seconds)) {
      measured_seconds / measured_count
    } else {
      NA_real_
    },
    observed_bytes_per_second = if (is.finite(measured_count) && measured_count > 0 &&
      is.finite(measured_seconds) && measured_seconds > 0) {
      measured_bytes / measured_seconds
    } else {
      NA_real_
    }
  )
}

acquisition_dashboard_validate_page <- function(page, page_size) {
  page <- suppressWarnings(as.integer(page))
  page_size <- suppressWarnings(as.integer(page_size))
  if (length(page) != 1L || is.na(page) || page < 1L) acquisition_controls_abort("{.arg page} must be a positive integer.")
  if (length(page_size) != 1L || is.na(page_size) || page_size < 1L || page_size > 500L) {
    acquisition_controls_abort("{.arg page_size} must be between 1 and 500.")
  }
  list(page = page, page_size = page_size)
}

#' List a filtered, paginated acquisition queue projection.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param campaign_id Optional campaign filter.
#' @param country Optional source country filter.
#' @param energy Optional normalized energy-kind filter.
#' @param state Optional queue-state filter.
#' @param portal Optional source portal filter.
#' @param search Optional ID or target search string.
#' @param page One-based page number.
#' @param page_size Rows per page.
#' @return A data frame with a `total_count` attribute.
#' @export
acquisition_dashboard_jobs <- function(
  catalog,
  campaign_id = NULL,
  country = NULL,
  energy = NULL,
  state = NULL,
  portal = NULL,
  search = NULL,
  page = 1L,
  page_size = 50L
) {
  acquisition_controls_ensure_schema(catalog)
  page_values <- acquisition_dashboard_validate_page(page, page_size)
  conn <- acquisition_catalog_connection(catalog)
  clauses <- character(0)
  params <- list()
  add_filter <- function(condition, value) {
    clauses <<- c(clauses, condition)
    params <<- c(params, list(value))
  }
  if (!is.null(campaign_id)) {
    campaign_id <- acquisition_controls_scalar(campaign_id, "campaign_id")
    clauses <- c(
      clauses,
      "(j.campaign_id = ? OR EXISTS (SELECT 1 FROM acquisition_campaign_items cfilter WHERE cfilter.job_id = j.job_id AND cfilter.campaign_id = ?))"
    )
    params <- c(params, list(campaign_id, campaign_id))
  }
  if (!is.null(country)) add_filter("COALESCE(sd.country, ss.country) = ?", acquisition_controls_scalar(country, "country"))
  if (!is.null(energy)) {
    energy <- acquisition_controls_scalar(energy, "energy")
    add_filter(
      paste("EXISTS (SELECT 1 FROM acquisition_campaign_items ci WHERE ci.job_id = j.job_id AND ci.energy_kind = ?)", sep = ""),
      energy
    )
  }
  if (!is.null(state)) add_filter("j.state = ?", acquisition_controls_scalar(state, "state"))
  if (!is.null(portal)) add_filter("COALESCE(sd.portal, ss.portal) = ?", acquisition_controls_scalar(portal, "portal"))
  if (!is.null(search)) {
    search <- acquisition_controls_scalar(search, "search")
    pattern <- paste0("%", search, "%")
    clauses <- c(clauses, "(j.job_id LIKE ? OR j.target_ref_id LIKE ? OR COALESCE(d.native_attachment_id, '') LIKE ?)")
    params <- c(params, list(pattern, pattern, pattern))
  }
  where <- if (length(clauses) == 0L) "" else paste("WHERE", paste(clauses, collapse = " AND "))
  from <- paste(
    "FROM acquisition_jobs j",
    "LEFT JOIN acquisition_sources ss ON ss.source_ref_id = j.target_ref_id AND j.target_type = 'source'",
    "LEFT JOIN acquisition_documents d ON d.document_ref_id = j.target_ref_id AND j.target_type = 'document'",
    "LEFT JOIN acquisition_sources sd ON sd.source_ref_id = d.source_ref_id",
    "LEFT JOIN acquisition_job_attempts a ON a.job_id = j.job_id",
    "AND a.attempt_no = (SELECT MAX(a2.attempt_no) FROM acquisition_job_attempts a2 WHERE a2.job_id = j.job_id)"
  )
  campaign_join <- paste(
    "LEFT JOIN (SELECT job_id, CASE WHEN COUNT(DISTINCT energy_kind) > 1 THEN 'mixed'",
    "ELSE COALESCE(MAX(energy_kind), 'unknown') END AS energy_kind",
    "FROM acquisition_campaign_items GROUP BY job_id) ci ON ci.job_id = j.job_id"
  )
  count_query <- paste("SELECT COUNT(*) AS n", from, where)
  count <- if (length(params) == 0L) {
    DBI::dbGetQuery(conn, count_query)$n[[1L]]
  } else {
    DBI::dbGetQuery(conn, count_query, params = params)$n[[1L]]
  }
  offset <- (page_values$page - 1L) * page_values$page_size
  rows <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT j.*, a.outcome AS latest_attempt_outcome, a.finished_at AS latest_finished_at,",
      "a.heartbeat_at AS latest_heartbeat_at, a.bytes AS latest_bytes,",
      "COALESCE(sd.source_ref_id, ss.source_ref_id) AS source_ref_id,",
      "COALESCE(sd.country, ss.country) AS country, COALESCE(sd.portal, ss.portal) AS portal,",
      "d.native_attachment_id, COALESCE(ci.energy_kind, 'unknown') AS energy_kind",
      from,
      campaign_join,
      where,
      "ORDER BY j.priority DESC, j.created_at, j.job_id LIMIT ? OFFSET ?"
    ),
    params = c(params, list(page_values$page_size, offset))
  )
  attr(rows, "total_count") <- as.integer(count)
  attr(rows, "page") <- page_values$page
  attr(rows, "page_size") <- page_values$page_size
  rows
}

#' Alias for [acquisition_dashboard_jobs()].
#' @inheritParams acquisition_dashboard_jobs
#' @export
acquisition_dashboard_queue <- function(...) acquisition_dashboard_jobs(...)

acquisition_dashboard_request <- function(request, session) {
  if (is.function(request)) {
    return(request(session = session))
  }
  if (is.null(request)) {
    session_request <- tryCatch(session$request, error = function(error) NULL)
    return(list(session = session, request = session_request))
  }
  request
}

acquisition_dashboard_optional_input <- function(value) {
  if (is.null(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(as.character(value)))) {
    return(NULL)
  }
  as.character(value)
}

# Return only the small set of control targets shown in the dashboard.  A
# campaign table can grow independently of the queue page, so reading the
# complete campaign history to populate a select input is an avoidable poll.
acquisition_dashboard_target_choices <- function(
  catalog,
  target_type = "job",
  limit = 100L
) {
  target_type <- acquisition_controls_scalar(target_type, "target_type")
  if (!target_type %in% c("job", "campaign")) {
    acquisition_controls_abort("{.arg target_type} must be `job` or `campaign`.")
  }
  limit <- acquisition_dashboard_validate_page(1L, limit)$page_size
  if (identical(target_type, "job")) {
    rows <- acquisition_dashboard_jobs(catalog, page = 1L, page_size = limit)
    ids <- if (nrow(rows) == 0L) character(0) else as.character(rows$job_id)
  } else {
    rows <- DBI::dbGetQuery(
      acquisition_catalog_connection(catalog),
      "SELECT campaign_id FROM acquisition_campaigns ORDER BY submitted_at DESC, campaign_id DESC LIMIT ?",
      params = list(limit)
    )
    ids <- if (nrow(rows) == 0L) character(0) else as.character(rows$campaign_id)
  }
  stats::setNames(ids, ids)
}

acquisition_dashboard_ui <- function(id = NULL, preview = NULL) {
  ns <- if (is.null(id)) identity else shiny::NS(id)
  campaign_panel <- if (is.null(preview)) {
    shiny::tags$div(
      id = ns("campaign-preview-unavailable"),
      shiny::h2("Acquisition preview unavailable"),
      shiny::p("A published model and campaign policy are required before preview or submission is available.")
    )
  } else {
    acquisition_campaign_panel_ui(ns("campaign"))
  }
  shiny::tagList(
    shiny::tags$div(
      id = ns("acquisition-dashboard"),
      campaign_panel,
      shiny::h2("Acquisition dashboard"),
      shiny::textOutput(ns("dashboard-actor")),
      shiny::tags$div(
        id = ns("dashboard-summary"),
        shiny::h3("Progress"),
        shiny::tableOutput(ns("dashboard-progress")),
        shiny::h3("Projects by country and energy"),
        shiny::tableOutput(ns("dashboard-project-counts")),
        shiny::h3("Files"),
        shiny::tableOutput(ns("dashboard-file-counts")),
        shiny::h3("Queue stages"),
        shiny::tableOutput(ns("dashboard-stage-counts")),
        shiny::h3("Active workers and heartbeats"),
        shiny::tableOutput(ns("dashboard-workers")),
        shiny::h3("Unknown or error totals"),
        shiny::tableOutput(ns("dashboard-unknown"))
      ),
      shiny::tags$div(
        id = ns("dashboard-queue-panel"),
        shiny::h3("Queue"),
        shiny::tags$div(
          id = ns("dashboard-queue-filters"),
          shiny::textInput(ns("dashboard-filter-campaign"), "Campaign", value = ""),
          shiny::textInput(ns("dashboard-filter-country"), "Country", value = ""),
          shiny::textInput(ns("dashboard-filter-energy"), "Energy", value = ""),
          shiny::selectInput(
            ns("dashboard-filter-state"),
            "State",
            choices = c("All" = "", "queued", "running", "retry_wait", "blocked", "succeeded", "failed", "cancelled")
          ),
          shiny::textInput(ns("dashboard-filter-portal"), "Portal", value = ""),
          shiny::textInput(ns("dashboard-filter-search"), "Search", value = ""),
          shiny::numericInput(ns("dashboard-page"), "Queue page", value = 1L, min = 1L, step = 1L),
          shiny::numericInput(ns("dashboard-page-size"), "Rows per page", value = 50L, min = 1L, max = 500L, step = 1L),
          shiny::actionButton(ns("dashboard-page-prev"), "Previous page"),
          shiny::actionButton(ns("dashboard-page-next"), "Next page"),
          shiny::textOutput(ns("dashboard-page-info"))
        ),
        shiny::tableOutput(ns("dashboard-queue")),
        shiny::h3("Administrator controls"),
        shiny::selectInput(ns("dashboard-action"), "Action", choices = c("pause", "resume", "retry", "cancel")),
        shiny::selectInput(ns("dashboard-target-type"), "Target type", choices = c("job", "campaign")),
        shiny::selectInput(ns("dashboard-target-id"), "Target", choices = character(0)),
        shiny::textInput(ns("dashboard-reason"), "Reason"),
        shiny::actionButton(ns("dashboard-apply"), "Apply control"),
        shiny::textOutput(ns("dashboard-action-result"))
      )
    )
  )
}

#' Run the server portion of the namespaced acquisition dashboard module.
#'
#' The module only reads the injected catalog and submits authenticated control
#' requests. It never owns acquisition workers or curation snapshots.
#'
#' @param id Shiny module namespace.
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param actor_resolver A trusted authenticated actor resolver.
#' @param request Optional deployment request or function accepting `session`.
#' @param actor_probe Optional non-touching resolver used for session checks.
#' @param refresh_ms Database refresh interval in milliseconds.
#' @param page_size Queue rows shown per refresh.
#' @param active Optional function or reactive value indicating whether the
#'   Acquisition tab is active. Standalone dashboards are active by default.
#' @param target_limit Maximum number of job or campaign control targets shown.
#' @return The value returned by [shiny::moduleServer()].
#' @export
acquisition_dashboard_server <- function(
  id,
  catalog,
  actor_resolver,
  request = NULL,
  actor_probe = NULL,
  preview = NULL,
  submit_fn = acquisition_campaign_submit_authenticated,
  refresh_ms = 2000L,
  page_size = 50L,
  active = NULL,
  target_limit = 100L
) {
  acquisition_dashboard_require_ui()
  acquisition_catalog_connection(catalog)
  acquisition_controls_check_resolver(actor_resolver)
  if (!is.null(actor_probe)) acquisition_controls_check_resolver(actor_probe)
  refresh_ms <- suppressWarnings(as.integer(refresh_ms))
  if (length(refresh_ms) != 1L || is.na(refresh_ms) || refresh_ms < 250L || refresh_ms > 600000L) {
    acquisition_controls_abort("{.arg refresh_ms} must be between 250 and 600000 milliseconds.")
  }
  page_size <- acquisition_dashboard_validate_page(1L, page_size)$page_size
  target_limit <- acquisition_dashboard_validate_page(1L, target_limit)$page_size
  acquisition_controls_ensure_schema(catalog)

  shiny::moduleServer(id, function(input, output, session) {
    if (!is.null(preview)) {
      acquisition_campaign_panel_server(
        id = "campaign",
        catalog = catalog,
        preview = preview,
        actor_resolver = actor_resolver,
        request = request,
        submit_fn = submit_fn,
        active = active
      )
    }
    dashboard_is_active <- function() {
      if (is.null(active)) {
        return(TRUE)
      }
      value <- if (is.function(active)) active() else active
      isTRUE(value)
    }
    initial_actor <- curation_resolve_actor(
      actor_resolver,
      acquisition_dashboard_request(request, session)
    )
    output$`dashboard-actor` <- shiny::renderText({
      paste("Authenticated actor:", initial_actor$id, "(", paste(initial_actor$roles, collapse = ", "), ")")
    })
    summary_cache <- shiny::reactiveVal(NULL)
    summary_failed <- FALSE
    retry_tick <- 0L
    poll_error <- NULL
    summary <- shiny::reactivePoll(
      refresh_ms,
      session,
      checkFunc = function() {
        if (!dashboard_is_active()) {
          return("inactive")
        }
        token <- tryCatch(
          acquisition_dashboard_poll_token(catalog),
          error = function(error) {
            poll_error <<- error
            NULL
          }
        )
        if (is.null(token)) {
          summary_failed <<- TRUE
          retry_tick <<- retry_tick + 1L
          return(paste("error", retry_tick, sep = ":"))
        }
        poll_error <<- NULL
        if (isTRUE(summary_failed)) {
          retry_tick <<- retry_tick + 1L
          return(paste(token, retry_tick, sep = ":"))
        }
        token
      },
      valueFunc = function() {
        if (!dashboard_is_active()) {
          return(summary_cache())
        }
        value <- if (!is.null(poll_error)) {
          poll_error
        } else {
          tryCatch(acquisition_dashboard_summary(catalog), error = identity)
        }
        poll_error <<- NULL
        summary_failed <<- inherits(value, "error")
        summary_cache(value)
        value
      }
    )
    queue_page <- shiny::reactiveVal(1L)
    queue_page_size <- shiny::reactive({
      value <- suppressWarnings(as.integer(input$`dashboard-page-size`))
      if (length(value) != 1L || is.na(value) || value < 1L || value > 500L) page_size else value
    })
    shiny::observeEvent(input$`dashboard-page`,
      {
        value <- suppressWarnings(as.integer(input$`dashboard-page`))
        if (length(value) == 1L && !is.na(value) && value >= 1L) queue_page(value)
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(input$`dashboard-page-size`,
      {
        queue_page(1L)
        shiny::updateNumericInput(session, "dashboard-page", value = 1L)
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      list(
        input$`dashboard-filter-campaign`, input$`dashboard-filter-country`,
        input$`dashboard-filter-energy`, input$`dashboard-filter-state`,
        input$`dashboard-filter-portal`, input$`dashboard-filter-search`
      ),
      {
        queue_page(1L)
        shiny::updateNumericInput(session, "dashboard-page", value = 1L)
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(input$`dashboard-page-prev`,
      {
        next_page <- max(1L, queue_page() - 1L)
        queue_page(next_page)
        shiny::updateNumericInput(session, "dashboard-page", value = next_page)
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(input$`dashboard-page-next`,
      {
        rows <- queue()
        total <- as.integer(attr(rows, "total_count") %||% 0L)
        max_page <- max(1L, ceiling(total / queue_page_size()))
        next_page <- min(max_page, queue_page() + 1L)
        queue_page(next_page)
        shiny::updateNumericInput(session, "dashboard-page", value = next_page)
      },
      ignoreInit = TRUE
    )
    queue <- shiny::reactive({
      if (!dashboard_is_active()) {
        return(acquisition_dashboard_empty(c(job_id = "character")))
      }
      current_summary <- summary()
      if (inherits(current_summary, "error") || is.null(current_summary)) {
        return(acquisition_dashboard_empty(c(job_id = "character")))
      }
      summary()
      acquisition_dashboard_jobs(
        catalog,
        campaign_id = acquisition_dashboard_optional_input(input$`dashboard-filter-campaign`),
        country = acquisition_dashboard_optional_input(input$`dashboard-filter-country`),
        energy = acquisition_dashboard_optional_input(input$`dashboard-filter-energy`),
        state = acquisition_dashboard_optional_input(input$`dashboard-filter-state`),
        portal = acquisition_dashboard_optional_input(input$`dashboard-filter-portal`),
        search = acquisition_dashboard_optional_input(input$`dashboard-filter-search`),
        page = queue_page(),
        page_size = queue_page_size()
      )
    })
    output$`dashboard-progress` <- shiny::renderTable(
      {
        current_summary <- summary()
        if (is.null(current_summary)) {
          return(data.frame(status = "inactive", stringsAsFactors = FALSE))
        }
        if (inherits(current_summary, "error")) {
          return(data.frame(status = "error", message = conditionMessage(current_summary), stringsAsFactors = FALSE))
        }
        progress <- current_summary$progress
        as.data.frame(progress, stringsAsFactors = FALSE)
      },
      rownames = TRUE
    )
    output$`dashboard-project-counts` <- shiny::renderTable(
      {
        value <- summary()
        if (is.null(value) || inherits(value, "error")) {
          return(data.frame())
        }
        value$project_counts
      },
      striped = TRUE
    )
    output$`dashboard-file-counts` <- shiny::renderTable(
      {
        value <- summary()
        if (is.null(value) || inherits(value, "error")) {
          return(data.frame())
        }
        value$file_counts
      },
      striped = TRUE
    )
    output$`dashboard-stage-counts` <- shiny::renderTable(
      {
        value <- summary()
        if (is.null(value) || inherits(value, "error")) {
          return(data.frame())
        }
        value$stage_counts
      },
      striped = TRUE
    )
    output$`dashboard-workers` <- shiny::renderTable(
      {
        value <- summary()
        if (is.null(value) || inherits(value, "error")) {
          return(data.frame())
        }
        value$workers
      },
      striped = TRUE
    )
    output$`dashboard-unknown` <- shiny::renderTable(
      {
        value <- summary()
        if (is.null(value) || inherits(value, "error")) {
          return(data.frame())
        }
        totals <- value$unknown_totals
        as.data.frame(totals, stringsAsFactors = FALSE)
      },
      rownames = TRUE
    )
    output$`dashboard-queue` <- shiny::renderTable(queue(), striped = TRUE)
    output$`dashboard-page-info` <- shiny::renderText({
      rows <- queue()
      total <- as.integer(attr(rows, "total_count") %||% 0L)
      max_page <- max(1L, ceiling(total / queue_page_size()))
      paste("Page", queue_page(), "of", max_page, "(", total, "jobs)")
    })
    target_choices <- NULL
    shiny::observe({
      choices <- if (!dashboard_is_active()) {
        character(0)
      } else {
        current_summary <- summary()
        if (is.null(current_summary) || inherits(current_summary, "error")) {
          character(0)
        } else if (identical(input$`dashboard-target-type`, "job")) {
          rows <- queue()
          ids <- if (nrow(rows) == 0L) character(0) else as.character(rows$job_id)
          stats::setNames(ids, ids)
        } else {
          acquisition_dashboard_target_choices(
            catalog,
            target_type = "campaign",
            limit = target_limit
          )
        }
      }
      if (!identical(choices, target_choices)) {
        selected <- shiny::isolate(input$`dashboard-target-id`)
        if (is.null(selected) || length(selected) != 1L || !selected %in% unname(choices)) {
          selected <- if (length(choices) > 0L) unname(choices[[1L]]) else character(0)
        }
        shiny::updateSelectInput(session, "dashboard-target-id", choices = choices, selected = selected)
        target_choices <<- choices
      }
    })
    output$`dashboard-action-result` <- shiny::renderText({
      if (is.null(input$`dashboard-apply`) || input$`dashboard-apply` < 1L) {
        return("No control action submitted.")
      }
      "Choose a target and apply an administrator control."
    })
    shiny::observeEvent(input$`dashboard-apply`,
      {
        result <- tryCatch(
          acquisition_control_apply(
            catalog = catalog,
            action = input$`dashboard-action`,
            target_type = input$`dashboard-target-type`,
            target_id = input$`dashboard-target-id`,
            actor_resolver = actor_resolver,
            request = acquisition_dashboard_request(request, session),
            reason = input$`dashboard-reason`
          ),
          error = function(error) error
        )
        if (inherits(result, "error")) {
          output$`dashboard-action-result` <- shiny::renderText(conditionMessage(result))
        } else {
          output$`dashboard-action-result` <- shiny::renderText(
            paste0(result$action, " recorded as ", result$status, " by ", result$actor_id, ". Audit: ", result$audit_id)
          )
        }
      },
      ignoreInit = TRUE
    )
    if (!is.null(actor_probe)) {
      auth_timer <- shiny::reactiveTimer(60000, session)
      shiny::observeEvent(auth_timer(),
        {
          tryCatch(
            curation_resolve_actor(actor_probe, acquisition_dashboard_request(request, session)),
            error = function(error) if (is.function(session$close)) session$close()
          )
        },
        ignoreInit = TRUE
      )
    }
    invisible(NULL)
  })
}

#' Build the standalone authenticated acquisition dashboard Shiny app.
#'
#' The app owns only read projections and authenticated control requests. It
#' never starts or stops acquisition workers, so closing a browser session does
#' not affect worker continuity.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param actor_resolver A trusted authenticated actor resolver.
#' @param request Optional deployment request or function accepting `session`.
#' @param actor_probe Optional non-touching resolver used for session checks.
#' @param refresh_ms Database refresh interval in milliseconds.
#' @param page_size Queue rows shown per refresh.
#' @param target_limit Maximum number of control targets shown.
#' @return A `shiny.appobj`.
#' @export
acquisition_dashboard_app <- function(
  catalog,
  actor_resolver,
  request = NULL,
  actor_probe = NULL,
  preview = NULL,
  submit_fn = acquisition_campaign_submit_authenticated,
  refresh_ms = 2000L,
  page_size = 50L,
  target_limit = 100L
) {
  acquisition_dashboard_require_ui()
  acquisition_catalog_connection(catalog)
  acquisition_controls_check_resolver(actor_resolver)
  if (!is.null(actor_probe)) acquisition_controls_check_resolver(actor_probe)
  refresh_ms <- suppressWarnings(as.integer(refresh_ms))
  if (length(refresh_ms) != 1L || is.na(refresh_ms) || refresh_ms < 250L || refresh_ms > 600000L) {
    acquisition_controls_abort("{.arg refresh_ms} must be between 250 and 600000 milliseconds.")
  }
  page_size <- acquisition_dashboard_validate_page(1L, page_size)$page_size
  target_limit <- acquisition_dashboard_validate_page(1L, target_limit)$page_size
  acquisition_controls_ensure_schema(catalog)
  shiny::shinyApp(
    ui = shiny::fluidPage(acquisition_dashboard_ui("dashboard", preview = preview)),
    server = function(input, output, session) {
      acquisition_dashboard_server(
        id = "dashboard",
        catalog = catalog,
        actor_resolver = actor_resolver,
        request = request,
        actor_probe = actor_probe,
        preview = preview,
        submit_fn = submit_fn,
        refresh_ms = refresh_ms,
        page_size = page_size,
        target_limit = target_limit
      )
    }
  )
}

#' Run the standalone acquisition dashboard.
#'
#' @inheritParams acquisition_dashboard_app
#' @param launch.browser Passed to [shiny::runApp()].
#' @param ... Additional arguments passed to [shiny::runApp()].
#' @return Invisibly, the result of [shiny::runApp()].
#' @export
run_acquisition_dashboard <- function(
  catalog,
  actor_resolver,
  request = NULL,
  actor_probe = NULL,
  preview = NULL,
  submit_fn = acquisition_campaign_submit_authenticated,
  refresh_ms = 2000L,
  page_size = 50L,
  launch.browser = interactive(),
  ...
) {
  app <- acquisition_dashboard_app(
    catalog = catalog,
    actor_resolver = actor_resolver,
    request = request,
    actor_probe = actor_probe,
    preview = preview,
    submit_fn = submit_fn,
    refresh_ms = refresh_ms,
    page_size = page_size
  )
  shiny::runApp(app, launch.browser = launch.browser, ...)
}
