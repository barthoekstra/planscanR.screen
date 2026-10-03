# ACQ-1b: a small durable queue over the acquisition catalog.
#
# Claims and state transitions are short SQLite transactions.  Network, model,
# and file work belongs to the worker callback, after a claim has committed.

acquisition_queue_default_host_concurrency <- 2L

acquisition_queue_abort <- function(message, class = "planscanR_error_acquisition_queue") {
  acquisition_catalog_abort(message, class = class)
}

acquisition_queue_time_after <- function(seconds, now = Sys.time()) {
  format(
    as.POSIXct(now, origin = "1970-01-01", tz = "UTC") + as.numeric(seconds),
    "%Y-%m-%dT%H:%M:%OS6Z",
    tz = "UTC"
  )
}

acquisition_queue_nonnegative_seconds <- function(value, arg, upper = 604800) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < 0 ||
      value > upper
  ) {
    acquisition_queue_abort(
      paste0("{.arg ", arg, "} must be a finite number between 0 and ", upper, ".")
    )
  }
  as.numeric(value)
}

acquisition_queue_status_value <- function(value, arg = "status") {
  value <- acquisition_catalog_scalar_character(value, arg)
  allowed <- c("succeeded", "retryable", "blocked", "failed", "cancelled")
  if (!value %in% allowed) {
    acquisition_queue_abort(
      paste0("{.arg ", arg, "} must be one of: ", paste(allowed, collapse = ", "), ".")
    )
  }
  value
}

acquisition_queue_optional_value <- function(value, name, default = NULL) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]])) {
    return(default)
  }
  value[[1L]]
}

acquisition_queue_optional_text <- function(value, arg) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]])) {
    return(NA_character_)
  }
  acquisition_catalog_scalar_character(as.character(value[[1L]]), arg)
}

acquisition_queue_normalize_host <- function(value, arg = "host") {
  value <- acquisition_queue_optional_text(value, arg)
  if (is.na(value)) {
    return(NA_character_)
  }
  tolower(sub("\\.$", "", value))
}

acquisition_queue_url_host <- function(url) {
  if (is.null(url) || length(url) == 0L || is.na(url[[1L]]) || !nzchar(url[[1L]])) {
    return(NA_character_)
  }
  match <- regexec("^[[:alpha:]][[:alnum:]+.-]*://([^/:?#]+)", as.character(url[[1L]]))
  parts <- regmatches(as.character(url[[1L]]), match)[[1L]]
  if (length(parts) < 2L || !nzchar(parts[[2L]])) {
    return(NA_character_)
  }
  acquisition_queue_normalize_host(parts[[2L]])
}

acquisition_queue_target_url_connection <- function(conn, target_type, target_ref_id) {
  if (identical(target_type, "source")) {
    row <- acquisition_catalog_one(conn, "SELECT source_url AS url FROM acquisition_sources WHERE source_ref_id = ?", list(target_ref_id))
  } else if (identical(target_type, "document")) {
    row <- acquisition_catalog_one(conn, "SELECT attachment_url AS url FROM acquisition_documents WHERE document_ref_id = ?", list(target_ref_id))
  } else {
    row <- acquisition_catalog_one(
      conn,
      paste(
        "SELECT d.attachment_url AS url FROM acquisition_files f",
        "JOIN acquisition_documents d ON d.document_ref_id = f.document_ref_id",
        "WHERE f.file_ref_id = ?"
      ),
      list(target_ref_id)
    )
  }
  if (is.null(row)) NA_character_ else acquisition_queue_url_host(row$url[[1L]])
}

acquisition_queue_job_id <- function(kind, target_type, target_ref_id, idempotency_key) {
  acquisition_catalog_id("job", kind, target_type, target_ref_id, idempotency_key)
}

acquisition_queue_existing_target <- function(conn, target_type, target_ref_id) {
  table <- switch(target_type,
    source = "acquisition_sources",
    document = "acquisition_documents",
    file = "acquisition_files",
    acquisition_queue_abort("Unsupported queue target type.")
  )
  column <- switch(target_type,
    source = "source_ref_id",
    document = "document_ref_id",
    file = "file_ref_id"
  )
  !is.null(acquisition_catalog_one(
    conn,
    paste0("SELECT ", column, " FROM ", table, " WHERE ", column, " = ?"),
    list(target_ref_id)
  ))
}

acquisition_queue_insert_missing_host_limits <- function(conn, now) {
  DBI::dbExecute(
    conn,
    paste(
      "INSERT OR IGNORE INTO acquisition_host_limits (host, max_concurrency, active_count, cooldown_until, updated_at)",
      "SELECT DISTINCT host, ?, 0, NULL, ? FROM acquisition_jobs WHERE host IS NOT NULL"
    ),
    params = list(acquisition_queue_default_host_concurrency, now)
  )
  invisible(TRUE)
}

acquisition_queue_sync_host_counts <- function(conn, now) {
  hosts <- DBI::dbGetQuery(conn, "SELECT host FROM acquisition_host_limits")$host
  for (host in hosts) {
    active <- DBI::dbGetQuery(
      conn,
      paste(
        "SELECT COUNT(*) AS n FROM acquisition_jobs",
        "WHERE host = ? AND state = 'running' AND lease_expires_at > ?"
      ),
      params = list(host, now)
    )$n[[1L]]
    DBI::dbExecute(
      conn,
      "UPDATE acquisition_host_limits SET active_count = ?, updated_at = ? WHERE host = ?",
      params = list(as.integer(active), now, host)
    )
  }
  invisible(TRUE)
}

acquisition_queue_backoff_seconds <- function(attempt_count) {
  min(3600, 2^max(0, as.integer(attempt_count) - 1L))
}

acquisition_queue_recover_expired_connection <- function(conn, now) {
  expired <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT * FROM acquisition_jobs",
      "WHERE state = 'running' AND lease_expires_at IS NOT NULL AND lease_expires_at <= ?",
      "ORDER BY job_id"
    ),
    params = list(now)
  )
  if (nrow(expired) == 0L) {
    return(0L)
  }
  for (i in seq_len(nrow(expired))) {
    row <- expired[i, , drop = FALSE]
    # An expired lease is immediately eligible for recovery. Retryable handler
    # failures use exponential backoff; a dead worker should not add another
    # avoidable delay before a supervisor can restart it.
    retry_at <- now
    exhausted <- as.integer(row$attempt_count[[1L]]) >= as.integer(row$max_attempts[[1L]])
    next_state <- if (exhausted) "failed" else "retry_wait"
    DBI::dbExecute(
      conn,
      paste(
        "UPDATE acquisition_job_attempts SET outcome = 'expired', finished_at = ?,",
        "error_category = 'lease-expired', error_message = ?",
        "WHERE job_id = ? AND lease_token = ? AND outcome = 'running'"
      ),
      params = list(
        now,
        "The worker lease expired before completion.",
        as.character(row$job_id[[1L]]),
        as.character(row$lease_token[[1L]])
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "UPDATE acquisition_jobs SET state = ?, available_at = ?, lease_token = NULL,",
        "worker_id = NULL, lease_expires_at = NULL, last_error_category = ?,",
        "last_error = ?, completed_at = ?, updated_at = ? WHERE job_id = ?"
      ),
      params = list(
        next_state,
        if (exhausted) as.character(row$available_at[[1L]]) else retry_at,
        "lease-expired",
        "The worker lease expired before completion.",
        if (exhausted) now else NA_character_,
        now,
        as.character(row$job_id[[1L]])
      )
    )
  }
  length(seq_len(nrow(expired)))
}

acquisition_queue_enqueue_connection <- function(
  conn,
  kind,
  target_type,
  target_ref_id,
  host = NULL,
  priority = 0L,
  max_attempts = 3L,
  delay_seconds = 0,
  idempotency_key = NULL
) {
  kind <- acquisition_catalog_scalar_character(kind, "kind")
  target_type <- acquisition_catalog_scalar_character(target_type, "target_type")
  target_ref_id <- acquisition_catalog_scalar_character(target_ref_id, "target_ref_id")
  host <- acquisition_queue_normalize_host(host)
  priority <- acquisition_catalog_scalar_integer(priority, "priority", lower = -1000000L, upper = 1000000L)
  max_attempts <- acquisition_catalog_scalar_integer(max_attempts, "max_attempts", lower = 1L, upper = 1000L)
  delay_seconds <- acquisition_queue_nonnegative_seconds(delay_seconds, "delay_seconds")
  idempotency_key <- if (is.null(idempotency_key)) {
    paste(kind, target_type, target_ref_id, sep = ":")
  } else {
    acquisition_catalog_scalar_character(idempotency_key, "idempotency_key")
  }
  if (!acquisition_queue_existing_target(conn, target_type, target_ref_id)) {
    acquisition_queue_abort(
      paste0("The queue target does not exist in the acquisition catalog: ", target_ref_id)
    )
  }
  derived_host <- acquisition_queue_target_url_connection(conn, target_type, target_ref_id)
  if (!is.na(host) && !is.na(derived_host) && !identical(host, derived_host)) {
    acquisition_queue_abort(
      paste0("The explicit queue host `", host, "` does not match the target URL host `", derived_host, "`.")
    )
  }
  if (is.na(host)) host <- derived_host
  existing_by_key <- acquisition_catalog_one(
    conn,
    "SELECT * FROM acquisition_jobs WHERE idempotency_key = ?",
    list(idempotency_key)
  )
  if (!is.null(existing_by_key) && (
    !identical(as.character(existing_by_key$kind[[1L]]), kind) ||
      !identical(as.character(existing_by_key$target_type[[1L]]), target_type) ||
      !identical(as.character(existing_by_key$target_ref_id[[1L]]), target_ref_id)
  )) {
    acquisition_queue_abort(
      paste0("The idempotency key is already bound to another acquisition target: ", idempotency_key),
      class = "planscanR_error_acquisition_queue_conflict"
    )
  }
  existing <- acquisition_catalog_one(
    conn,
    "SELECT * FROM acquisition_jobs WHERE kind = ? AND target_type = ? AND target_ref_id = ?",
    list(kind, target_type, target_ref_id)
  )
  if (!is.null(existing)) {
    return(list(
      job_id = as.character(existing$job_id[[1L]]),
      idempotency_key = as.character(existing$idempotency_key[[1L]]),
      status = "replayed"
    ))
  }
  now <- acquisition_catalog_now()
  job_id <- acquisition_queue_job_id(kind, target_type, target_ref_id, idempotency_key)
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO acquisition_jobs (job_id, kind, target_type, target_ref_id,",
      "idempotency_key, host, priority, state, available_at, attempt_count,",
      "max_attempts, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?, ?, 'queued', ?, 0, ?, ?, ?)"
    ),
    params = list(
      job_id,
      kind,
      target_type,
      target_ref_id,
      idempotency_key,
      host,
      priority,
      acquisition_queue_time_after(delay_seconds),
      max_attempts,
      now,
      now
    )
  )
  acquisition_queue_insert_missing_host_limits(conn, now)
  list(job_id = job_id, idempotency_key = idempotency_key, status = "inserted")
}

#' Enqueue one durable acquisition job.
#'
#' Repeating the same `(kind, target_type, target_ref_id)` is idempotent.  The
#' target must already be an identity in the acquisition catalog.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param kind `discover` or `download`.
#' @param target_type `source`, `document`, or `file`.
#' @param target_ref_id Persisted source, document, or file reference ID.
#' @param host Optional shared host throttle key.
#' @param priority Integer priority; larger values claim first.
#' @param max_attempts Bounded number of worker attempts.
#' @param delay_seconds Delay before the first claim.
#' @param idempotency_key Optional caller key.  The persisted target identity
#'   remains unique even when a caller key is supplied.
#' @return A list containing `job_id` and `status`.
#' @export
acquisition_queue_enqueue <- function(
  catalog,
  kind,
  target_type,
  target_ref_id,
  host = NULL,
  priority = 0L,
  max_attempts = 3L,
  delay_seconds = 0,
  idempotency_key = NULL
) {
  kind <- acquisition_catalog_scalar_character(kind, "kind")
  target_type <- acquisition_catalog_scalar_character(target_type, "target_type")
  target_ref_id <- acquisition_catalog_scalar_character(target_ref_id, "target_ref_id")
  if (!kind %in% c("discover", "download")) {
    acquisition_queue_abort("{.arg kind} must be `discover` or `download`.")
  }
  if (!target_type %in% c("source", "document", "file")) {
    acquisition_queue_abort("{.arg target_type} must be `source`, `document`, or `file`.")
  }
  priority <- acquisition_catalog_scalar_integer(priority, "priority", lower = -1000000L, upper = 1000000L)
  max_attempts <- acquisition_catalog_scalar_integer(max_attempts, "max_attempts", lower = 1L, upper = 1000L)
  delay_seconds <- acquisition_queue_nonnegative_seconds(delay_seconds, "delay_seconds")
  acquisition_catalog_transaction(catalog, function(conn) {
    acquisition_queue_enqueue_connection(
      conn,
      kind,
      target_type,
      target_ref_id,
      host,
      priority,
      max_attempts,
      delay_seconds,
      idempotency_key
    )
  })
}

#' Set one persisted shared host concurrency and cooldown limit.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param host Non-empty host key.
#' @param max_concurrency Maximum number of active non-expired leases.
#' @param cooldown_until Optional UTC timestamp after which claims may resume.
#' @return A list with the persisted host limit.
#' @export
acquisition_queue_set_host_limit <- function(
  catalog,
  host,
  max_concurrency = acquisition_queue_default_host_concurrency,
  cooldown_until = NULL
) {
  host <- acquisition_queue_normalize_host(host)
  max_concurrency <- acquisition_catalog_scalar_integer(max_concurrency, "max_concurrency", lower = 1L, upper = 1000L)
  cooldown_until <- acquisition_queue_optional_text(cooldown_until, "cooldown_until")
  acquisition_catalog_transaction(catalog, function(conn) {
    now <- acquisition_catalog_now()
    DBI::dbExecute(
      conn,
      paste(
        "INSERT INTO acquisition_host_limits (host, max_concurrency, active_count, cooldown_until, updated_at)",
        "VALUES (?, ?, 0, ?, ?) ON CONFLICT(host) DO UPDATE SET",
        "max_concurrency = excluded.max_concurrency, cooldown_until = excluded.cooldown_until, updated_at = excluded.updated_at"
      ),
      params = list(host, max_concurrency, cooldown_until, now)
    )
    acquisition_queue_sync_host_counts(conn, now)
    list(host = host, max_concurrency = max_concurrency, cooldown_until = cooldown_until)
  })
}

#' List durable acquisition jobs.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param state Optional state filter.
#' @return A data frame of jobs ordered for inspection.
#' @export
acquisition_queue_jobs <- function(catalog, state = NULL) {
  conn <- acquisition_catalog_connection(catalog)
  sql <- "SELECT * FROM acquisition_jobs"
  params <- list()
  if (!is.null(state)) {
    state <- acquisition_catalog_scalar_character(state, "state")
    sql <- paste(sql, "WHERE state = ?")
    params <- list(state)
  }
  query <- paste(sql, "ORDER BY priority DESC, created_at, job_id")
  if (length(params) == 0L) DBI::dbGetQuery(conn, query) else DBI::dbGetQuery(conn, query, params = params)
}

#' List durable acquisition attempts.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param job_id Optional job filter.
#' @return A data frame of attempts ordered by job and attempt number.
#' @export
acquisition_queue_attempts <- function(catalog, job_id = NULL) {
  conn <- acquisition_catalog_connection(catalog)
  sql <- "SELECT * FROM acquisition_job_attempts"
  params <- list()
  if (!is.null(job_id)) {
    job_id <- acquisition_catalog_scalar_character(job_id, "job_id")
    sql <- paste(sql, "WHERE job_id = ?")
    params <- list(job_id)
  }
  query <- paste(sql, "ORDER BY job_id, attempt_no")
  if (length(params) == 0L) DBI::dbGetQuery(conn, query) else DBI::dbGetQuery(conn, query, params = params)
}

#' Recover expired leases and make them retryable or exhausted.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @return The number of expired jobs recovered.
#' @export
acquisition_queue_recover <- function(catalog) {
  acquisition_catalog_transaction(catalog, function(conn) {
    now <- acquisition_catalog_now()
    acquisition_queue_insert_missing_host_limits(conn, now)
    n <- acquisition_queue_recover_expired_connection(conn, now)
    acquisition_queue_sync_host_counts(conn, now)
    as.integer(n)
  })
}

#' Claim one available job with a unique expiring lease token.
#'
#' The transaction only selects and records state.  Worker work must happen
#' after this function returns.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param worker_id Stable worker process identity.
#' @param lease_seconds Lease duration.
#' @return A claim row with `ok`, `job_id`, `attempt_no`, `lease_token`, and
#'   job fields, or `ok = FALSE` when no claim is available.
#' @export
acquisition_queue_claim <- function(catalog, worker_id, lease_seconds = 300L) {
  worker_id <- acquisition_catalog_scalar_character(worker_id, "worker_id")
  lease_seconds <- acquisition_catalog_scalar_integer(lease_seconds, "lease_seconds", lower = 1L, upper = 604800L)
  acquisition_catalog_transaction(catalog, function(conn) {
    now <- acquisition_catalog_now()
    acquisition_queue_insert_missing_host_limits(conn, now)
    acquisition_queue_recover_expired_connection(conn, now)
    acquisition_queue_sync_host_counts(conn, now)
    candidate <- acquisition_catalog_one(
      conn,
      paste(
        "SELECT j.* FROM acquisition_jobs j LEFT JOIN acquisition_host_limits h ON h.host = j.host",
        "WHERE j.state IN ('queued', 'retry_wait') AND j.available_at <= ?",
        "AND j.attempt_count < j.max_attempts",
        "AND (j.host IS NULL OR ((h.cooldown_until IS NULL OR h.cooldown_until <= ?) AND h.active_count < h.max_concurrency))",
        "ORDER BY j.priority DESC, j.available_at, j.created_at, j.job_id LIMIT 1"
      ),
      list(now, now)
    )
    if (is.null(candidate)) {
      return(list(ok = FALSE, status = "empty"))
    }
    attempt_no <- as.integer(candidate$attempt_count[[1L]]) + 1L
    token <- paste0(
      "attempt-",
      acquisition_catalog_digest(list(Sys.time(), Sys.getpid(), worker_id, candidate$job_id[[1L]], attempt_no, runif(1)))
    )
    expires <- acquisition_queue_time_after(lease_seconds)
    job_id <- as.character(candidate$job_id[[1L]])
    DBI::dbExecute(
      conn,
      paste(
        "INSERT INTO acquisition_job_attempts (job_id, attempt_no, lease_token, worker_id, host,",
        "claimed_at, lease_expires_at, heartbeat_at, outcome) VALUES (?, ?, ?, ?, ?, ?, ?, ?, 'running')"
      ),
      params = list(
        job_id,
        attempt_no,
        token,
        worker_id,
        acquisition_queue_optional_value(candidate$host, "host", NA_character_),
        now,
        expires,
        now
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "UPDATE acquisition_jobs SET state = 'running', attempt_count = ?, lease_token = ?,",
        "worker_id = ?, lease_expires_at = ?, updated_at = ? WHERE job_id = ?"
      ),
      params = list(attempt_no, token, worker_id, expires, now, job_id)
    )
    acquisition_queue_sync_host_counts(conn, now)
    out <- as.list(candidate[1L, , drop = FALSE])
    out <- lapply(out, function(value) if (length(value) == 0L) NA else value[[1L]])
    out$ok <- TRUE
    out$status <- "claimed"
    out$attempt_no <- attempt_no
    out$lease_token <- token
    out$lease_expires_at <- expires
    out$worker_id <- worker_id
    out
  })
}

#' Heartbeat a current, unexpired queue lease.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param job_id Claimed job ID.
#' @param lease_token Current attempt token.
#' @param lease_seconds New lease duration from the heartbeat time.
#' @return A list with `ok` and `status`.
#' @export
acquisition_queue_heartbeat <- function(catalog, job_id, lease_token, lease_seconds = 300L) {
  job_id <- acquisition_catalog_scalar_character(job_id, "job_id")
  lease_token <- acquisition_catalog_scalar_character(lease_token, "lease_token")
  lease_seconds <- acquisition_catalog_scalar_integer(lease_seconds, "lease_seconds", lower = 1L, upper = 604800L)
  acquisition_catalog_transaction(catalog, function(conn) {
    now <- acquisition_catalog_now()
    expires <- acquisition_queue_time_after(lease_seconds)
    changed <- DBI::dbExecute(
      conn,
      paste(
        "UPDATE acquisition_jobs SET lease_expires_at = ?, updated_at = ?",
        "WHERE job_id = ? AND state = 'running' AND lease_token = ? AND lease_expires_at > ?"
      ),
      params = list(expires, now, job_id, lease_token, now)
    )
    if (changed != 1L) {
      return(list(ok = FALSE, status = "stale"))
    }
    DBI::dbExecute(
      conn,
      paste(
        "UPDATE acquisition_job_attempts SET lease_expires_at = ?, heartbeat_at = ?",
        "WHERE job_id = ? AND lease_token = ? AND outcome = 'running'"
      ),
      params = list(expires, now, job_id, lease_token)
    )
    list(ok = TRUE, status = "renewed", lease_expires_at = expires)
  })
}

acquisition_queue_result_fields <- function(result) {
  if (is.null(result)) {
    return(list())
  }
  if (!is.list(result)) {
    acquisition_queue_abort("{.arg result} must be a list.")
  }
  result
}

acquisition_queue_register_result_file <- function(conn, result) {
  document_ref_id <- acquisition_queue_optional_value(result$document_ref_id, "document_ref_id")
  root_id <- acquisition_queue_optional_value(result$root_id, "root_id")
  relative_path <- acquisition_queue_optional_value(result$relative_path, "relative_path")
  if (is.null(document_ref_id) || is.null(root_id) || is.null(relative_path)) {
    return(NULL)
  }
  acquisition_catalog_register_file_connection(
    conn,
    document_ref_id = as.character(document_ref_id),
    root_id = as.character(root_id),
    relative_path = as.character(relative_path),
    sha256 = acquisition_queue_optional_value(result$sha256, "sha256"),
    size_bytes = acquisition_queue_optional_value(result$size_bytes, "size_bytes"),
    verification_status = acquisition_queue_optional_value(result$verification_status, "verification_status", "verified"),
    provenance = acquisition_queue_optional_value(result$provenance, "provenance", "acquisition-worker"),
    legacy_path = acquisition_queue_optional_value(result$legacy_path, "legacy_path")
  )
}

acquisition_queue_result_has_verified_file <- function(result) {
  required <- c("document_ref_id", "root_id", "relative_path", "local_path", "size_bytes", "sha256")
  if (!is.list(result) || !all(required %in% names(result))) {
    return(FALSE)
  }
  verification_status <- acquisition_queue_optional_value(result$verification_status, "verification_status")
  if (!identical(as.character(verification_status), "verified")) {
    return(FALSE)
  }
  all(vapply(required, function(name) {
    value <- result[[name]]
    length(value) == 1L && !is.na(value) && nzchar(as.character(value))
  }, logical(1)))
}

acquisition_queue_register_result_documents <- function(conn, job, result) {
  documents <- result$documents
  if (is.null(documents) || length(documents) == 0L) {
    return(character(0))
  }
  if (!is.list(documents)) {
    acquisition_queue_abort("The discovery handler's `documents` result must be a list.")
  }
  source_ref_id <- acquisition_queue_optional_value(result$source_ref_id, "source_ref_id")
  if (is.null(source_ref_id) && identical(as.character(job$target_type), "source")) {
    source_ref_id <- as.character(job$target_ref_id)
  }
  if (is.null(source_ref_id) && identical(as.character(job$target_type), "document")) {
    source_row <- acquisition_catalog_one(
      conn,
      "SELECT source_ref_id FROM acquisition_documents WHERE document_ref_id = ?",
      list(as.character(job$target_ref_id))
    )
    if (!is.null(source_row)) source_ref_id <- as.character(source_row$source_ref_id[[1L]])
  }
  if (is.null(source_ref_id)) {
    acquisition_queue_abort("Discovery results need a persisted source reference.")
  }
  ids <- character(0)
  for (i in seq_along(documents)) {
    document <- documents[[i]]
    if (!is.list(document)) acquisition_queue_abort("Each discovered document must be a list.")
    registered <- acquisition_catalog_register_document_connection(
      conn,
      source_ref_id = source_ref_id,
      native_attachment_id = acquisition_queue_optional_value(document$native_attachment_id, "native_attachment_id"),
      attachment_key = acquisition_queue_optional_value(document$attachment_key, "attachment_key"),
      attachment_url = acquisition_queue_optional_value(document$attachment_url, "attachment_url"),
      section = acquisition_queue_optional_value(document$section, "section"),
      ordinal = acquisition_queue_optional_value(document$ordinal, "ordinal", i),
      identity_status = acquisition_queue_optional_value(document$identity_status, "identity_status"),
      legacy_local_path = acquisition_queue_optional_value(document$legacy_local_path, "legacy_local_path"),
      metadata = acquisition_queue_optional_value(document$metadata, "metadata")
    )
    ids <- c(ids, registered$document_ref_id)
  }
  unique(ids)
}

#' Complete a current queue attempt, fenced by token and lease expiry.
#'
#' A stale worker receives `ok = FALSE` and cannot change job state or file
#' claims.  Optional file fields in `result` are registered in the same short
#' transaction only after the worker's network/file work has finished.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param job_id Claimed job ID.
#' @param lease_token Current attempt token.
#' @param status Terminal or retry status.
#' @param result Optional handler result list.
#' @param error_category Optional durable error category.
#' @param error_message Optional durable error message.
#' @param retry_after_seconds Optional retry delay, including a server
#'   Retry-After value.
#' @param cooldown_seconds Optional shared host cooldown.
#' @param outcome Optional alias for `status`.
#' @return A list with `ok`, `status`, and the resulting job state.
#' @export
acquisition_queue_complete <- function(
  catalog,
  job_id,
  lease_token,
  status = "succeeded",
  result = NULL,
  error_category = NULL,
  error_message = NULL,
  retry_after_seconds = NULL,
  cooldown_seconds = NULL,
  outcome = NULL
) {
  job_id <- acquisition_catalog_scalar_character(job_id, "job_id")
  lease_token <- acquisition_catalog_scalar_character(lease_token, "lease_token")
  if (!is.null(outcome)) status <- outcome
  status <- acquisition_queue_status_value(status)
  result <- acquisition_queue_result_fields(result)
  if (is.null(error_category)) error_category <- result$error_category
  if (is.null(error_message)) error_message <- result$error_message
  if (is.null(retry_after_seconds)) retry_after_seconds <- result$retry_after_seconds
  if (is.null(cooldown_seconds)) cooldown_seconds <- result$cooldown_seconds
  error_category <- acquisition_queue_optional_text(error_category, "error_category")
  error_message <- acquisition_queue_optional_text(error_message, "error_message")
  retry_after_seconds <- if (is.null(retry_after_seconds)) NULL else acquisition_queue_nonnegative_seconds(retry_after_seconds, "retry_after_seconds")
  cooldown_seconds <- if (is.null(cooldown_seconds)) NULL else acquisition_queue_nonnegative_seconds(cooldown_seconds, "cooldown_seconds")

  acquisition_catalog_transaction(catalog, function(conn) {
    now <- acquisition_catalog_now()
    job <- acquisition_catalog_one(
      conn,
      paste(
        "SELECT * FROM acquisition_jobs WHERE job_id = ? AND state = 'running'",
        "AND lease_token = ? AND lease_expires_at > ?"
      ),
      list(job_id, lease_token, now)
    )
    if (is.null(job)) {
      return(list(ok = FALSE, status = "stale", job_id = job_id))
    }
    registered_document_ids <- character(0)
    if (identical(status, "succeeded") && identical(as.character(job$kind[[1L]]), "discover")) {
      registered_document_ids <- acquisition_queue_register_result_documents(conn, job, result)
    }
    # A publication can succeed while the locked sidecar merge is retryable or
    # blocked. Keep that verified file claim durable so the next attempt can
    # reuse its receipt instead of fetching the bytes again. Unverified legacy
    # claims do not satisfy acquisition_queue_result_has_verified_file().
    if (identical(status, "succeeded") || acquisition_queue_result_has_verified_file(result)) {
      acquisition_queue_register_result_file(conn, result)
    }
    download_jobs <- list()
    if (length(registered_document_ids) > 0L) {
      download_jobs <- lapply(registered_document_ids, function(document_ref_id) {
        campaign_item <- NULL
        if (
          !is.na(job$campaign_id[[1L]]) &&
            nzchar(as.character(job$campaign_id[[1L]])) &&
            !is.na(job$campaign_item_key[[1L]]) &&
            nzchar(as.character(job$campaign_item_key[[1L]]))
        ) {
          campaign_item <- acquisition_catalog_one(
            conn,
            paste(
              "SELECT priority, priority_rule_id, priority_explanation",
              "FROM acquisition_campaign_items WHERE campaign_id = ? AND item_key = ?"
            ),
            params = list(
              as.character(job$campaign_id[[1L]]),
              as.character(job$campaign_item_key[[1L]])
            )
          )
        }
        download_priority <- if (is.null(campaign_item)) {
          as.integer(job$priority[[1L]])
        } else {
          as.integer(campaign_item$priority[[1L]])
        }
        download_rule_id <- if (is.null(campaign_item)) {
          as.character(job$priority_rule_id[[1L]])
        } else {
          as.character(campaign_item$priority_rule_id[[1L]])
        }
        download_explanation <- if (is.null(campaign_item)) {
          as.character(job$priority_explanation[[1L]])
        } else {
          as.character(campaign_item$priority_explanation[[1L]])
        }
        # The connection helper keeps document registration and its follow-on
        # download job in this same fenced transaction.  Host is intentionally
        # derived from each attachment URL, since a CDN may differ from the
        # discovery portal host.
        queued <- acquisition_queue_enqueue_connection(
          conn,
          kind = "download",
          target_type = "document",
          target_ref_id = document_ref_id,
          priority = download_priority
        )
        if (
          !is.na(job$campaign_id[[1L]]) &&
            nzchar(as.character(job$campaign_id[[1L]])) &&
            !is.null(queued$job_id)
        ) {
          # Discovery and download priorities use the same campaign evidence.
          # Only an unassociated job is claimed for this campaign; a durable
          # job already owned by another campaign remains shared work.
          existing_download <- acquisition_catalog_one(
            conn,
            "SELECT campaign_id FROM acquisition_jobs WHERE job_id = ?",
            list(queued$job_id)
          )
          if (
            !is.null(existing_download) &&
              (is.na(existing_download$campaign_id[[1L]]) || !nzchar(as.character(existing_download$campaign_id[[1L]])))
          ) {
            DBI::dbExecute(
              conn,
              paste(
                "UPDATE acquisition_jobs SET campaign_id = ?, campaign_item_key = ?, priority = ?,",
                "priority_rule_id = ?, priority_explanation = ?, updated_at = ?",
                "WHERE job_id = ? AND state IN ('queued', 'retry_wait')"
              ),
              params = list(
                as.character(job$campaign_id[[1L]]),
                as.character(job$campaign_item_key[[1L]]),
                download_priority,
                download_rule_id,
                download_explanation,
                acquisition_catalog_now(),
                queued$job_id
              )
            )
          }
        }
        queued
      })
    }
    attempt_no <- as.integer(job$attempt_count[[1L]])
    retry_at <- NA_character_
    next_state <- status
    completed_at <- now
    if (identical(status, "retryable")) {
      exhausted <- attempt_no >= as.integer(job$max_attempts[[1L]])
      if (isTRUE(exhausted)) {
        next_state <- "failed"
      } else {
        delay <- retry_after_seconds %||% acquisition_queue_backoff_seconds(attempt_no)
        retry_at <- acquisition_queue_time_after(delay)
        next_state <- "retry_wait"
        completed_at <- NA_character_
      }
    }
    DBI::dbExecute(
      conn,
      paste(
        "UPDATE acquisition_job_attempts SET outcome = ?, finished_at = ?,",
        "error_category = ?, error_message = ?, retry_at = ?, bytes = ?, sha256 = ?, local_path = ?, result_blob = ?",
        "WHERE job_id = ? AND lease_token = ? AND outcome = 'running'"
      ),
      params = list(
        status,
        now,
        error_category,
        error_message,
        if (is.na(retry_at)) NA_character_ else retry_at,
        acquisition_queue_optional_value(result$size_bytes, "size_bytes", NA_real_),
        acquisition_queue_optional_value(result$sha256, "sha256", NA_character_),
        acquisition_queue_optional_value(result$local_path, "local_path", NA_character_),
        acquisition_catalog_blob(result),
        job_id,
        lease_token
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "UPDATE acquisition_jobs SET state = ?, available_at = COALESCE(?, available_at),",
        "lease_token = NULL, worker_id = NULL, lease_expires_at = NULL,",
        "last_error_category = ?, last_error = ?, completed_at = ?, updated_at = ? WHERE job_id = ?"
      ),
      params = list(
        next_state,
        if (is.na(retry_at)) NA_character_ else retry_at,
        error_category,
        error_message,
        completed_at,
        now,
        job_id
      )
    )
    if (!is.null(cooldown_seconds) && !is.na(job$host[[1L]])) {
      cooldown_until <- acquisition_queue_time_after(cooldown_seconds)
      DBI::dbExecute(
        conn,
        paste(
          "UPDATE acquisition_host_limits SET cooldown_until = CASE",
          "WHEN cooldown_until IS NULL OR cooldown_until < ? THEN ? ELSE cooldown_until END,",
          "updated_at = ? WHERE host = ?"
        ),
        params = list(cooldown_until, cooldown_until, now, as.character(job$host[[1L]]))
      )
    }
    acquisition_queue_sync_host_counts(conn, now)
    list(
      ok = TRUE,
      status = status,
      job_state = next_state,
      job_id = job_id,
      attempt_no = attempt_no,
      document_ref_ids = registered_document_ids,
      download_jobs = download_jobs
    )
  })
}

acquisition_queue_normalise_handler_result <- function(value) {
  if (is.null(value)) {
    return(list(status = "failed", error_category = "empty-handler-result", error_message = "The worker handler returned NULL."))
  }
  if (!is.list(value)) {
    return(list(status = "failed", error_category = "bad-handler-result", error_message = "The worker handler must return a list."))
  }
  status <- value$status
  if (is.null(status) || length(status) == 0L || is.na(status[[1L]])) {
    value$status <- "failed"
    value$error_category <- value$error_category %||% "missing-handler-status"
    value$error_message <- value$error_message %||% "The worker handler did not return a status."
    return(value)
  }
  status <- as.character(status[[1L]])
  if (status %in% c("downloaded", "exists", "empty", "successful-empty", "ok")) status <- "succeeded"
  if (!status %in% c("succeeded", "retryable", "blocked", "cancelled")) status <- "failed"
  value$status <- status
  value
}

acquisition_queue_call_handler <- function(handler, job, catalog) {
  n_formals <- length(formals(handler))
  if (isTRUE(n_formals <= 1L)) handler(job) else handler(job, catalog)
}

acquisition_queue_heartbeat_preflight <- function() {
  if (!identical(.Platform$OS.type, "unix") || !dir.exists("/proc")) {
    acquisition_queue_abort("Acquisition workers require a Linux-style /proc filesystem for lease-heartbeat supervision.")
  }
  if (requireNamespace("reticulate", quietly = TRUE) && isTRUE(reticulate::py_available(initialize = FALSE))) {
    acquisition_queue_abort("Start acquisition workers in a fresh R process before Python has been initialized.")
  }
  if (
    requireNamespace("shiny", quietly = TRUE) &&
      exists("isRunning", asNamespace("shiny"), inherits = FALSE) &&
      isTRUE(shiny::isRunning())
  ) {
    acquisition_queue_abort("Acquisition workers must run outside an active Shiny process.")
  }
  invisible(TRUE)
}

acquisition_queue_start_heartbeat_guard <- function(catalog, claim, lease_seconds) {
  if (!identical(.Platform$OS.type, "unix") || !requireNamespace("parallel", quietly = TRUE) || !dir.exists("/proc")) {
    return(list(ok = FALSE, error_message = "Automatic lease heartbeats require a Linux worker process."))
  }
  stop_file <- tempfile("planscanR-acquisition-heartbeat-stop-")
  ready_file <- tempfile("planscanR-acquisition-heartbeat-ready-")
  lost_file <- tempfile("planscanR-acquisition-heartbeat-lost-")
  done_file <- tempfile("planscanR-acquisition-heartbeat-done-")
  parent_pid <- Sys.getpid()
  catalog_path <- catalog$path
  job_id <- claim$job_id
  lease_token <- claim$lease_token
  interval <- min(30, max(0.05, as.numeric(lease_seconds) / 3))
  handle <- parallel::mcparallel(
    {
      parent_alive <- function() {
        status_path <- file.path("/proc", as.character(parent_pid), "status")
        if (!file.exists(status_path)) {
          return(FALSE)
        }
        lines <- tryCatch(readLines(status_path, warn = FALSE), error = function(error) character())
        state <- lines[startsWith(lines, "State:")][1L]
        length(state) == 1L && nzchar(state) && !grepl("Z", state, fixed = TRUE)
      }
      child_catalog <- tryCatch(
        acquisition_catalog_open(catalog_path),
        error = function(error) NULL
      )
      if (is.null(child_catalog)) {
        writeLines("open-failed", lost_file)
        writeLines("done", done_file)
      } else {
        renewed <- tryCatch(
          acquisition_queue_heartbeat(
            child_catalog,
            job_id,
            lease_token,
            lease_seconds = lease_seconds
          ),
          error = function(error) list(ok = FALSE)
        )
        if (!isTRUE(renewed$ok)) {
          writeLines("heartbeat-failed", lost_file)
        } else {
          writeLines("ready", ready_file)
          next_heartbeat <- Sys.time() + interval
          repeat {
            if (file.exists(stop_file) || !parent_alive()) break
            wait <- max(0, as.numeric(next_heartbeat - Sys.time()))
            Sys.sleep(min(0.25, wait))
            if (file.exists(stop_file) || !parent_alive()) break
            if (Sys.time() >= next_heartbeat) {
              renewed <- tryCatch(
                acquisition_queue_heartbeat(
                  child_catalog,
                  job_id,
                  lease_token,
                  lease_seconds = lease_seconds
                ),
                error = function(error) list(ok = FALSE)
              )
              if (!isTRUE(renewed$ok)) {
                writeLines("heartbeat-failed", lost_file)
                break
              }
              next_heartbeat <- Sys.time() + interval
            }
          }
        }
        child_catalog$close()
        writeLines("done", done_file)
      }
      invisible(NULL)
    },
    silent = TRUE,
    detached = TRUE
  )
  deadline <- Sys.time() + 2
  while (!file.exists(ready_file) && !file.exists(lost_file) && !file.exists(done_file) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  if (!file.exists(ready_file) || file.exists(lost_file)) {
    invisible(file.create(stop_file))
    deadline <- Sys.time() + 2
    while (!file.exists(done_file) && Sys.time() < deadline) Sys.sleep(0.05)
    unlink(c(stop_file, ready_file, lost_file, done_file), force = TRUE)
    return(list(ok = FALSE, error_message = "The lease heartbeat process did not become ready."))
  }
  list(
    ok = TRUE,
    handle = handle,
    stop_file = stop_file,
    ready_file = ready_file,
    lost_file = lost_file,
    done_file = done_file
  )
}

acquisition_queue_stop_heartbeat_guard <- function(guard) {
  if (is.null(guard) || !isTRUE(guard$ok)) {
    return(TRUE)
  }
  invisible(file.create(guard$stop_file))
  deadline <- Sys.time() + 2
  while (!file.exists(guard$done_file) && Sys.time() < deadline) Sys.sleep(0.05)
  lost <- file.exists(guard$lost_file)
  unlink(c(guard$stop_file, guard$ready_file, guard$lost_file, guard$done_file), force = TRUE)
  !lost
}

acquisition_queue_relative_path <- function(path, root) {
  path <- normalizePath(path, mustWork = FALSE)
  root <- normalizePath(root, mustWork = FALSE)
  prefix <- paste0(root, .Platform$file.sep)
  if (!startsWith(path, prefix)) {
    return(NA_character_)
  }
  substring(path, nchar(prefix) + 1L)
}

acquisition_queue_cache_document_id <- function(source_ref_id, document_ref_id) {
  paste(as.character(source_ref_id), as.character(document_ref_id), sep = "--")
}

acquisition_queue_unblob <- function(value) {
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

acquisition_queue_metadata_field <- function(metadata, name) {
  if (is.null(metadata) || !is.list(metadata) || is.null(metadata[[name]])) {
    return(NULL)
  }
  value <- metadata[[name]]
  if (is.list(value)) value <- value[[1L]]
  if (length(value) == 0L || is.na(value[[1L]])) {
    return(NULL)
  }
  as.character(value[[1L]])
}

acquisition_queue_document_row <- function(conn, job) {
  if (identical(as.character(job$target_type), "document")) {
    return(acquisition_catalog_one(
      conn,
      paste(
        "SELECT d.document_ref_id, d.source_ref_id, d.attachment_url,",
        "d.legacy_local_path, s.country, s.portal, s.native_project_id,",
        "s.legacy_sidecar_path",
        "FROM acquisition_documents d JOIN acquisition_sources s ON s.source_ref_id = d.source_ref_id",
        "WHERE d.document_ref_id = ?"
      ),
      list(as.character(job$target_ref_id))
    ))
  }
  if (identical(as.character(job$target_type), "file")) {
    return(acquisition_catalog_one(
      conn,
      paste(
        "SELECT d.document_ref_id, d.source_ref_id, d.attachment_url,",
        "d.legacy_local_path, s.country, s.portal, s.native_project_id,",
        "s.legacy_sidecar_path",
        "FROM acquisition_files f JOIN acquisition_documents d ON d.document_ref_id = f.document_ref_id",
        "JOIN acquisition_sources s ON s.source_ref_id = d.source_ref_id",
        "WHERE f.file_ref_id = ?"
      ),
      list(as.character(job$target_ref_id))
    ))
  }
  NULL
}

acquisition_queue_sidecar_row_value <- function(row, name, default = NA_character_) {
  if (!name %in% names(row)) {
    return(default)
  }
  value <- row[[name]]
  if (length(value) == 0L || is.na(value[[1L]])) {
    return(default)
  }
  as.character(value[[1L]])
}

acquisition_queue_sidecar_target <- function(row, cache_root) {
  country <- acquisition_queue_sidecar_row_value(row, "country")
  document_id <- acquisition_queue_sidecar_row_value(row, "native_project_id")
  legacy_path <- acquisition_queue_sidecar_row_value(row, "legacy_sidecar_path")
  explicit_path <- !is.na(legacy_path) && nzchar(legacy_path)
  path <- if (explicit_path) {
    normalizePath(legacy_path, mustWork = FALSE)
  } else {
    file.path(cache_root, "files", country, document_id, paste0(document_id, ".meta.json"))
  }
  if (!file.exists(path)) {
    return(list(
      status = if (explicit_path) "blocked" else "skipped",
      error_category = if (explicit_path) "missing-sidecar" else "sidecar-missing",
      error_message = if (explicit_path) {
        "The persisted legacy sidecar path does not exist."
      } else {
        "No source sidecar exists to synchronize."
      },
      path = path
    ))
  }
  expected_name <- paste0(document_id, ".meta.json")
  if (!identical(basename(path), expected_name) || !identical(basename(dirname(path)), document_id)) {
    return(list(
      status = "blocked",
      error_category = "sidecar-path-conflict",
      error_message = "The sidecar path does not match the persisted native project identity.",
      path = path
    ))
  }
  country_dir <- basename(dirname(dirname(path)))
  if (!identical(country_dir, country)) {
    return(list(
      status = "blocked",
      error_category = "sidecar-path-conflict",
      error_message = "The sidecar path does not match the persisted country identity.",
      path = path
    ))
  }
  root <- dirname(dirname(dirname(dirname(path))))
  list(status = "ready", path = path, root = root, country = country, document_id = document_id)
}

acquisition_queue_sidecar_download_update <- function(current, row, result) {
  if (!is.data.frame(current) || nrow(current) != 1L) {
    acquisition_queue_abort(
      "The source sidecar did not contain exactly one record.",
      class = "planscanR_error_acquisition_sidecar_identity"
    )
  }
  expected <- list(
    country = acquisition_queue_sidecar_row_value(row, "country"),
    source_portal = acquisition_queue_sidecar_row_value(row, "portal"),
    document_id = acquisition_queue_sidecar_row_value(row, "native_project_id")
  )
  actual <- list(
    country = as.character(current$country[[1L]]),
    source_portal = as.character(current$source_portal[[1L]]),
    document_id = as.character(current$document_id[[1L]])
  )
  if (!identical(actual, expected)) {
    acquisition_queue_abort(
      "The source sidecar identity does not match the acquisition source; refusing to write it.",
      class = "planscanR_error_acquisition_sidecar_identity"
    )
  }
  url <- acquisition_queue_optional_value(result$url, "url")
  if (!identical(as.character(url), as.character(row$attachment_url[[1L]]))) {
    acquisition_queue_abort("The publication URL does not match the catalog attachment.",
      class = "planscanR_error_acquisition_sidecar_attachment"
    )
  }
  status <- current[["download_status"]]
  status <- if (is.list(status) && length(status) >= 1L) status[[1L]] else NULL
  if (!is.data.frame(status)) status <- tibble::tibble(url = character())
  matches <- if ("url" %in% names(status)) which(!is.na(status$url) & as.character(status$url) == url) else integer()
  if (length(matches) > 1L) {
    acquisition_queue_abort("The attachment URL is ambiguous in the source sidecar.",
      class = "planscanR_error_acquisition_sidecar_attachment"
    )
  }
  outcome <- tibble::tibble(
    url = as.character(url), local_path = as.character(result$local_path),
    status = as.character(result$download_status %||% "downloaded"),
    size_bytes = as.numeric(result$size_bytes), sha256 = as.character(result$sha256),
    reason = NA_character_
  )
  if (length(matches) == 0L) {
    status <- dplyr::bind_rows(status, outcome)
  } else {
    for (name in names(outcome)) {
      if (!name %in% names(status)) status[[name]] <- outcome[[name]][rep(NA_integer_, nrow(status))]
      status[[name]][matches[[1L]]] <- outcome[[name]][[1L]]
    }
  }
  current$download_status <- list(status)
  current
}

acquisition_queue_sync_sidecar_row <- function(row, result, cache_root) {
  if (!requireNamespace("planscanR", quietly = TRUE) ||
    !exists("reconcile_cached_download", asNamespace("planscanR"), inherits = FALSE) ||
    !exists("update_record_sidecar", asNamespace("planscanR"), inherits = FALSE)) {
    return(list(
      status = "blocked",
      error_category = "missing-sidecar-api",
      error_message = "The installed planscanR package lacks the receipt or sidecar update API."
    ))
  }
  if (!acquisition_queue_result_has_verified_file(result)) {
    return(list(
      status = "blocked",
      error_category = "unverified-publication",
      error_message = "A sidecar can only be synchronized from a verified publication result."
    ))
  }
  receipt <- tryCatch(
    planscanR::reconcile_cached_download(
      result$local_path,
      expected_size_bytes = result$size_bytes,
      expected_sha256 = result$sha256,
      url = result$url,
      repair = FALSE
    ),
    error = function(error) error
  )
  if (inherits(receipt, "error")) {
    return(list(
      status = "retryable",
      error_category = "receipt-check",
      error_message = conditionMessage(receipt)
    ))
  }
  receipt_status <- as.character(receipt$status[[1L]])
  if (!identical(receipt_status, "verified")) {
    return(list(
      status = "blocked",
      error_category = if (identical(receipt_status, "partial")) "partial-publication" else "unverified-publication",
      error_message = as.character(receipt$reason[[1L]] %||% receipt_status),
      receipt_status = receipt_status
    ))
  }
  receipt_size <- suppressWarnings(as.numeric(receipt$size_bytes[[1L]]))
  receipt_sha256 <- as.character(receipt$sha256[[1L]])
  if (
    is.na(receipt_size) ||
      receipt_size != as.numeric(result$size_bytes) ||
      is.na(receipt_sha256) ||
      !identical(tolower(receipt_sha256), tolower(as.character(result$sha256)))
  ) {
    return(list(
      status = "blocked",
      error_category = "receipt-metadata-mismatch",
      error_message = "The receipt metadata does not match the catalog publication result.",
      receipt_status = receipt_status
    ))
  }
  target <- acquisition_queue_sidecar_target(row, cache_root)
  if (!identical(target$status, "ready")) {
    if (identical(target$status, "skipped")) {
      return(list(
        status = "succeeded",
        sync_status = "skipped",
        sync_reason = target$error_message,
        receipt_status = receipt_status
      ))
    }
    return(target)
  }
  updated <- tryCatch(
    {
      planscanR::update_record_sidecar(
        target$country,
        target$document_id,
        function(current) acquisition_queue_sidecar_download_update(current, row, result),
        root = target$root
      )
      TRUE
    },
    error = function(error) error
  )
  if (inherits(updated, "error")) {
    classes <- class(updated)
    blocked <- any(grepl("sidecar_(identity|attachment|missing|corrupt)|acquisition_sidecar_(identity|attachment)", classes))
    return(list(
      status = if (blocked) "blocked" else "retryable",
      error_category = if (blocked) "sidecar-identity-conflict" else "sidecar-sync",
      error_message = conditionMessage(updated)
    ))
  }
  list(status = "succeeded", sync_status = "succeeded", receipt_status = receipt_status, sidecar_path = target$path)
}

#' Synchronize one verified catalog download into its source sidecar.
#'
#' The receipt is checked before the locked sidecar callback runs. The callback
#' validates country, portal, native project ID, and attachment URL against the
#' latest sidecar row, so a same-country portal collision is blocked without a
#' write. Legacy claims remain unverified and cannot enter this path.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param document_ref_id Registered document reference ID.
#' @param result Verified publication result from a queue worker.
#' @param cache_root Cache root containing the published attachment.
#' @return A list with `status` and synchronization details.
#' @export
acquisition_queue_sync_sidecar <- function(catalog, document_ref_id, result, cache_root = NULL) {
  document_ref_id <- acquisition_catalog_scalar_character(document_ref_id, "document_ref_id")
  cache_root <- cache_root %||% planscanR::cache_dir_default()
  cache_root <- acquisition_catalog_scalar_character(cache_root, "cache_root")
  row <- acquisition_catalog_one(
    acquisition_catalog_connection(catalog),
    paste(
      "SELECT d.document_ref_id, d.source_ref_id, d.attachment_url,",
      "d.legacy_local_path, s.country, s.portal, s.native_project_id,",
      "s.legacy_sidecar_path",
      "FROM acquisition_documents d JOIN acquisition_sources s ON s.source_ref_id = d.source_ref_id",
      "WHERE d.document_ref_id = ?"
    ),
    list(document_ref_id)
  )
  if (is.null(row)) {
    return(list(status = "blocked", error_category = "missing-document", error_message = "The document reference was not found."))
  }
  result$document_ref_id <- document_ref_id
  acquisition_queue_sync_sidecar_row(row, result, cache_root)
}

#' Reconcile a verified catalog file and synchronize its source sidecar.
#'
#' Only `planscan-cache` locations are resolved by this compact service. A
#' legacy or unverified claim is reported without promoting its bytes.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param file_ref_id Registered file reference ID.
#' @param cache_root Cache root containing the published attachment.
#' @return A list with `status` and reconciliation details.
#' @export
acquisition_queue_reconcile_file <- function(catalog, file_ref_id, cache_root = NULL) {
  file_ref_id <- acquisition_catalog_scalar_character(file_ref_id, "file_ref_id")
  cache_root <- cache_root %||% planscanR::cache_dir_default()
  cache_root <- acquisition_catalog_scalar_character(cache_root, "cache_root")
  row <- acquisition_catalog_one(
    acquisition_catalog_connection(catalog),
    paste(
      "SELECT f.file_ref_id, f.document_ref_id, f.root_id, f.relative_path,",
      "f.sha256, f.size_bytes, f.verification_status, f.provenance,",
      "d.source_ref_id, d.attachment_url, d.legacy_local_path,",
      "s.country, s.portal, s.native_project_id, s.legacy_sidecar_path",
      "FROM acquisition_files f JOIN acquisition_documents d ON d.document_ref_id = f.document_ref_id",
      "JOIN acquisition_sources s ON s.source_ref_id = d.source_ref_id",
      "WHERE f.file_ref_id = ?"
    ),
    list(file_ref_id)
  )
  if (is.null(row)) {
    return(list(status = "blocked", error_category = "missing-file", error_message = "The file reference was not found."))
  }
  if (!identical(as.character(row$verification_status[[1L]]), "verified")) {
    return(list(status = "blocked", error_category = "unverified-file-claim", error_message = "Legacy or unverified file claims cannot repair receipts."))
  }
  if (!identical(as.character(row$root_id[[1L]]), "planscan-cache")) {
    return(list(status = "blocked", error_category = "unknown-storage-root", error_message = "The file storage root is not mapped to the active cache."))
  }
  local_path <- file.path(cache_root, as.character(row$relative_path[[1L]]))
  result <- list(
    status = "succeeded",
    document_ref_id = as.character(row$document_ref_id[[1L]]),
    root_id = as.character(row$root_id[[1L]]),
    relative_path = as.character(row$relative_path[[1L]]),
    local_path = local_path,
    size_bytes = as.numeric(row$size_bytes[[1L]]),
    sha256 = as.character(row$sha256[[1L]]),
    verification_status = "verified",
    provenance = as.character(row$provenance[[1L]]),
    url = as.character(row$attachment_url[[1L]]),
    download_status = "downloaded"
  )
  out <- acquisition_queue_sync_sidecar_row(row, result, cache_root)
  out$file_ref_id <- file_ref_id
  out
}

#' Recover an exhausted download job from its existing publication receipt.
#'
#' This explicit local operation never downloads or resets retry limits. It
#' appends a receipt-only audited attempt and uses normal lease fencing. Active
#' leases are refused; only failed or blocked download jobs may be reconciled.
#' Catalog-only sources with no implicit sidecar report synchronization skipped.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param job_id Failed or blocked download job ID.
#' @param cache_root Root containing the canonical published attachment.
#' @return A list with `ok`, `status`, and recovery details.
#' @export
acquisition_queue_reconcile_job <- function(catalog, job_id, cache_root = NULL) {
  job_id <- acquisition_catalog_scalar_character(job_id, "job_id")
  cache_root <- cache_root %||% planscanR::cache_dir_default()
  conn <- acquisition_catalog_connection(catalog)
  job <- acquisition_catalog_one(conn, "SELECT * FROM acquisition_jobs WHERE job_id = ?", list(job_id))
  if (is.null(job) || !identical(job$kind[[1L]], "download")) {
    return(list(ok = FALSE, status = "blocked", error_category = "not-download-job"))
  }
  row <- acquisition_queue_document_row(conn, as.list(job))
  if (is.null(row) || is.na(row$attachment_url[[1L]])) {
    return(list(ok = FALSE, status = "blocked", error_category = "missing-attachment-url"))
  }
  path <- planscanR::cache_path(row$attachment_url[[1L]], row$country[[1L]],
    acquisition_queue_cache_document_id(row$source_ref_id[[1L]], row$document_ref_id[[1L]]),
    cache_dir = cache_root
  )
  if (!file.exists(path)) path <- planscanR::resolve_cached_path(path)
  if (is.na(path) || !file.exists(path)) {
    return(list(ok = FALSE, status = "blocked", error_category = "missing-publication"))
  }
  receipt <- planscanR::reconcile_cached_download(path, url = row$attachment_url[[1L]], repair = FALSE)
  if (!identical(receipt$status[[1L]], "verified")) {
    return(list(ok = FALSE, status = "blocked", error_category = "unverified-publication"))
  }
  publication <- list(
    status = "succeeded", document_ref_id = row$document_ref_id[[1L]],
    root_id = "planscan-cache", relative_path = acquisition_queue_relative_path(path, cache_root),
    local_path = path, size_bytes = receipt$size_bytes[[1L]], sha256 = receipt$sha256[[1L]],
    verification_status = "verified", provenance = "receipt-only-reconciliation",
    reconciliation = "receipt-only", url = row$attachment_url[[1L]], download_status = "cached"
  )
  claim <- acquisition_catalog_transaction(catalog, function(conn) {
    now <- acquisition_catalog_now()
    current <- acquisition_catalog_one(conn, "SELECT * FROM acquisition_jobs WHERE job_id = ?", list(job_id))
    if (!current$state[[1L]] %in% c("failed", "blocked")) {
      return(list(ok = FALSE, status = "not-terminal"))
    }
    attempt <- as.integer(current$attempt_count[[1L]]) + 1L
    token <- paste0("receipt-", acquisition_catalog_digest(list(job_id, Sys.time(), Sys.getpid(), runif(1))))
    expires <- acquisition_queue_time_after(300)
    DBI::dbExecute(conn, paste(
      "INSERT INTO acquisition_job_attempts (job_id, attempt_no, lease_token, worker_id, host,",
      "claimed_at, lease_expires_at, heartbeat_at, outcome, result_blob)",
      "VALUES (?, ?, ?, 'receipt-only-reconciliation', ?, ?, ?, ?, 'running', ?)"
    ),
    params = list(
      job_id, attempt, token, current$host[[1L]], now, expires, now,
      acquisition_catalog_blob(list(reconciliation = "receipt-only"))
    )
    )
    DBI::dbExecute(conn, paste(
      "UPDATE acquisition_jobs SET state = 'running', attempt_count = ?,",
      "lease_token = ?, worker_id = 'receipt-only-reconciliation', lease_expires_at = ?, updated_at = ? WHERE job_id = ?"
    ),
    params = list(attempt, token, expires, now, job_id)
    )
    acquisition_queue_sync_host_counts(conn, now)
    list(ok = TRUE, lease_token = token)
  })
  if (!isTRUE(claim$ok)) {
    return(claim)
  }
  synced <- tryCatch(acquisition_queue_sync_sidecar_row(row, publication, cache_root),
    error = function(error) list(status = "retryable", error_category = "sidecar-sync", error_message = conditionMessage(error))
  )
  publication$status <- synced$status
  publication$sync_status <- synced$sync_status %||% synced$status
  publication$error_category <- synced$error_category
  publication$error_message <- synced$error_message
  completion <- acquisition_queue_complete(catalog, job_id, claim$lease_token,
    status = publication$status, result = publication
  )
  completion$reconciliation <- "receipt-only"
  completion$sync_status <- publication$sync_status
  completion
}

#' Audit cache files for verified, unverified, and interrupted publications.
#'
#' @param cache_dir Cache root. Defaults to planscanR's cache root.
#' @param country Optional country filter.
#' @return The tibble returned by [planscanR::audit_cache_downloads()].
#' @export
acquisition_queue_audit_cache <- function(cache_dir = NULL, country = NULL) {
  if (!requireNamespace("planscanR", quietly = TRUE) ||
    !exists("audit_cache_downloads", asNamespace("planscanR"), inherits = FALSE)) {
    acquisition_queue_abort("The installed planscanR package lacks audit_cache_downloads().")
  }
  planscanR::audit_cache_downloads(cache_dir = cache_dir, country = country)
}

acquisition_queue_default_download <- function(
  job,
  catalog,
  lock_timeout = 60,
  sidecar_sync_fn = acquisition_queue_sync_sidecar_row
) {
  if (!requireNamespace("filelock", quietly = TRUE)) {
    return(list(status = "blocked", error_category = "missing-filelock", error_message = "The filelock package is required for canonical publication."))
  }
  row <- acquisition_queue_document_row(acquisition_catalog_connection(catalog), job)
  if (is.null(row) || is.na(row$attachment_url[[1L]]) || !nzchar(row$attachment_url[[1L]])) {
    return(list(status = "blocked", error_category = "missing-attachment-url", error_message = "The persisted document has no attachment URL."))
  }
  url <- as.character(row$attachment_url[[1L]])
  country <- as.character(row$country[[1L]])
  document_ref_id <- as.character(row$document_ref_id[[1L]])
  if (is.function(job$heartbeat)) {
    heartbeat <- job$heartbeat()
    if (!isTRUE(heartbeat$ok)) {
      return(list(status = "retryable", error_category = "lease-stale", error_message = "The download lease expired before publication."))
    }
  }
  cache_root <- planscanR::cache_dir_default()
  cache_document_id <- acquisition_queue_cache_document_id(row$source_ref_id[[1L]], document_ref_id)
  destination <- planscanR::cache_path(url, country, cache_document_id, cache_dir = cache_root)
  lock <- filelock::lock(paste0(destination, ".lock"), timeout = as.integer(lock_timeout * 1000))
  if (is.null(lock)) {
    return(list(status = "retryable", error_category = "download-lock-timeout", error_message = "The canonical download path remained locked."))
  }
  on.exit(filelock::unlock(lock), add = TRUE)
  if (is.function(job$heartbeat)) {
    heartbeat <- job$heartbeat()
    if (!isTRUE(heartbeat$ok)) {
      return(list(status = "retryable", error_category = "lease-stale", error_message = "The download lease expired while waiting for publication."))
    }
  }
  result <- planscanR::download_to_cache(
    url,
    country,
    cache_document_id,
    cache_dir = cache_root,
    overwrite = FALSE
  )
  if (!is.data.frame(result) || nrow(result) == 0L) {
    return(list(status = "retryable", error_category = "empty-download-result", error_message = "The downloader returned no result."))
  }
  status <- as.character(result$status[[1L]])
  if (status %in% c("downloaded", "exists")) {
    local_path <- as.character(result$local_path[[1L]])
    if (is.na(local_path) || !file.exists(local_path)) {
      return(list(status = "retryable", error_category = "missing-published-file", error_message = "The downloader reported success without a file."))
    }
    size_bytes <- as.numeric(result$size_bytes[[1L]])
    sha256 <- as.character(result$sha256[[1L]])
    receipt <- tryCatch(
      planscanR::reconcile_cached_download(
        local_path,
        expected_size_bytes = size_bytes,
        expected_sha256 = sha256,
        url = url,
        repair = FALSE
      ),
      error = function(error) error
    )
    if (inherits(receipt, "error")) {
      return(list(
        status = "retryable",
        error_category = "receipt-check",
        error_message = conditionMessage(receipt)
      ))
    }
    if (!identical(as.character(receipt$status[[1L]]), "verified")) {
      return(list(
        status = "retryable",
        error_category = "unverified-publication",
        error_message = as.character(receipt$reason[[1L]] %||% receipt$status[[1L]]),
        receipt_status = as.character(receipt$status[[1L]])
      ))
    }
    publication <- list(
      status = "succeeded",
      document_ref_id = document_ref_id,
      root_id = "planscan-cache",
      relative_path = acquisition_queue_relative_path(local_path, cache_root),
      local_path = local_path,
      size_bytes = size_bytes,
      sha256 = sha256,
      verification_status = "verified",
      provenance = "planscanR-download",
      url = url,
      download_status = if (identical(status, "exists")) "cached" else "downloaded",
      receipt_status = as.character(receipt$status[[1L]])
    )
    synchronized <- tryCatch(
      sidecar_sync_fn(row, publication, cache_root),
      error = function(error) {
        list(
          status = "retryable",
          error_category = "sidecar-sync",
          error_message = conditionMessage(error)
        )
      }
    )
    if (!is.list(synchronized) || is.null(synchronized$status)) {
      synchronized <- list(
        status = "retryable",
        error_category = "sidecar-sync",
        error_message = "The sidecar synchronization callback returned no status."
      )
    }
    if (!identical(as.character(synchronized$status[[1L]]), "succeeded")) {
      publication$status <- as.character(synchronized$status[[1L]])
      publication$error_category <- synchronized$error_category %||% "sidecar-sync"
      publication$error_message <- synchronized$error_message %||% "The sidecar synchronization did not complete."
      publication$sync_status <- synchronized$status
      return(publication)
    }
    publication$sync_status <- synchronized$sync_status %||% "succeeded"
    publication$sidecar_path <- synchronized$sidecar_path %||% NA_character_
    return(publication)
  }
  if (identical(status, "skipped_size")) {
    return(list(status = "blocked", error_category = "download-size-limit", error_message = as.character(result$reason[[1L]])))
  }
  list(
    status = "retryable",
    error_category = "download-failed",
    error_message = as.character(result$reason[[1L]] %||% status)
  )
}

acquisition_queue_default_discover <- function(job, catalog) {
  conn <- acquisition_catalog_connection(catalog)
  source <- acquisition_catalog_one(
    conn,
    "SELECT * FROM acquisition_sources WHERE source_ref_id = ?",
    list(as.character(job$target_ref_id))
  )
  if (is.null(source)) {
    return(list(status = "blocked", error_category = "missing-source", error_message = "The persisted source identity was not found."))
  }
  if (!requireNamespace("planscanR", quietly = TRUE) || !exists("resolve_attachment_inventory", asNamespace("planscanR"), inherits = FALSE)) {
    return(list(status = "blocked", error_category = "missing-discovery-adapter", error_message = "planscanR does not provide resolve_attachment_inventory()."))
  }
  documents <- DBI::dbGetQuery(
    conn,
    "SELECT document_ref_id, native_attachment_id, attachment_key, attachment_url, section, ordinal, identity_status FROM acquisition_documents WHERE source_ref_id = ? ORDER BY ordinal, document_ref_id",
    params = list(as.character(source$source_ref_id[[1L]]))
  )
  urls <- as.character(documents$attachment_url)
  urls <- urls[!is.na(urls) & nzchar(urls)]
  metadata <- acquisition_queue_unblob(source$metadata_blob)
  records <- data.frame(
    country = as.character(source$country[[1L]]),
    document_id = as.character(source$native_project_id[[1L]]),
    stringsAsFactors = FALSE
  )
  records$attachment_urls <- list(urls)
  stored_status <- acquisition_queue_metadata_field(metadata, "discovery_status")
  stored_reason <- acquisition_queue_metadata_field(metadata, "discovery_reason")
  stored_checked_at <- acquisition_queue_metadata_field(metadata, "discovery_checked_at")
  if (!is.null(stored_status)) records$discovery_status <- stored_status
  if (!is.null(stored_reason)) records$discovery_reason <- stored_reason
  if (!is.null(stored_checked_at)) records$discovery_checked_at <- stored_checked_at
  if (is.function(job$heartbeat)) {
    heartbeat <- job$heartbeat()
    if (!isTRUE(heartbeat$ok)) {
      return(list(status = "retryable", error_category = "lease-stale", error_message = "The discovery lease expired before inventory validation."))
    }
  }
  inventory <- planscanR::resolve_attachment_inventory(records)
  outcome <- as.character(inventory$status[[1L]])
  result_status <- switch(outcome,
    success = "succeeded",
    empty = "succeeded",
    retryable = "retryable",
    blocked = "blocked",
    "retryable"
  )
  result <- list(
    status = result_status,
    discovery_status = outcome,
    discovery_reason = as.character(inventory$reason[[1L]]),
    successful_empty = identical(outcome, "empty"),
    discovered = length(urls),
    source_ref_id = as.character(source$source_ref_id[[1L]])
  )
  if (identical(outcome, "success")) {
    result$documents <- lapply(seq_len(nrow(documents)), function(i) {
      as.list(documents[i, , drop = FALSE])
    })
  }
  if (identical(outcome, "retryable")) {
    result$error_category <- "discovery-retryable"
    result$error_message <- as.character(inventory$reason[[1L]])
  }
  if (identical(outcome, "blocked")) {
    result$error_category <- "discovery-blocked"
    result$error_message <- as.character(inventory$reason[[1L]])
  }
  result
}

#' Run supervised acquisition workers outside Shiny.
#'
#' A worker claims one job, performs handler work outside the SQLite
#' transaction, and fences its completion with the lease token.  The default
#' handlers use persisted catalog identities; tests and deployments may inject
#' handlers with the same `(job, catalog)` signature.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param worker_id Stable process identity.
#' @param max_jobs Maximum number of jobs to process; defaults to one.
#' @param lease_seconds Lease duration for each claim.
#' @param handlers Optional named list with `discover` and/or `download`
#'   functions.
#' @param poll_seconds Optional delay after an empty claim before returning.
#' @param lock_timeout Download publication lock timeout in seconds.
#' @return A list of worker outcomes, one per claimed job.
#' @export
acquisition_worker_run <- function(
  catalog,
  worker_id = paste0("worker-", Sys.getpid()),
  max_jobs = 1L,
  lease_seconds = 300L,
  handlers = NULL,
  poll_seconds = 0,
  lock_timeout = 60
) {
  acquisition_queue_heartbeat_preflight()
  worker_id <- acquisition_catalog_scalar_character(worker_id, "worker_id")
  if (
    length(max_jobs) != 1L ||
      is.na(max_jobs) ||
      (!is.infinite(max_jobs) && (!is.numeric(max_jobs) || max_jobs < 0 || max_jobs != trunc(max_jobs)))
  ) {
    acquisition_queue_abort("{.arg max_jobs} must be a non-negative integer or Inf.")
  }
  max_jobs <- if (is.infinite(max_jobs)) Inf else as.integer(max_jobs)
  lease_seconds <- acquisition_catalog_scalar_integer(lease_seconds, "lease_seconds", lower = 1L, upper = 604800L)
  poll_seconds <- acquisition_queue_nonnegative_seconds(poll_seconds, "poll_seconds", upper = 3600)
  lock_timeout <- acquisition_queue_nonnegative_seconds(lock_timeout, "lock_timeout", upper = 3600)
  if (is.null(handlers) && lock_timeout >= lease_seconds) {
    acquisition_queue_abort("{.arg lock_timeout} must be shorter than {.arg lease_seconds} for built-in downloads.")
  }
  defaults <- list(
    discover = acquisition_queue_default_discover,
    download = function(job, catalog) acquisition_queue_default_download(job, catalog, lock_timeout = lock_timeout)
  )
  if (!is.null(handlers)) {
    if (!is.list(handlers)) acquisition_queue_abort("{.arg handlers} must be a named list of functions.")
    if (is.null(names(handlers)) || any(!nzchar(names(handlers)))) acquisition_queue_abort("{.arg handlers} must be named.")
    for (name in names(handlers)) {
      if (!is.function(handlers[[name]])) acquisition_queue_abort(paste0("Handler `", name, "` must be a function."))
      if (!name %in% names(defaults)) acquisition_queue_abort(paste0("Unknown worker handler: ", name))
      defaults[[name]] <- handlers[[name]]
    }
  }
  outcomes <- list()
  while (length(outcomes) < max_jobs) {
    acquisition_queue_heartbeat_preflight()
    claim <- acquisition_queue_claim(catalog, worker_id, lease_seconds = lease_seconds)
    if (!isTRUE(claim$ok)) {
      if (poll_seconds > 0 && length(outcomes) < max_jobs) Sys.sleep(poll_seconds)
      break
    }
    claim$heartbeat <- function(seconds = lease_seconds) {
      acquisition_queue_heartbeat(catalog, claim$job_id, claim$lease_token, lease_seconds = seconds)
    }
    handler <- defaults[[as.character(claim$kind)]]
    heartbeat_guard <- acquisition_queue_start_heartbeat_guard(catalog, claim, lease_seconds)
    if (!isTRUE(heartbeat_guard$ok)) {
      result <- list(
        status = "retryable",
        error_category = "heartbeat-unavailable",
        error_message = heartbeat_guard$error_message
      )
    } else {
      result <- tryCatch(
        acquisition_queue_call_handler(handler, claim, catalog),
        error = function(error) {
          list(
            status = "retryable",
            error_category = "worker-error",
            error_message = conditionMessage(error)
          )
        }
      )
      if (!isTRUE(acquisition_queue_stop_heartbeat_guard(heartbeat_guard))) {
        result <- list(
          status = "retryable",
          error_category = "lease-heartbeat-lost",
          error_message = "The worker lost its automatic lease heartbeat before completion."
        )
      }
    }
    result <- acquisition_queue_normalise_handler_result(result)
    completed <- tryCatch(
      acquisition_queue_complete(
        catalog,
        claim$job_id,
        claim$lease_token,
        status = result$status,
        result = result,
        error_category = result$error_category,
        error_message = result$error_message,
        retry_after_seconds = result$retry_after_seconds,
        cooldown_seconds = result$cooldown_seconds
      ),
      error = function(error) list(ok = FALSE, status = "completion-error", error_message = conditionMessage(error))
    )
    outcomes[[length(outcomes) + 1L]] <- list(
      job_id = claim$job_id,
      attempt_no = claim$attempt_no,
      lease_token = claim$lease_token,
      status = completed$status %||% result$status,
      completion = completed,
      download_jobs = completed$download_jobs %||% list(),
      result = result
    )
  }
  outcomes
}
