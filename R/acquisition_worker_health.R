# ACQ-RUN3: worker-process liveness is separate from queue attempt progress.
# The catalog stores only the latest lifecycle observation for the bounded
# supervised worker pool. Stale and unknown are derived at read time.

# Keep reads and persisted identity configuration bounded without baking the
# BIOGAIN deployment's default two-worker pool into the generic screen API.
acquisition_worker_health_max_workers <- 32L

acquisition_worker_health_abort <- function(message) {
  acquisition_queue_abort(message)
}

acquisition_worker_health_ids <- function(value, arg = "worker_ids", allow_null = FALSE) {
  if (is.null(value) && isTRUE(allow_null)) {
    return(NULL)
  }
  if (!is.character(value) || anyNA(value) || length(value) > acquisition_worker_health_max_workers) {
    acquisition_worker_health_abort(
      paste0("{.arg ", arg, "} must contain at most ", acquisition_worker_health_max_workers, " worker IDs.")
    )
  }
  value <- trimws(value)
  if (length(value) == 0L || any(!nzchar(value)) || anyDuplicated(value) > 0L ||
    any(!grepl("^[A-Za-z0-9][A-Za-z0-9_.-]{0,127}$", value))) {
    acquisition_worker_health_abort(
      paste0("{.arg ", arg, "} must contain unique worker IDs using letters, numbers, dots, underscores, or hyphens.")
    )
  }
  value
}

acquisition_worker_health_id <- function(value, arg = "worker_id") {
  if (!is.character(value) || length(value) != 1L) {
    acquisition_worker_health_abort(paste0("{.arg ", arg, "} must be one worker ID."))
  }
  ids <- acquisition_worker_health_ids(value, arg = arg)
  ids[[1L]]
}

acquisition_worker_health_now <- function(value, arg = "now") {
  if (inherits(value, "POSIXlt")) value <- as.POSIXct(value)
  if (!inherits(value, "POSIXct") || length(value) != 1L || is.na(value)) {
    acquisition_worker_health_abort(paste0("{.arg ", arg, "} must be one non-missing POSIXct value."))
  }
  as.POSIXct(value, tz = "UTC")
}

acquisition_worker_health_time_text <- function(value) {
  format(acquisition_worker_health_now(value), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
}

acquisition_worker_health_optional_text <- function(value, arg, max_chars = 128L) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]])) {
    return(NA_character_)
  }
  value <- trimws(as.character(value[[1L]]))
  if (!nzchar(value) || nchar(value) > max_chars) {
    acquisition_worker_health_abort(paste0("{.arg ", arg, "} must be non-empty and at most ", max_chars, " characters."))
  }
  value
}

acquisition_worker_health_error_message <- function(value) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]])) {
    return(NA_character_)
  }
  value <- as.character(value[[1L]])
  if (exists("acquisition_queue_redact_sensitive", mode = "function")) {
    value <- acquisition_queue_redact_sensitive(value)
  }
  substr(value, 1L, 1000L)
}

acquisition_worker_health_expected_flag <- function(value) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    acquisition_worker_health_abort("{.arg expected} must be one TRUE or FALSE value.")
  }
  value
}

acquisition_worker_health_existing <- function(conn, worker_id) {
  acquisition_catalog_one(
    conn,
    "SELECT * FROM acquisition_worker_health WHERE worker_id = ?",
    list(worker_id)
  )
}

acquisition_worker_health_is_registered <- function(catalog, worker_id) {
  worker_id <- acquisition_catalog_scalar_character(worker_id, "worker_id")
  !is.null(acquisition_catalog_one(
    acquisition_catalog_connection(catalog),
    "SELECT worker_id FROM acquisition_worker_health WHERE worker_id = ? AND expected = 1",
    list(worker_id)
  ))
}

acquisition_worker_health_assert_capacity <- function(conn, worker_id, expected) {
  if (!isTRUE(expected)) {
    return(invisible(TRUE))
  }
  other_expected <- DBI::dbGetQuery(
    conn,
    "SELECT COUNT(*) AS n FROM acquisition_worker_health WHERE expected = 1 AND worker_id <> ?",
    params = list(worker_id)
  )$n[[1L]]
  if (as.integer(other_expected) >= acquisition_worker_health_max_workers) {
    acquisition_worker_health_abort(
      paste0("The acquisition worker pool cannot expect more than ", acquisition_worker_health_max_workers, " identities.")
    )
  }
  invisible(TRUE)
}

acquisition_worker_health_insert_unknown <- function(conn, worker_id, now) {
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO acquisition_worker_health (worker_id, expected, state,",
      "started_at, last_seen_at, stopped_at, error_category, error_message, updated_at)",
      "VALUES (?, 1, 'unknown', NULL, NULL, NULL, NULL, NULL, ?)"
    ),
    params = list(worker_id, now)
  )
  invisible(TRUE)
}

#' Configure the expected independent acquisition workers.
#'
#' The generic registry is bounded to 32 identities. Missing identities are
#' represented as `unknown` until they publish a lifecycle heartbeat.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param worker_ids One to 32 stable worker process identities.
#' @return A list containing the configured `worker_ids`.
#' @export
acquisition_worker_health_configure <- function(catalog, worker_ids) {
  worker_ids <- acquisition_worker_health_ids(worker_ids)
  acquisition_catalog_transaction(catalog, function(conn) {
    now <- acquisition_catalog_now()
    existing_expected <- DBI::dbGetQuery(
      conn,
      "SELECT worker_id FROM acquisition_worker_health WHERE expected = 1 ORDER BY worker_id LIMIT ?",
      params = list(acquisition_worker_health_max_workers)
    )$worker_id
    for (worker_id in setdiff(existing_expected, worker_ids)) {
      DBI::dbExecute(
        conn,
        "UPDATE acquisition_worker_health SET expected = 0, updated_at = ? WHERE worker_id = ?",
        params = list(now, worker_id)
      )
    }
    for (worker_id in worker_ids) {
      existing <- acquisition_worker_health_existing(conn, worker_id)
      if (is.null(existing)) {
        acquisition_worker_health_insert_unknown(conn, worker_id, now)
      } else {
        DBI::dbExecute(
          conn,
          "UPDATE acquisition_worker_health SET expected = 1, updated_at = ? WHERE worker_id = ?",
          params = list(now, worker_id)
        )
      }
    }
    list(worker_ids = worker_ids, status = "configured")
  })
}

#' Record a worker startup and initial idle heartbeat.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param worker_id Stable worker process identity.
#' @param expected Whether this identity belongs to the expected worker pool.
#' @param now Observation time, primarily for deterministic tests.
#' @return A list with the worker identity and lifecycle state.
#' @export
acquisition_worker_health_start <- function(
  catalog,
  worker_id,
  expected = TRUE,
  now = Sys.time()
) {
  worker_id <- acquisition_worker_health_id(worker_id)
  expected <- acquisition_worker_health_expected_flag(expected)
  now <- acquisition_worker_health_time_text(now)
  acquisition_catalog_transaction(catalog, function(conn) {
    acquisition_worker_health_assert_capacity(conn, worker_id, expected)
    existing <- acquisition_worker_health_existing(conn, worker_id)
    if (is.null(existing)) {
      DBI::dbExecute(
        conn,
        paste(
          "INSERT INTO acquisition_worker_health (worker_id, expected, state,",
          "started_at, last_seen_at, stopped_at, error_category, error_message, updated_at)",
          "VALUES (?, ?, 'idle', ?, ?, NULL, NULL, NULL, ?)"
        ),
        params = list(worker_id, as.integer(expected), now, now, now)
      )
    } else {
      DBI::dbExecute(
        conn,
        paste(
          "UPDATE acquisition_worker_health SET expected = ?, state = 'idle',",
          "started_at = ?, last_seen_at = ?, stopped_at = NULL,",
          "error_category = NULL, error_message = NULL, updated_at = ?",
          "WHERE worker_id = ?"
        ),
        params = list(as.integer(expected), now, now, now, worker_id)
      )
    }
    list(ok = TRUE, worker_id = worker_id, state = "idle", last_seen_at = now)
  })
}

#' Record an independent worker heartbeat.
#'
#' Heartbeats contain lifecycle state only. Queue attempts and their byte
#' receipts remain separate records.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param worker_id Stable worker process identity.
#' @param state Either `idle` or `busy`.
#' @param now Observation time, primarily for deterministic tests.
#' @return A list with the worker identity and lifecycle state.
#' @export
acquisition_worker_health_heartbeat <- function(
  catalog,
  worker_id,
  state = c("idle", "busy"),
  now = Sys.time()
) {
  worker_id <- acquisition_worker_health_id(worker_id)
  state <- match.arg(state)
  now <- acquisition_worker_health_time_text(now)
  acquisition_catalog_transaction(catalog, function(conn) {
    existing <- acquisition_worker_health_existing(conn, worker_id)
    if (is.null(existing) || as.integer(existing$expected[[1L]]) != 1L) {
      return(list(ok = FALSE, status = "unregistered", worker_id = worker_id))
    }
    DBI::dbExecute(
      conn,
      paste(
        "UPDATE acquisition_worker_health SET state = ?,",
        "started_at = COALESCE(started_at, ?), last_seen_at = ?, stopped_at = NULL,",
        "error_category = NULL, error_message = NULL, updated_at = ?",
        "WHERE worker_id = ? AND expected = 1"
      ),
      params = list(state, now, now, now, worker_id)
    )
    list(ok = TRUE, worker_id = worker_id, state = state, last_seen_at = now)
  })
}

#' Record a graceful stop or worker error.
#'
#' Error text is redacted and bounded before persistence. A process that is
#' killed without calling this function remains `stale` at the next read.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param worker_id Stable worker process identity.
#' @param state Either `stopped` or `error`.
#' @param error_category Optional bounded diagnostic category.
#' @param error_message Optional diagnostic text; credential-shaped values are
#'   redacted before persistence.
#' @param now Observation time, primarily for deterministic tests.
#' @return A list with the worker identity and lifecycle state.
#' @export
acquisition_worker_health_stop <- function(
  catalog,
  worker_id,
  state = c("stopped", "error"),
  error_category = NULL,
  error_message = NULL,
  now = Sys.time()
) {
  worker_id <- acquisition_worker_health_id(worker_id)
  state <- match.arg(state)
  error_category <- acquisition_worker_health_optional_text(error_category, "error_category")
  error_message <- acquisition_worker_health_error_message(error_message)
  now <- acquisition_worker_health_time_text(now)
  if (identical(state, "stopped")) {
    error_category <- NA_character_
    error_message <- NA_character_
  }
  acquisition_catalog_transaction(catalog, function(conn) {
    existing <- acquisition_worker_health_existing(conn, worker_id)
    if (is.null(existing) || as.integer(existing$expected[[1L]]) != 1L) {
      return(list(ok = FALSE, status = "unregistered", worker_id = worker_id))
    }
    DBI::dbExecute(
      conn,
      paste(
        "UPDATE acquisition_worker_health SET state = ?, last_seen_at = ?,",
        "stopped_at = ?, error_category = ?, error_message = ?, updated_at = ?",
        "WHERE worker_id = ? AND expected = 1"
      ),
      params = list(state, now, now, error_category, error_message, now, worker_id)
    )
    list(ok = TRUE, worker_id = worker_id, state = state, last_seen_at = now)
  })
}

acquisition_worker_health_parse_time <- function(value) {
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]])) {
    return(as.POSIXct(NA, origin = "1970-01-01", tz = "UTC"))
  }
  as.POSIXct(
    sub("Z$", "", as.character(value[[1L]])),
    format = "%Y-%m-%dT%H:%M:%OS",
    tz = "UTC"
  )
}

acquisition_worker_health_status_row <- function(worker_id, row, now, freshness_seconds) {
  missing <- is.null(row)
  raw_state <- if (missing) "unknown" else as.character(row$state[[1L]])
  last_seen <- if (missing) NA_character_ else as.character(row$last_seen_at[[1L]])
  last_seen_time <- acquisition_worker_health_parse_time(last_seen)
  age <- if (is.na(last_seen_time)) {
    NA_real_
  } else {
    max(0, as.numeric(now - last_seen_time, units = "secs"))
  }
  state <- if (raw_state %in% c("stopped", "error", "unknown")) {
    raw_state
  } else if (is.na(age)) {
    "unknown"
  } else if (age > freshness_seconds) {
    "stale"
  } else {
    raw_state
  }
  list(
    worker_id = worker_id,
    expected = TRUE,
    state = state,
    started_at = if (missing) NA_character_ else as.character(row$started_at[[1L]]),
    last_seen_at = last_seen,
    last_seen_age_seconds = age,
    alive = state %in% c("idle", "busy"),
    available = identical(state, "idle"),
    stopped_at = if (missing) NA_character_ else as.character(row$stopped_at[[1L]]),
    error_category = if (missing) NA_character_ else as.character(row$error_category[[1L]]),
    error_message = if (missing) NA_character_ else as.character(row$error_message[[1L]])
  )
}

#' Read bounded independent worker health.
#'
#' `stale` and `unknown` are derived from the persisted lifecycle rows and are
#' never inferred from the queue's running-attempt table. When `expected_workers`
#' is omitted, configured identities in the catalog are read; callers may pass
#' trusted deployment identities to include an unstarted worker as
#' `unknown`.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param expected_workers Optional one to 32 trusted worker identities.
#' @param freshness_seconds Maximum age of an idle/busy heartbeat in seconds.
#' @param now Observation time, primarily for deterministic tests.
#' @return A list with `workers`, bounded `counts`, and `as_of`. `workers` has
#'   lifecycle and freshness fields but no progress or byte claims.
#' @export
acquisition_worker_health_status <- function(
  catalog,
  expected_workers = NULL,
  freshness_seconds = 30,
  now = Sys.time()
) {
  expected_workers <- if (is.null(expected_workers)) {
    NULL
  } else if (length(expected_workers) == 0L) {
    character(0)
  } else {
    acquisition_worker_health_ids(expected_workers, arg = "expected_workers")
  }
  freshness_seconds <- acquisition_queue_nonnegative_seconds(
    freshness_seconds,
    "freshness_seconds",
    upper = 604800
  )
  now_time <- acquisition_worker_health_now(now)
  now_text <- acquisition_worker_health_time_text(now_time)
  rows <- acquisition_catalog_transaction(catalog, function(conn) {
    if (is.null(expected_workers)) {
      DBI::dbGetQuery(
        conn,
        paste(
          "SELECT worker_id, expected, state, started_at, last_seen_at, stopped_at,",
          "error_category, error_message FROM acquisition_worker_health",
          "WHERE expected = 1 ORDER BY worker_id LIMIT ?"
        ),
        params = list(acquisition_worker_health_max_workers)
      )
    } else if (length(expected_workers) == 0L) {
      data.frame()
    } else {
      placeholders <- paste(rep("?", length(expected_workers)), collapse = ", ")
      DBI::dbGetQuery(
        conn,
        paste(
          "SELECT worker_id, expected, state, started_at, last_seen_at, stopped_at,",
          "error_category, error_message FROM acquisition_worker_health",
          "WHERE worker_id IN (", placeholders, ") ORDER BY worker_id LIMIT ?"
        ),
        params = c(as.list(expected_workers), list(acquisition_worker_health_max_workers))
      )
    }
  })
  if (is.null(expected_workers)) {
    worker_ids <- as.character(rows$worker_id)
  } else if (length(expected_workers) == 0L) {
    worker_ids <- character(0)
  } else {
    worker_ids <- expected_workers
  }
  worker_rows <- lapply(worker_ids, function(worker_id) {
    row <- if (nrow(rows) == 0L) NULL else rows[rows$worker_id == worker_id, , drop = FALSE]
    if (!is.null(row) && nrow(row) == 0L) row <- NULL
    acquisition_worker_health_status_row(worker_id, row, now_time, freshness_seconds)
  })
  workers <- if (length(worker_rows) == 0L) {
    data.frame(
      worker_id = character(0), expected = logical(0), state = character(0),
      started_at = character(0), last_seen_at = character(0),
      last_seen_age_seconds = numeric(0), alive = logical(0), available = logical(0),
      stopped_at = character(0), error_category = character(0),
      error_message = character(0), stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, lapply(worker_rows, as.data.frame, stringsAsFactors = FALSE))
  }
  rownames(workers) <- NULL
  counts <- c(
    expected = as.integer(nrow(workers)),
    alive = as.integer(sum(workers$alive)),
    available = as.integer(sum(workers$available))
  )
  structure(
    list(workers = workers, counts = counts, as_of = now_text),
    class = "planscanR_acquisition_worker_health"
  )
}
