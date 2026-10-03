# ACQ-2b: authenticated, audited queue controls.

acquisition_controls_abort <- function(
  message,
  class = "planscanR_error_acquisition_control",
  ...
) {
  rlang::abort(message, class = unique(c(class, "planscanR_error_acquisition")), ...)
}

acquisition_controls_scalar <- function(value, arg, allow_null = FALSE) {
  if (is.null(value) && isTRUE(allow_null)) {
    return(NULL)
  }
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
    acquisition_controls_abort(paste0("{.arg ", arg, "} must be one non-empty character string."))
  }
  trimws(value)
}

acquisition_controls_optional <- function(value, arg) {
  if (is.null(value) || (length(value) == 1L && is.na(value))) {
    return(NA_character_)
  }
  if (is.character(value) && length(value) == 1L && !is.na(value) && !nzchar(trimws(value))) {
    return(NA_character_)
  }
  acquisition_controls_scalar(value, arg)
}

acquisition_controls_ensure_schema_connection <- function(conn) {
  DBI::dbExecute(
    conn,
    paste(
      "CREATE TABLE IF NOT EXISTS acquisition_control_flags (",
      "target_type TEXT NOT NULL CHECK (target_type IN ('job', 'campaign')),",
      "target_id TEXT NOT NULL,",
      "action TEXT NOT NULL CHECK (action IN ('pause', 'cancel')),",
      "active INTEGER NOT NULL DEFAULT 1 CHECK (active IN (0, 1)),",
      "actor_id TEXT,",
      "reason TEXT,",
      "requested_at TEXT NOT NULL,",
      "updated_at TEXT NOT NULL,",
      "PRIMARY KEY (target_type, target_id, action)",
      ")"
    )
  )
  DBI::dbExecute(
    conn,
    paste(
      "CREATE TABLE IF NOT EXISTS acquisition_control_pauses (",
      "job_id TEXT NOT NULL,",
      "target_type TEXT NOT NULL CHECK (target_type IN ('job', 'campaign')),",
      "target_id TEXT NOT NULL,",
      "previous_state TEXT NOT NULL,",
      "previous_available_at TEXT NOT NULL,",
      "paused_at TEXT NOT NULL,",
      "PRIMARY KEY (job_id, target_type, target_id),",
      "FOREIGN KEY (job_id) REFERENCES acquisition_jobs(job_id)",
      ")"
    )
  )
  DBI::dbExecute(
    conn,
    paste(
      "CREATE TABLE IF NOT EXISTS acquisition_control_audit (",
      "audit_id TEXT PRIMARY KEY,",
      "action TEXT NOT NULL CHECK (action IN ('pause', 'resume', 'retry', 'cancel')),",
      "target_type TEXT NOT NULL CHECK (target_type IN ('job', 'campaign')),",
      "target_id TEXT NOT NULL,",
      "actor_id TEXT NOT NULL,",
      "reason TEXT,",
      "prior_state TEXT NOT NULL,",
      "resulting_state TEXT NOT NULL,",
      "details_blob TEXT,",
      "created_at TEXT NOT NULL",
      ")"
    )
  )
  DBI::dbExecute(
    conn,
    paste(
      "CREATE INDEX IF NOT EXISTS acquisition_control_flags_claim_idx",
      "ON acquisition_control_flags (active, action, target_type, target_id)"
    )
  )
  DBI::dbExecute(
    conn,
    paste(
      "CREATE INDEX IF NOT EXISTS acquisition_control_audit_target_idx",
      "ON acquisition_control_audit (target_type, target_id, created_at)"
    )
  )
  invisible(TRUE)
}

acquisition_controls_ensure_schema <- function(catalog) {
  conn <- acquisition_catalog_connection(catalog)
  acquisition_controls_ensure_schema_connection(conn)
  invisible(TRUE)
}

acquisition_controls_check_resolver <- function(actor_resolver) {
  if (!inherits(actor_resolver, "planscanR_curation_actor_resolver")) {
    acquisition_controls_abort(
      "{.arg actor_resolver} must be a trusted curation actor resolver.",
      class = "planscanR_error_curation_actor"
    )
  }
  invisible(actor_resolver)
}

acquisition_controls_admin_actor <- function(actor_resolver, request) {
  acquisition_controls_check_resolver(actor_resolver)
  actor <- curation_resolve_actor(actor_resolver, request)
  roles <- tolower(as.character(actor$roles))
  if (!"admin" %in% roles) {
    acquisition_controls_abort(
      "An authenticated admin role is required for acquisition controls.",
      class = "planscanR_error_acquisition_forbidden"
    )
  }
  list(id = as.character(actor$id), roles = roles, display_name = actor$display_name)
}

acquisition_controls_target_jobs <- function(conn, target_type, target_id) {
  if (identical(target_type, "job")) {
    return(DBI::dbGetQuery(
      conn,
      "SELECT * FROM acquisition_jobs WHERE job_id = ?",
      params = list(target_id)
    ))
  }
  DBI::dbGetQuery(
    conn,
    "SELECT * FROM acquisition_jobs WHERE campaign_id = ? ORDER BY priority DESC, created_at, job_id",
    params = list(target_id)
  )
}

acquisition_controls_state_summary <- function(states) {
  states <- as.character(states)
  states <- states[!is.na(states) & nzchar(states)]
  if (length(states) == 0L) {
    return("none")
  }
  counts <- table(states)
  paste(paste0(names(counts), "=", as.integer(counts)), collapse = ",")
}

acquisition_controls_upsert_flag <- function(conn, target_type, target_id, action, actor_id, reason, active = TRUE) {
  now <- acquisition_catalog_now()
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO acquisition_control_flags (target_type, target_id, action, active, actor_id, reason, requested_at, updated_at)",
      "VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
      "ON CONFLICT(target_type, target_id, action) DO UPDATE SET active = excluded.active,",
      "actor_id = excluded.actor_id, reason = excluded.reason, updated_at = excluded.updated_at"
    ),
    params = list(target_type, target_id, action, as.integer(active), actor_id, reason, now, now)
  )
  invisible(TRUE)
}

acquisition_controls_set_flag <- function(conn, target_type, target_id, action, active) {
  DBI::dbExecute(
    conn,
    "UPDATE acquisition_control_flags SET active = ?, updated_at = ? WHERE target_type = ? AND target_id = ? AND action = ?",
    params = list(as.integer(active), acquisition_catalog_now(), target_type, target_id, action)
  )
  invisible(TRUE)
}

acquisition_controls_insert_audit <- function(
  conn,
  action,
  target_type,
  target_id,
  actor_id,
  reason,
  prior_state,
  resulting_state,
  details
) {
  created_at <- acquisition_catalog_now()
  audit_id <- paste0(
    "control-",
    acquisition_catalog_digest(list(action, target_type, target_id, actor_id, reason, prior_state, resulting_state, details, created_at, runif(1)))
  )
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO acquisition_control_audit (audit_id, action, target_type, target_id, actor_id, reason,",
      "prior_state, resulting_state, details_blob, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ),
    params = list(
      audit_id,
      action,
      target_type,
      target_id,
      actor_id,
      reason,
      prior_state,
      resulting_state,
      acquisition_catalog_blob(details),
      created_at
    )
  )
  audit_id
}

acquisition_controls_pause_connection <- function(conn, target_type, target_id, actor_id, reason) {
  jobs <- acquisition_controls_target_jobs(conn, target_type, target_id)
  prior_state <- acquisition_controls_state_summary(jobs$state)
  acquisition_controls_upsert_flag(conn, target_type, target_id, "pause", actor_id, reason, active = TRUE)
  paused <- integer(0)
  if (nrow(jobs) > 0L) {
    pending <- jobs$state %in% c("queued", "retry_wait")
    for (i in which(pending)) {
      DBI::dbExecute(
        conn,
        paste(
          "INSERT OR IGNORE INTO acquisition_control_pauses (job_id, target_type, target_id, previous_state,",
          "previous_available_at, paused_at) VALUES (?, ?, ?, ?, ?, ?)"
        ),
        params = list(
          jobs$job_id[[i]],
          target_type,
          target_id,
          jobs$state[[i]],
          jobs$available_at[[i]],
          acquisition_catalog_now()
        )
      )
      DBI::dbExecute(
        conn,
        paste(
          "UPDATE acquisition_jobs SET state = 'blocked', last_error_category = 'paused',",
          "last_error = ?, updated_at = ? WHERE job_id = ? AND state IN ('queued', 'retry_wait')"
        ),
        params = list("Paused by an authenticated acquisition administrator.", acquisition_catalog_now(), jobs$job_id[[i]])
      )
      paused <- c(paused, jobs$job_id[[i]])
    }
  }
  updated <- acquisition_controls_target_jobs(conn, target_type, target_id)
  audit_id <- acquisition_controls_insert_audit(
    conn,
    action = "pause",
    target_type = target_type,
    target_id = target_id,
    actor_id = actor_id,
    reason = reason,
    prior_state = prior_state,
    resulting_state = acquisition_controls_state_summary(updated$state),
    details = list(paused_job_ids = paused, running_job_ids = updated$job_id[updated$state == "running"])
  )
  list(
    status = "paused",
    target_type = target_type,
    target_id = target_id,
    audit_id = audit_id,
    affected = length(paused),
    running = sum(updated$state == "running"),
    actor_id = actor_id
  )
}

acquisition_controls_resume_connection <- function(conn, target_type, target_id, actor_id, reason) {
  jobs <- acquisition_controls_target_jobs(conn, target_type, target_id)
  prior_state <- acquisition_controls_state_summary(jobs$state)
  acquisition_controls_set_flag(conn, target_type, target_id, "pause", active = FALSE)
  paused <- DBI::dbGetQuery(
    conn,
    "SELECT * FROM acquisition_control_pauses WHERE target_type = ? AND target_id = ? ORDER BY job_id",
    params = list(target_type, target_id)
  )
  resumed <- character(0)
  if (nrow(paused) > 0L) {
    for (i in seq_len(nrow(paused))) {
      changed <- DBI::dbExecute(
        conn,
        paste(
          "UPDATE acquisition_jobs SET state = ?, available_at = ?, last_error_category = NULL,",
          "last_error = NULL, updated_at = ? WHERE job_id = ? AND state = 'blocked' AND last_error_category = 'paused'"
        ),
        params = list(
          paused$previous_state[[i]],
          paused$previous_available_at[[i]],
          acquisition_catalog_now(),
          paused$job_id[[i]]
        )
      )
      if (changed == 1L) resumed <- c(resumed, paused$job_id[[i]])
    }
  }
  DBI::dbExecute(
    conn,
    "DELETE FROM acquisition_control_pauses WHERE target_type = ? AND target_id = ?",
    params = list(target_type, target_id)
  )
  updated <- acquisition_controls_target_jobs(conn, target_type, target_id)
  audit_id <- acquisition_controls_insert_audit(
    conn,
    action = "resume",
    target_type = target_type,
    target_id = target_id,
    actor_id = actor_id,
    reason = reason,
    prior_state = prior_state,
    resulting_state = acquisition_controls_state_summary(updated$state),
    details = list(resumed_job_ids = resumed)
  )
  list(
    status = "resumed",
    target_type = target_type,
    target_id = target_id,
    audit_id = audit_id,
    affected = length(resumed),
    running = sum(updated$state == "running"),
    actor_id = actor_id
  )
}

acquisition_controls_cancel_connection <- function(conn, target_type, target_id, actor_id, reason) {
  jobs <- acquisition_controls_target_jobs(conn, target_type, target_id)
  prior_state <- acquisition_controls_state_summary(jobs$state)
  acquisition_controls_upsert_flag(conn, target_type, target_id, "cancel", actor_id, reason, active = TRUE)
  cancelled <- character(0)
  if (nrow(jobs) > 0L) {
    pending <- jobs$state %in% c("queued", "retry_wait", "blocked")
    for (i in which(pending)) {
      DBI::dbExecute(
        conn,
        paste(
          "UPDATE acquisition_jobs SET state = 'cancelled', last_error_category = 'cancelled',",
          "last_error = ?, completed_at = ?, updated_at = ? WHERE job_id = ? AND state IN ('queued', 'retry_wait', 'blocked')"
        ),
        params = list(
          "Cancelled by an authenticated acquisition administrator.",
          acquisition_catalog_now(),
          acquisition_catalog_now(),
          jobs$job_id[[i]]
        )
      )
      cancelled <- c(cancelled, jobs$job_id[[i]])
    }
  }
  updated <- acquisition_controls_target_jobs(conn, target_type, target_id)
  audit_id <- acquisition_controls_insert_audit(
    conn,
    action = "cancel",
    target_type = target_type,
    target_id = target_id,
    actor_id = actor_id,
    reason = reason,
    prior_state = prior_state,
    resulting_state = acquisition_controls_state_summary(updated$state),
    details = list(
      cancelled_job_ids = cancelled,
      running_job_ids = updated$job_id[updated$state == "running"],
      safe_stop = any(updated$state == "running")
    )
  )
  list(
    status = "cancel_requested",
    target_type = target_type,
    target_id = target_id,
    audit_id = audit_id,
    affected = length(cancelled),
    running = sum(updated$state == "running"),
    actor_id = actor_id
  )
}

acquisition_controls_retry_connection <- function(conn, target_type, target_id, actor_id, reason) {
  jobs <- acquisition_controls_target_jobs(conn, target_type, target_id)
  prior_state <- acquisition_controls_state_summary(jobs$state)
  paused <- acquisition_catalog_one(
    conn,
    "SELECT target_id FROM acquisition_control_flags WHERE target_type = ? AND target_id = ? AND action = 'pause' AND active = 1",
    list(target_type, target_id)
  )
  if (!is.null(paused)) {
    acquisition_controls_abort(
      "Resume the paused acquisition target before retrying it.",
      class = "planscanR_error_acquisition_control_state"
    )
  }
  acquisition_controls_set_flag(conn, target_type, target_id, "cancel", active = FALSE)
  retried <- character(0)
  retry_allowance_job_ids <- character(0)
  if (nrow(jobs) > 0L) {
    eligible <- jobs$state %in% c("failed", "blocked", "cancelled")
    for (i in which(eligible)) {
      changed <- DBI::dbExecute(
        conn,
        paste(
          "UPDATE acquisition_jobs SET state = 'queued', available_at = ?,",
          "max_attempts = CASE WHEN attempt_count >= max_attempts THEN attempt_count + 1 ELSE max_attempts END,",
          "lease_token = NULL,",
          "worker_id = NULL, lease_expires_at = NULL, last_error_category = NULL, last_error = NULL,",
          "completed_at = NULL, updated_at = ? WHERE job_id = ? AND state IN ('failed', 'blocked', 'cancelled')"
        ),
        params = list(acquisition_catalog_now(), acquisition_catalog_now(), jobs$job_id[[i]])
      )
      if (changed == 1L) {
        retried <- c(retried, jobs$job_id[[i]])
        if (isTRUE(jobs$attempt_count[[i]] >= jobs$max_attempts[[i]])) {
          retry_allowance_job_ids <- c(retry_allowance_job_ids, jobs$job_id[[i]])
        }
      }
    }
  }
  updated <- acquisition_controls_target_jobs(conn, target_type, target_id)
  audit_id <- acquisition_controls_insert_audit(
    conn,
    action = "retry",
    target_type = target_type,
    target_id = target_id,
    actor_id = actor_id,
    reason = reason,
    prior_state = prior_state,
    resulting_state = acquisition_controls_state_summary(updated$state),
    details = list(
      retried_job_ids = retried,
      retry_allowance_job_ids = retry_allowance_job_ids,
      running_job_ids = updated$job_id[updated$state == "running"]
    )
  )
  list(
    status = "retry_requested",
    target_type = target_type,
    target_id = target_id,
    audit_id = audit_id,
    affected = length(retried),
    retry_allowance = length(retry_allowance_job_ids),
    running = sum(updated$state == "running"),
    actor_id = actor_id
  )
}

acquisition_controls_target_type <- function(target_type) {
  target_type <- acquisition_controls_scalar(target_type, "target_type")
  if (!target_type %in% c("job", "campaign")) {
    acquisition_controls_abort("{.arg target_type} must be `job` or `campaign`.")
  }
  target_type
}

#' Apply an authenticated, audited acquisition control action.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param action One of `pause`, `resume`, `retry`, or `cancel`.
#' @param target_type Either `job` or `campaign`.
#' @param target_id Durable job or campaign ID.
#' @param actor_resolver A trusted authenticated actor resolver.
#' @param request Deployment-supplied request/session context.
#' @param reason Optional operator reason retained in the audit trail.
#' @return A list describing the authenticated action and affected jobs.
#' @export
acquisition_control_apply <- function(
  catalog,
  action,
  target_type,
  target_id,
  actor_resolver,
  request = list(),
  reason = NULL
) {
  acquisition_catalog_connection(catalog)
  action <- acquisition_controls_scalar(action, "action")
  if (!action %in% c("pause", "resume", "retry", "cancel")) {
    acquisition_controls_abort("{.arg action} must be pause, resume, retry, or cancel.")
  }
  target_type <- acquisition_controls_target_type(target_type)
  target_id <- acquisition_controls_scalar(target_id, "target_id")
  reason <- acquisition_controls_optional(reason, "reason")
  actor <- acquisition_controls_admin_actor(actor_resolver, request)
  acquisition_controls_ensure_schema(catalog)
  result <- acquisition_catalog_transaction(catalog, function(conn) {
    target <- acquisition_controls_target_jobs(conn, target_type, target_id)
    campaign_exists <- if (identical(target_type, "campaign")) {
      !is.null(acquisition_catalog_one(conn, "SELECT campaign_id FROM acquisition_campaigns WHERE campaign_id = ?", list(target_id)))
    } else {
      FALSE
    }
    if (nrow(target) == 0L && !campaign_exists) {
      acquisition_controls_abort("The requested acquisition control target does not exist.", class = "planscanR_error_acquisition_not_found")
    }
    switch(action,
      pause = acquisition_controls_pause_connection(conn, target_type, target_id, actor$id, reason),
      resume = acquisition_controls_resume_connection(conn, target_type, target_id, actor$id, reason),
      retry = acquisition_controls_retry_connection(conn, target_type, target_id, actor$id, reason),
      cancel = acquisition_controls_cancel_connection(conn, target_type, target_id, actor$id, reason)
    )
  })
  result$action <- action
  result
}

#' Pause a job or campaign after resolving an authenticated admin.
#' @inheritParams acquisition_control_apply
#' @export
acquisition_control_pause <- function(catalog, target_type, target_id, actor_resolver, request = list(), reason = NULL) {
  acquisition_control_apply(catalog, "pause", target_type, target_id, actor_resolver, request, reason)
}

#' Resume a job or campaign after resolving an authenticated admin.
#' @inheritParams acquisition_control_apply
#' @export
acquisition_control_resume <- function(catalog, target_type, target_id, actor_resolver, request = list(), reason = NULL) {
  acquisition_control_apply(catalog, "resume", target_type, target_id, actor_resolver, request, reason)
}

#' Retry eligible terminal work after resolving an authenticated admin.
#' @inheritParams acquisition_control_apply
#' @export
acquisition_control_retry <- function(catalog, target_type, target_id, actor_resolver, request = list(), reason = NULL) {
  acquisition_control_apply(catalog, "retry", target_type, target_id, actor_resolver, request, reason)
}

#' Cancel pending work or request a fenced safe stop for a running job.
#' @inheritParams acquisition_control_apply
#' @export
acquisition_control_cancel <- function(catalog, target_type, target_id, actor_resolver, request = list(), reason = NULL) {
  acquisition_control_apply(catalog, "cancel", target_type, target_id, actor_resolver, request, reason)
}

#' List audited acquisition control actions.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param target_type Optional `job` or `campaign` filter.
#' @param target_id Optional target filter.
#' @param action Optional action filter.
#' @param limit Maximum number of rows.
#' @return A data frame ordered newest first.
#' @export
acquisition_control_audit <- function(catalog, target_type = NULL, target_id = NULL, action = NULL, limit = 100L) {
  acquisition_controls_ensure_schema(catalog)
  limit <- as.integer(limit)
  if (length(limit) != 1L || is.na(limit) || limit < 1L || limit > 10000L) {
    acquisition_controls_abort("{.arg limit} must be an integer between 1 and 10000.")
  }
  clauses <- character(0)
  params <- list()
  if (!is.null(target_type)) {
    target_type <- acquisition_controls_target_type(target_type)
    clauses <- c(clauses, "target_type = ?")
    params <- c(params, list(target_type))
  }
  if (!is.null(target_id)) {
    target_id <- acquisition_controls_scalar(target_id, "target_id")
    clauses <- c(clauses, "target_id = ?")
    params <- c(params, list(target_id))
  }
  if (!is.null(action)) {
    action <- acquisition_controls_scalar(action, "action")
    if (!action %in% c("pause", "resume", "retry", "cancel")) {
      acquisition_controls_abort("{.arg action} is not a recognized control action.")
    }
    clauses <- c(clauses, "action = ?")
    params <- c(params, list(action))
  }
  where <- if (length(clauses) == 0L) "" else paste("WHERE", paste(clauses, collapse = " AND "))
  DBI::dbGetQuery(
    acquisition_catalog_connection(catalog),
    paste("SELECT * FROM acquisition_control_audit", where, "ORDER BY created_at DESC, audit_id DESC LIMIT", limit),
    params = params
  )
}

acquisition_controls_cancel_requested_connection <- function(conn, job) {
  campaign_id <- if ("campaign_id" %in% names(job)) job$campaign_id[[1L]] else NA_character_
  result <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT 1 AS requested FROM acquisition_control_flags",
      "WHERE active = 1 AND action = 'cancel' AND ((target_type = 'job' AND target_id = ?) OR",
      "(target_type = 'campaign' AND target_id = ?)) LIMIT 1"
    ),
    params = list(as.character(job$job_id[[1L]]), if (is.na(campaign_id)) "" else as.character(campaign_id))
  )
  nrow(result) == 1L
}

acquisition_controls_pause_guard_sql <- function() {
  paste(
    "NOT EXISTS (SELECT 1 FROM acquisition_control_flags f WHERE f.active = 1 AND",
    "f.action IN ('pause', 'cancel') AND ((f.target_type = 'job' AND f.target_id = j.job_id) OR",
    "(f.target_type = 'campaign' AND f.target_id = COALESCE(j.campaign_id, ''))))"
  )
}
