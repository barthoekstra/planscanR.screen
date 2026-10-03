# ACQ-2c: local storage metadata and reservation policy.
#
# Physical roots and safe path joins belong to planscanR.  This file keeps only
# the small catalog policy layer: registered roots, versioned settings,
# reservations, and durable relocation requests.

acquisition_storage_schema_version <- 1L

acquisition_storage_abort <- function(
  message,
  class = "planscanR_error_acquisition_storage",
  ...
) {
  acquisition_catalog_abort(message, class = unique(c(class, "planscanR_error_acquisition_storage")), ...)
}

acquisition_storage_require_admin <- function(actor) {
  if (!is.list(actor) || is.null(actor$id) || is.null(actor$roles) ||
    !any(tolower(as.character(actor$roles)) == "admin")) {
    acquisition_storage_abort(
      "An authenticated administrator is required for storage administration.",
      class = "planscanR_error_acquisition_storage_authorization"
    )
  }
  invisible(TRUE)
}

acquisition_storage_scalar <- function(value, arg) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
    !nzchar(trimws(value))) {
    acquisition_storage_abort(paste0("{.arg ", arg, "} must be one non-empty character string."))
  }
  trimws(value)
}

acquisition_storage_bytes <- function(value, arg = "bytes", allow_zero = TRUE) {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
    !is.finite(value) || value < if (allow_zero) 0 else 1 || value > 9e18) {
    acquisition_storage_abort(paste0("{.arg ", arg, "} must be a finite byte count."))
  }
  as.numeric(value)
}

acquisition_storage_fraction <- function(value, arg = "reserve_fraction") {
  if (!is.numeric(value) || length(value) != 1L || is.na(value) ||
    !is.finite(value) || value < 0 || value >= 1) {
    acquisition_storage_abort(paste0("{.arg ", arg, "} must be between 0 and 1."))
  }
  as.numeric(value)
}

acquisition_storage_root_path <- function(path, create = FALSE) {
  if (!requireNamespace("planscanR", quietly = TRUE) ||
    !exists("storage_root_validate", asNamespace("planscanR"), inherits = FALSE)) {
    acquisition_storage_abort(
      "The installed planscanR package lacks the required storage-root resolver.",
      class = "planscanR_error_acquisition_storage_dependency"
    )
  }
  planscanR::storage_root_validate(path, create = create, writable = TRUE)
}

acquisition_storage_capacity <- function(path) {
  if (!requireNamespace("planscanR", quietly = TRUE) ||
    !exists("storage_capacity", asNamespace("planscanR"), inherits = FALSE)) {
    acquisition_storage_abort(
      "The installed planscanR package lacks the required storage-capacity resolver.",
      class = "planscanR_error_acquisition_storage_dependency"
    )
  }
  planscanR::storage_capacity(path)
}

acquisition_storage_transfer_cap_bytes <- function() {
  if (!requireNamespace("planscanR", quietly = TRUE) ||
    !exists("storage_transfer_cap_bytes", asNamespace("planscanR"), inherits = FALSE)) {
    acquisition_storage_abort(
      "The installed planscanR package lacks the required transfer-cap resolver.",
      class = "planscanR_error_acquisition_storage_dependency"
    )
  }
  as.numeric(planscanR::storage_transfer_cap_bytes())
}

acquisition_storage_job_estimate <- function(job, conn = NULL) {
  value <- if (is.list(job)) job$estimated_bytes else NULL
  if (is.null(value) || length(value) == 0L || is.na(value[[1L]]) || value[[1L]] <= 0) {
    if (!is.null(conn)) {
      setting <- acquisition_storage_setting_value_connection(conn, "default_estimate_bytes")
      value <- setting %||% 0
    } else {
      value <- 0
    }
  }
  acquisition_storage_bytes(as.numeric(value), "estimated_bytes")
}

acquisition_storage_setting_value_connection <- function(conn, key, default = NULL) {
  row <- acquisition_catalog_one(
    conn,
    paste(
      "SELECT value_blob FROM acquisition_settings_versions",
      "WHERE setting_key = ? ORDER BY version DESC LIMIT 1"
    ),
    list(key)
  )
  if (is.null(row)) {
    return(default)
  }
  value <- acquisition_queue_unblob(row$value_blob[[1L]])
  if (is.null(value)) default else value
}

acquisition_storage_create_job_table <- function(conn) {
  fields <- DBI::dbListFields(conn, "acquisition_jobs_old")
  # ACQ-2a owns migration 4 and adds these nullable campaign columns. Keep
  # them when this storage migration has to rebuild the queue CHECK constraint.
  campaign_columns <- intersect(
    c("campaign_id", "campaign_item_key", "priority_rule_id", "priority_explanation"),
    fields
  )
  optional_sql <- if (length(campaign_columns) > 0L) {
    paste(paste0(campaign_columns, " TEXT"), collapse = ", ")
  } else {
    ""
  }
  estimated_sql <- if (length(campaign_columns) > 0L) {
    "estimated_bytes REAL NOT NULL DEFAULT 0 CHECK (estimated_bytes >= 0),"
  } else {
    "estimated_bytes REAL NOT NULL DEFAULT 0 CHECK (estimated_bytes >= 0)"
  }
  DBI::dbExecute(
    conn,
    paste0(
      "CREATE TABLE acquisition_jobs_new (",
      "job_id TEXT PRIMARY KEY,",
      "kind TEXT NOT NULL CHECK (kind IN ('discover', 'download', 'migrate_storage')),",
      "target_type TEXT NOT NULL CHECK (target_type IN ('source', 'document', 'file', 'storage_root')),",
      "target_ref_id TEXT NOT NULL,",
      "idempotency_key TEXT NOT NULL UNIQUE,",
      "host TEXT,",
      "priority INTEGER NOT NULL DEFAULT 0,",
      "state TEXT NOT NULL CHECK (state IN ('queued', 'running', 'retry_wait', 'blocked', 'succeeded', 'failed', 'cancelled')),",
      "available_at TEXT NOT NULL,",
      "attempt_count INTEGER NOT NULL DEFAULT 0 CHECK (attempt_count >= 0),",
      "max_attempts INTEGER NOT NULL CHECK (max_attempts >= 1),",
      "lease_token TEXT,",
      "worker_id TEXT,",
      "lease_expires_at TEXT,",
      "last_error_category TEXT,",
      "last_error TEXT,",
      "completed_at TEXT,",
      "created_at TEXT NOT NULL,",
      "updated_at TEXT NOT NULL,",
      estimated_sql,
      optional_sql,
      ")"
    )
  )
  columns <- c(
    "job_id", "kind", "target_type", "target_ref_id", "idempotency_key", "host",
    "priority", "state", "available_at", "attempt_count", "max_attempts", "lease_token",
    "worker_id", "lease_expires_at", "last_error_category", "last_error", "completed_at",
    "created_at", "updated_at", "estimated_bytes", campaign_columns
  )
  select_fields <- vapply(columns, function(column) {
    if (column %in% fields) column else if (identical(column, "estimated_bytes")) "0" else "NULL"
  }, character(1))
  DBI::dbExecute(
    conn,
    paste0(
      "INSERT INTO acquisition_jobs_new (", paste(columns, collapse = ", "), ") SELECT ",
      paste(select_fields, collapse = ", "), " FROM acquisition_jobs_old"
    )
  )
  invisible(TRUE)
}

acquisition_storage_rebuild_queue <- function(conn) {
  sql <- DBI::dbGetQuery(
    conn,
    "SELECT sql FROM sqlite_master WHERE type = 'table' AND name = 'acquisition_jobs'"
  )$sql
  target_index <- DBI::dbGetQuery(
    conn,
    "SELECT name FROM sqlite_master WHERE type = 'index' AND name = 'acquisition_jobs_target_idx'"
  )
  if (length(sql) == 1L && grepl("migrate_storage", sql, fixed = TRUE) &&
    grepl("storage_root", sql, fixed = TRUE) &&
    "estimated_bytes" %in% DBI::dbListFields(conn, "acquisition_jobs") &&
    nrow(target_index) == 1L &&
    !grepl("UNIQUE[[:space:]]*\\([[:space:]]*kind[[:space:]]*,[[:space:]]*target_type[[:space:]]*,[[:space:]]*target_ref_id", sql)) {
    return(invisible(FALSE))
  }
  DBI::dbExecute(conn, "DROP INDEX IF EXISTS acquisition_jobs_claim_idx")
  DBI::dbExecute(conn, "DROP INDEX IF EXISTS acquisition_jobs_host_idx")
  DBI::dbExecute(conn, "DROP INDEX IF EXISTS acquisition_jobs_campaign_idx")
  DBI::dbExecute(conn, "DROP INDEX IF EXISTS acquisition_jobs_target_idx")
  DBI::dbExecute(conn, "DROP INDEX IF EXISTS acquisition_job_attempts_token_idx")
  DBI::dbExecute(conn, "ALTER TABLE acquisition_job_attempts RENAME TO acquisition_job_attempts_old")
  DBI::dbExecute(conn, "ALTER TABLE acquisition_jobs RENAME TO acquisition_jobs_old")
  acquisition_storage_create_job_table(conn)
  # Make the replacement parent table visible before creating its child table;
  # SQLite otherwise validates the child foreign key against a missing parent
  # while the old table names are still present.
  DBI::dbExecute(conn, "ALTER TABLE acquisition_jobs_new RENAME TO acquisition_jobs")
  DBI::dbExecute(
    conn,
    paste(
      "CREATE TABLE acquisition_job_attempts_new (",
      "job_id TEXT NOT NULL, attempt_no INTEGER NOT NULL CHECK (attempt_no >= 1),",
      "lease_token TEXT NOT NULL UNIQUE, worker_id TEXT NOT NULL, host TEXT,",
      "claimed_at TEXT NOT NULL, lease_expires_at TEXT NOT NULL, heartbeat_at TEXT NOT NULL,",
      "finished_at TEXT, outcome TEXT CHECK (outcome IN ('running', 'expired', 'succeeded', 'retryable', 'blocked', 'failed', 'cancelled')),",
      "error_category TEXT, error_message TEXT, retry_at TEXT, bytes INTEGER, sha256 TEXT,",
      "local_path TEXT, result_blob TEXT, PRIMARY KEY (job_id, attempt_no),",
      "FOREIGN KEY (job_id) REFERENCES acquisition_jobs(job_id))"
    )
  )
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO acquisition_job_attempts_new (job_id, attempt_no, lease_token, worker_id, host,",
      "claimed_at, lease_expires_at, heartbeat_at, finished_at, outcome, error_category,",
      "error_message, retry_at, bytes, sha256, local_path, result_blob)",
      "SELECT job_id, attempt_no, lease_token, worker_id, host, claimed_at, lease_expires_at,",
      "heartbeat_at, finished_at, outcome, error_category, error_message, retry_at, bytes,",
      "sha256, local_path, result_blob FROM acquisition_job_attempts_old"
    )
  )
  DBI::dbExecute(conn, "DROP TABLE acquisition_job_attempts_old")
  DBI::dbExecute(conn, "DROP TABLE acquisition_jobs_old")
  DBI::dbExecute(conn, "ALTER TABLE acquisition_job_attempts_new RENAME TO acquisition_job_attempts")
  DBI::dbExecute(conn, "CREATE INDEX acquisition_jobs_claim_idx ON acquisition_jobs (state, available_at, priority, created_at, job_id)")
  DBI::dbExecute(conn, "CREATE INDEX acquisition_jobs_host_idx ON acquisition_jobs (host, state, lease_expires_at)")
  DBI::dbExecute(
    conn,
    paste(
      "CREATE UNIQUE INDEX acquisition_jobs_target_idx ON acquisition_jobs",
      "(kind, target_type, target_ref_id) WHERE kind IN ('discover', 'download')"
    )
  )
  DBI::dbExecute(conn, "CREATE INDEX acquisition_job_attempts_token_idx ON acquisition_job_attempts (lease_token)")
  if ("campaign_id" %in% DBI::dbListFields(conn, "acquisition_jobs")) {
    DBI::dbExecute(conn, "CREATE INDEX acquisition_jobs_campaign_idx ON acquisition_jobs (campaign_id, state, priority)")
  }
  invisible(TRUE)
}

acquisition_storage_apply_migration <- function(conn, version) {
  if (!identical(version, 1L)) {
    acquisition_storage_abort(paste0("Unknown acquisition storage migration: ", version))
  }
  DBI::dbExecute(
    conn,
    paste(
      "CREATE TABLE IF NOT EXISTS acquisition_storage_schema_migrations (",
      "version INTEGER PRIMARY KEY, applied_at TEXT NOT NULL)"
    )
  )
  DBI::dbExecute(
    conn,
    paste(
      "CREATE TABLE IF NOT EXISTS acquisition_storage_roots (",
      "root_id TEXT PRIMARY KEY, name TEXT NOT NULL UNIQUE, purpose TEXT NOT NULL,",
      "root_path TEXT NOT NULL, relative_policy TEXT NOT NULL CHECK (relative_policy = 'portable'),",
      "reserve_bytes REAL NOT NULL DEFAULT 0 CHECK (reserve_bytes >= 0),",
      "reserve_fraction REAL NOT NULL DEFAULT 0 CHECK (reserve_fraction >= 0 AND reserve_fraction < 1),",
      "active INTEGER NOT NULL DEFAULT 1 CHECK (active IN (0, 1)),",
      "created_by TEXT, created_at TEXT NOT NULL, validated_at TEXT NOT NULL, updated_at TEXT NOT NULL)"
    )
  )
  DBI::dbExecute(
    conn,
    paste(
      "CREATE TABLE IF NOT EXISTS acquisition_settings_versions (",
      "setting_key TEXT NOT NULL, version INTEGER NOT NULL, value_blob TEXT NOT NULL,",
      "changed_by TEXT NOT NULL, changed_at TEXT NOT NULL,",
      "PRIMARY KEY (setting_key, version))"
    )
  )
  # Rebuild the queue before creating storage tables that refer to it. SQLite
  # otherwise rewrites those foreign keys to the temporary table name.
  if (!"estimated_bytes" %in% DBI::dbListFields(conn, "acquisition_jobs")) {
    DBI::dbExecute(conn, "ALTER TABLE acquisition_jobs ADD COLUMN estimated_bytes REAL NOT NULL DEFAULT 0")
  }
  acquisition_storage_rebuild_queue(conn)
  DBI::dbExecute(
    conn,
    paste(
      "CREATE TABLE IF NOT EXISTS acquisition_storage_reservations (",
      "reservation_id TEXT PRIMARY KEY, job_id TEXT NOT NULL UNIQUE, root_id TEXT NOT NULL,",
      "bytes REAL NOT NULL CHECK (bytes >= 0), state TEXT NOT NULL CHECK (state IN ('held', 'released')),",
      "created_at TEXT NOT NULL, released_at TEXT,",
      "FOREIGN KEY (root_id) REFERENCES acquisition_storage_roots(root_id),",
      "FOREIGN KEY (job_id) REFERENCES acquisition_jobs(job_id))"
    )
  )
  DBI::dbExecute(
    conn,
    paste(
      "CREATE TABLE IF NOT EXISTS acquisition_storage_relocations (",
      "relocation_id TEXT PRIMARY KEY, source_root_id TEXT NOT NULL, target_root_id TEXT NOT NULL,",
      "job_id TEXT UNIQUE, estimated_bytes REAL NOT NULL DEFAULT 0,",
      "state TEXT NOT NULL CHECK (state IN ('queued', 'running', 'succeeded', 'blocked', 'failed', 'cancelled')),",
      "requested_by TEXT NOT NULL, requested_at TEXT NOT NULL, updated_at TEXT NOT NULL,",
      "FOREIGN KEY (source_root_id) REFERENCES acquisition_storage_roots(root_id),",
      "FOREIGN KEY (target_root_id) REFERENCES acquisition_storage_roots(root_id))"
    )
  )
  DBI::dbExecute(
    conn,
    "INSERT OR IGNORE INTO acquisition_storage_schema_migrations(version, applied_at) VALUES (?, ?)",
    params = list(1L, acquisition_catalog_now())
  )
  invisible(TRUE)
}

# Called by acquisition_catalog_open() immediately after the numbered catalog
# migrations. Storage owns this separate migration ledger so ACQ-2a can keep
# ownership of catalog migration 4.
acquisition_storage_migrate <- function(conn, busy_timeout = 5000L) {
  existing <- if ("acquisition_storage_schema_migrations" %in% DBI::dbListTables(conn)) {
    DBI::dbGetQuery(conn, "SELECT version FROM acquisition_storage_schema_migrations ORDER BY version DESC LIMIT 1")
  } else {
    data.frame(version = integer(0))
  }
  if (nrow(existing) > 0L && as.integer(existing$version[[1L]]) > acquisition_storage_schema_version) {
    acquisition_storage_abort("The acquisition storage schema is newer than this package supports.", class = "planscanR_error_acquisition_schema")
  }
  if (nrow(existing) == 0L) {
    # Queue constraints need a SQLite table rebuild. SQLite only permits
    # changing this pragma outside a transaction, so disable FK checks before
    # the shared bounded transaction and restore them on every exit path.
    DBI::dbExecute(conn, "PRAGMA foreign_keys = OFF")
    on.exit(DBI::dbExecute(conn, "PRAGMA foreign_keys = ON"), add = TRUE)
    acquisition_catalog_transaction_connection(conn, function(connection) {
      acquisition_storage_apply_migration(connection, 1L)
      invisible(TRUE)
    }, busy_timeout = busy_timeout)
  }
  acquisition_storage_ensure_default_root_connection(conn)
  invisible(acquisition_storage_schema_version)
}

acquisition_storage_ensure_default_root_connection <- function(conn) {
  if (!requireNamespace("planscanR", quietly = TRUE) ||
    !exists("storage_root_validate", asNamespace("planscanR"), inherits = FALSE)) {
    return(invisible(FALSE))
  }
  existing <- acquisition_catalog_one(
    conn,
    "SELECT root_id FROM acquisition_storage_roots WHERE root_id = ?",
    list("planscan-cache")
  )
  if (is.null(existing)) {
    path <- tryCatch(planscanR::storage_root_validate(create = TRUE, writable = TRUE), error = function(error) NULL)
    if (!is.null(path)) {
      now <- acquisition_catalog_now()
      DBI::dbExecute(
        conn,
        paste(
          "INSERT OR IGNORE INTO acquisition_storage_roots (root_id, name, purpose, root_path,",
          "relative_policy, reserve_bytes, reserve_fraction, active, created_by, created_at, validated_at, updated_at)",
          "VALUES ('planscan-cache', 'planscan-cache', 'downloads', ?, 'portable', 0, 0.15, 1, 'system', ?, ?, ?)"
        ),
        params = list(path, now, now, now)
      )
    }
  }
  # Unknown-size built-in downloads reserve the leaf's hard transfer cap. An
  # administrator can replace this versioned setting for other job kinds.
  if (is.null(acquisition_storage_setting_value_connection(conn, "default_estimate_bytes"))) {
    now <- acquisition_catalog_now()
    DBI::dbExecute(
      conn,
      paste(
        "INSERT OR IGNORE INTO acquisition_settings_versions",
        "(setting_key, version, value_blob, changed_by, changed_at) VALUES (?, 1, ?, 'system', ?)"
      ),
      params = list("default_estimate_bytes", acquisition_catalog_blob(acquisition_storage_transfer_cap_bytes()), now)
    )
  }
  invisible(TRUE)
}

#' Register or update one validated local storage root.
#'
#' @param catalog Acquisition catalog.
#' @param root_id Stable catalog identifier.
#' @param path Absolute writable directory.
#' @param actor Authenticated administrator.
#' @param purpose Root purpose, normally `downloads` or `staging`.
#' @param reserve_bytes Fixed free-space headroom.
#' @param reserve_fraction Additional fraction of total filesystem capacity to keep free.
#' @return A list containing the root identifier and canonical path.
#' @export
acquisition_storage_register_root <- function(
  catalog,
  root_id,
  path,
  actor,
  purpose = "downloads",
  reserve_bytes = 0,
  reserve_fraction = 0
) {
  acquisition_storage_require_admin(actor)
  root_id <- acquisition_storage_scalar(root_id, "root_id")
  purpose <- acquisition_storage_scalar(purpose, "purpose")
  reserve_bytes <- acquisition_storage_bytes(reserve_bytes, "reserve_bytes")
  reserve_fraction <- acquisition_storage_fraction(reserve_fraction)
  path <- acquisition_storage_root_path(path, create = TRUE)
  acquisition_catalog_transaction(catalog, function(conn) {
    existing <- acquisition_catalog_one(
      conn,
      "SELECT root_path FROM acquisition_storage_roots WHERE root_id = ?",
      list(root_id)
    )
    if (!is.null(existing) && !identical(as.character(existing$root_path[[1L]]), path)) {
      acquisition_storage_abort(
        "A registered storage root ID cannot change physical paths; register a new root and queue a relocation.",
        class = "planscanR_error_acquisition_storage_immutable"
      )
    }
    now <- acquisition_catalog_now()
    DBI::dbExecute(
      conn,
      paste(
        "INSERT INTO acquisition_storage_roots (root_id, name, purpose, root_path, relative_policy,",
        "reserve_bytes, reserve_fraction, active, created_by, created_at, validated_at, updated_at)",
        "VALUES (?, ?, ?, ?, 'portable', ?, ?, 1, ?, ?, ?, ?) ON CONFLICT(root_id) DO UPDATE SET",
        "name = excluded.name, purpose = excluded.purpose, root_path = excluded.root_path,",
        "reserve_bytes = excluded.reserve_bytes, reserve_fraction = excluded.reserve_fraction,",
        "active = 1, validated_at = excluded.validated_at, updated_at = excluded.updated_at"
      ),
      params = list(root_id, root_id, purpose, path, reserve_bytes, reserve_fraction, actor$id, now, now, now)
    )
    list(root_id = root_id, path = path, status = "saved")
  })
}

#' List registered storage roots.
#' @param catalog Acquisition catalog.
#' @param active Optional logical filter.
#' @return A data frame of root metadata.
#' @export
acquisition_storage_roots <- function(catalog, active = NULL) {
  conn <- acquisition_catalog_connection(catalog)
  sql <- "SELECT * FROM acquisition_storage_roots"
  params <- list()
  if (!is.null(active)) {
    sql <- paste(sql, "WHERE active = ?")
    params <- list(as.integer(isTRUE(active)))
  }
  sql <- paste(sql, "ORDER BY root_id")
  if (length(params) == 0L) DBI::dbGetQuery(conn, sql) else DBI::dbGetQuery(conn, sql, params = params)
}

#' Append a versioned acquisition setting.
#' @param catalog Acquisition catalog.
#' @param key Setting key.
#' @param value Scalar or serializable setting value.
#' @param actor Authenticated administrator.
#' @return A list containing the new setting version.
#' @export
acquisition_storage_set <- function(catalog, key, value, actor) {
  acquisition_storage_require_admin(actor)
  key <- acquisition_storage_scalar(key, "key")
  if (grepl("secret|token|password|credential", tolower(key))) {
    acquisition_storage_abort("Secrets are not stored in acquisition settings.")
  }
  if (identical(key, "default_estimate_bytes")) value <- acquisition_storage_bytes(value, key)
  blob <- acquisition_catalog_blob(value)
  acquisition_catalog_transaction(catalog, function(conn) {
    current <- acquisition_catalog_one(
      conn,
      "SELECT MAX(version) AS version FROM acquisition_settings_versions WHERE setting_key = ?",
      list(key)
    )
    version <- if (is.null(current) || is.na(current$version[[1L]])) 1L else as.integer(current$version[[1L]]) + 1L
    now <- acquisition_catalog_now()
    DBI::dbExecute(
      conn,
      "INSERT INTO acquisition_settings_versions(setting_key, version, value_blob, changed_by, changed_at) VALUES (?, ?, ?, ?, ?)",
      params = list(key, version, blob, actor$id, now)
    )
    list(key = key, version = version, status = "saved")
  })
}

#' List versioned acquisition settings.
#' @param catalog Acquisition catalog.
#' @param key Optional setting key.
#' @return A data frame with a list-column `value`.
#' @export
acquisition_storage_settings <- function(catalog, key = NULL) {
  conn <- acquisition_catalog_connection(catalog)
  sql <- "SELECT * FROM acquisition_settings_versions"
  params <- list()
  if (!is.null(key)) {
    key <- acquisition_storage_scalar(key, "key")
    sql <- paste(sql, "WHERE setting_key = ?")
    params <- list(key)
  }
  sql <- paste(sql, "ORDER BY setting_key, version")
  rows <- if (length(params) == 0L) DBI::dbGetQuery(conn, sql) else DBI::dbGetQuery(conn, sql, params = params)
  if (nrow(rows) > 0L) rows$value <- lapply(rows$value_blob, acquisition_queue_unblob)
  rows
}

acquisition_storage_active_root_connection <- function(conn, purpose = "downloads") {
  row <- acquisition_catalog_one(
    conn,
    paste(
      "SELECT * FROM acquisition_storage_roots WHERE active = 1 AND purpose = ?",
      "ORDER BY root_id LIMIT 1"
    ),
    list(purpose)
  )
  if (is.null(row)) {
    row <- acquisition_catalog_one(
      conn,
      "SELECT * FROM acquisition_storage_roots WHERE active = 1 ORDER BY root_id LIMIT 1"
    )
  }
  row
}

acquisition_storage_root_for_job_connection <- function(conn, job) {
  if (identical(as.character(job$target_type), "storage_root")) {
    return(acquisition_catalog_one(conn, "SELECT * FROM acquisition_storage_roots WHERE root_id = ? AND active = 1", list(as.character(job$target_ref_id))))
  }
  acquisition_storage_active_root_connection(conn, purpose = "downloads")
}

acquisition_storage_try_reserve_connection <- function(conn, job) {
  if (!identical(as.character(job$kind), "download") && !identical(as.character(job$kind), "migrate_storage")) {
    return(list(ok = TRUE, skipped = TRUE))
  }
  root <- acquisition_storage_root_for_job_connection(conn, job)
  if (is.null(root)) {
    return(list(ok = TRUE, skipped = TRUE))
  }
  bytes <- acquisition_storage_job_estimate(job, conn)
  if (identical(as.character(job$kind), "download")) {
    bytes <- max(bytes, acquisition_storage_transfer_cap_bytes())
  }
  capacity <- acquisition_storage_capacity(as.character(root$root_path[[1L]]))
  available <- as.numeric(capacity$free_bytes)
  total <- as.numeric(capacity$total_bytes)
  held <- DBI::dbGetQuery(
    conn,
    "SELECT COALESCE(SUM(bytes), 0) AS bytes FROM acquisition_storage_reservations WHERE state = 'held'"
  )$bytes[[1L]]
  reserve <- max(
    as.numeric(root$reserve_bytes[[1L]]),
    total * as.numeric(root$reserve_fraction[[1L]])
  )
  if (available - as.numeric(held) - bytes < reserve) {
    return(list(ok = FALSE, reason = "disk-reserve-exhausted", root_id = as.character(root$root_id[[1L]]), root_path = as.character(root$root_path[[1L]]), available_bytes = available, reserved_bytes = held, requested_bytes = bytes))
  }
  now <- acquisition_catalog_now()
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO acquisition_storage_reservations (reservation_id, job_id, root_id, bytes, state, created_at)",
      "VALUES (?, ?, ?, ?, 'held', ?) ON CONFLICT(job_id) DO UPDATE SET root_id = excluded.root_id,",
      "bytes = excluded.bytes, state = 'held', created_at = excluded.created_at, released_at = NULL"
    ),
    params = list(paste0("reservation-", as.character(job$job_id[[1L]])), as.character(job$job_id[[1L]]), as.character(root$root_id[[1L]]), bytes, now)
  )
  list(ok = TRUE, root_id = as.character(root$root_id[[1L]]), root_path = as.character(root$root_path[[1L]]), bytes = bytes, available_bytes = available, reserve_bytes = reserve)
}

acquisition_storage_release_connection <- function(conn, job_id) {
  DBI::dbExecute(
    conn,
    "UPDATE acquisition_storage_reservations SET state = 'released', released_at = ? WHERE job_id = ? AND state = 'held'",
    params = list(acquisition_catalog_now(), as.character(job_id))
  )
  invisible(TRUE)
}

#' List disk reservations.
#' @param catalog Acquisition catalog.
#' @param state Optional `held` or `released` filter.
#' @return A data frame of reservations.
#' @export
acquisition_storage_reservations <- function(catalog, state = NULL) {
  conn <- acquisition_catalog_connection(catalog)
  sql <- "SELECT * FROM acquisition_storage_reservations"
  params <- list()
  if (!is.null(state)) {
    state <- acquisition_storage_scalar(state, "state")
    if (!state %in% c("held", "released")) acquisition_storage_abort("{.arg state} must be `held` or `released`.")
    sql <- paste(sql, "WHERE state = ?")
    params <- list(state)
  }
  sql <- paste(sql, "ORDER BY created_at, reservation_id")
  if (length(params) == 0L) DBI::dbGetQuery(conn, sql) else DBI::dbGetQuery(conn, sql, params = params)
}

#' Request an asynchronous storage relocation.
#'
#' The operation only records a relocation and queues `migrate_storage`; the
#' worker must perform copy, verification, reference switch, and retention
#' policy explicitly. No file is moved by this function.
#'
#' @param catalog Acquisition catalog.
#' @param source_root_id Existing source root.
#' @param target_root_id Existing target root.
#' @param actor Authenticated administrator.
#' @param estimated_bytes Expected temporary copy size.
#' @return A list containing `relocation_id` and `job_id`.
#' @export
acquisition_storage_enqueue_relocation <- function(
  catalog,
  source_root_id,
  target_root_id,
  actor,
  estimated_bytes = 0
) {
  acquisition_storage_require_admin(actor)
  source_root_id <- acquisition_storage_scalar(source_root_id, "source_root_id")
  target_root_id <- acquisition_storage_scalar(target_root_id, "target_root_id")
  if (identical(source_root_id, target_root_id)) acquisition_storage_abort("Source and target storage roots must differ.")
  estimated_bytes <- acquisition_storage_bytes(estimated_bytes, "estimated_bytes")
  acquisition_catalog_transaction(catalog, function(conn) {
    source <- acquisition_catalog_one(conn, "SELECT root_id FROM acquisition_storage_roots WHERE root_id = ? AND active = 1", list(source_root_id))
    target <- acquisition_catalog_one(conn, "SELECT root_id FROM acquisition_storage_roots WHERE root_id = ? AND active = 1", list(target_root_id))
    if (is.null(source) || is.null(target)) acquisition_storage_abort("Both relocation roots must be registered and active.")
    relocation_id <- acquisition_catalog_id("relocation", source_root_id, target_root_id)
    now <- acquisition_catalog_now()
    DBI::dbExecute(
      conn,
      paste(
        "INSERT OR IGNORE INTO acquisition_storage_relocations (relocation_id, source_root_id, target_root_id,",
        "estimated_bytes, state, requested_by, requested_at, updated_at) VALUES (?, ?, ?, ?, 'queued', ?, ?, ?)"
      ),
      params = list(relocation_id, source_root_id, target_root_id, estimated_bytes, actor$id, now, now)
    )
    queued <- acquisition_queue_enqueue_connection(
      conn,
      kind = "migrate_storage",
      target_type = "storage_root",
      target_ref_id = target_root_id,
      priority = 0L,
      max_attempts = 3L,
      idempotency_key = paste0("relocation:", relocation_id),
      estimated_bytes = estimated_bytes
    )
    DBI::dbExecute(
      conn,
      "UPDATE acquisition_storage_relocations SET job_id = ?, updated_at = ? WHERE relocation_id = ?",
      params = list(queued$job_id, now, relocation_id)
    )
    list(relocation_id = relocation_id, job_id = queued$job_id, status = queued$status)
  })
}

#' List durable storage relocation requests.
#' @param catalog Acquisition catalog.
#' @param state Optional relocation state.
#' @return A data frame of relocation requests.
#' @export
acquisition_storage_relocations <- function(catalog, state = NULL) {
  conn <- acquisition_catalog_connection(catalog)
  sql <- "SELECT * FROM acquisition_storage_relocations"
  params <- list()
  if (!is.null(state)) {
    state <- acquisition_storage_scalar(state, "state")
    sql <- paste(sql, "WHERE state = ?")
    params <- list(state)
  }
  sql <- paste(sql, "ORDER BY requested_at, relocation_id")
  if (length(params) == 0L) DBI::dbGetQuery(conn, sql) else DBI::dbGetQuery(conn, sql, params = params)
}

acquisition_storage_mark_running_connection <- function(conn, job_id) {
  DBI::dbExecute(
    conn,
    "UPDATE acquisition_storage_relocations SET state = 'running', updated_at = ? WHERE job_id = ? AND state = 'queued'",
    params = list(acquisition_catalog_now(), as.character(job_id))
  )
  invisible(TRUE)
}

acquisition_storage_mark_complete_connection <- function(conn, job_id, state) {
  if (!state %in% c("succeeded", "blocked", "failed", "cancelled", "retry_wait")) {
    return(invisible(TRUE))
  }
  if (identical(state, "retry_wait")) state <- "queued"
  DBI::dbExecute(
    conn,
    "UPDATE acquisition_storage_relocations SET state = ?, updated_at = ? WHERE job_id = ?",
    params = list(state, acquisition_catalog_now(), as.character(job_id))
  )
  invisible(TRUE)
}
