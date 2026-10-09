# ACQ-2c: local storage metadata and reservation policy.
#
# Physical roots and safe path joins belong to planscanR.  This file keeps only
# the small catalog policy layer: registered roots, versioned settings,
# reservations, and durable relocation requests.

acquisition_storage_schema_version <- 2L

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
      "kind TEXT NOT NULL CHECK (kind IN ('discover', 'download', 'migrate_storage', 'ingest')),",
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
    grepl("'ingest'", sql, fixed = TRUE) &&
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

acquisition_storage_rebuild_queue_dependents <- function(conn) {
  if ("acquisition_storage_reservations" %in% DBI::dbListTables(conn)) {
    DBI::dbExecute(conn, "ALTER TABLE acquisition_storage_reservations RENAME TO acquisition_storage_reservations_old")
    old_fields <- DBI::dbListFields(conn, "acquisition_storage_reservations_old")
    DBI::dbExecute(conn, paste(
      "CREATE TABLE acquisition_storage_reservations (",
      "reservation_id TEXT PRIMARY KEY, job_id TEXT NOT NULL UNIQUE, root_id TEXT NOT NULL,",
      "bytes REAL NOT NULL CHECK (bytes >= 0), state TEXT NOT NULL CHECK (state IN ('held', 'released')),",
      "created_at TEXT NOT NULL, released_at TEXT, lease_token TEXT,",
      "written_bytes REAL NOT NULL DEFAULT 0 CHECK (written_bytes >= 0),",
      "pending_bytes REAL NOT NULL DEFAULT 0 CHECK (pending_bytes >= 0),",
      "advertised_bytes REAL, phase TEXT NOT NULL DEFAULT 'stream' CHECK (phase IN ('stream', 'promotion')),",
      "FOREIGN KEY (root_id) REFERENCES acquisition_storage_roots(root_id),",
      "FOREIGN KEY (job_id) REFERENCES acquisition_jobs(job_id))"
    ))
    fields <- c(
      "reservation_id", "job_id", "root_id", "bytes", "state", "created_at", "released_at",
      "lease_token", "written_bytes", "pending_bytes", "advertised_bytes", "phase"
    )
    expressions <- vapply(fields, function(field) {
      if (field %in% old_fields) field else switch(field,
        lease_token = "NULL",
        written_bytes = "0",
        pending_bytes = "0",
        advertised_bytes = "NULL",
        phase = "'stream'",
        "NULL"
      )
    }, character(1))
    DBI::dbExecute(conn, paste(
      "INSERT INTO acquisition_storage_reservations (", paste(fields, collapse = ", "), ")",
      "SELECT", paste(expressions, collapse = ", "), "FROM acquisition_storage_reservations_old"
    ))
    DBI::dbExecute(conn, "DROP TABLE acquisition_storage_reservations_old")
  }
  if ("acquisition_control_pauses" %in% DBI::dbListTables(conn)) {
    DBI::dbExecute(conn, "ALTER TABLE acquisition_control_pauses RENAME TO acquisition_control_pauses_old")
    DBI::dbExecute(conn, paste(
      "CREATE TABLE acquisition_control_pauses (",
      "job_id TEXT NOT NULL, target_type TEXT NOT NULL CHECK (target_type IN ('job', 'campaign')),",
      "target_id TEXT NOT NULL, previous_state TEXT NOT NULL, previous_available_at TEXT NOT NULL,",
      "paused_at TEXT NOT NULL, PRIMARY KEY (job_id, target_type, target_id),",
      "FOREIGN KEY (job_id) REFERENCES acquisition_jobs(job_id))"
    ))
    DBI::dbExecute(conn, paste(
      "INSERT INTO acquisition_control_pauses",
      "SELECT job_id, target_type, target_id, previous_state, previous_available_at, paused_at",
      "FROM acquisition_control_pauses_old"
    ))
    DBI::dbExecute(conn, "DROP TABLE acquisition_control_pauses_old")
  }
  invisible(TRUE)
}

acquisition_storage_upgrade_ingest_queue <- function(conn, busy_timeout) {
  sql <- DBI::dbGetQuery(
    conn,
    "SELECT sql FROM sqlite_master WHERE type = 'table' AND name = 'acquisition_jobs'"
  )$sql
  if (length(sql) != 1L || grepl("'ingest'", sql, fixed = TRUE)) {
    return(invisible(FALSE))
  }
  old_foreign_keys <- as.integer(DBI::dbGetQuery(conn, "PRAGMA foreign_keys")[[1L]]) == 1L
  DBI::dbExecute(conn, "PRAGMA foreign_keys = OFF")
  on.exit(DBI::dbExecute(conn, paste0("PRAGMA foreign_keys = ", as.integer(old_foreign_keys))), add = TRUE)
  acquisition_catalog_transaction_connection(conn, function(connection) {
    acquisition_storage_rebuild_queue(connection)
    acquisition_storage_rebuild_queue_dependents(connection)
    violations <- DBI::dbGetQuery(connection, "PRAGMA foreign_key_check")
    if (nrow(violations) > 0L) {
      acquisition_storage_abort("The ingestion queue migration would leave broken foreign keys.", class = "planscanR_error_acquisition_schema")
    }
    invisible(TRUE)
  }, busy_timeout = busy_timeout)
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
      "created_at TEXT NOT NULL, released_at TEXT, lease_token TEXT,",
      "written_bytes REAL NOT NULL DEFAULT 0 CHECK (written_bytes >= 0),",
      "pending_bytes REAL NOT NULL DEFAULT 0 CHECK (pending_bytes >= 0),",
      "advertised_bytes REAL, phase TEXT NOT NULL DEFAULT 'stream' CHECK (phase IN ('stream', 'promotion')),",
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
  acquisition_storage_upgrade_ingest_queue(conn, busy_timeout)
  acquisition_storage_upgrade_reservation_schema(conn, busy_timeout)
  acquisition_storage_ensure_default_root_connection(conn)
  invisible(acquisition_storage_schema_version)
}

acquisition_storage_upgrade_reservation_schema <- function(conn, busy_timeout = 5000L) {
  # Keep this additive for catalogs created by ACQ-2c before dynamic download
  # reservations existed. SQLite cannot add a CHECK constraint in place, so
  # the new fields are deliberately nullable/defaulted and validated by the
  # coordinator transaction below.
  additions <- list(
    lease_token = "ALTER TABLE acquisition_storage_reservations ADD COLUMN lease_token TEXT",
    written_bytes = "ALTER TABLE acquisition_storage_reservations ADD COLUMN written_bytes REAL NOT NULL DEFAULT 0",
    pending_bytes = "ALTER TABLE acquisition_storage_reservations ADD COLUMN pending_bytes REAL NOT NULL DEFAULT 0",
    advertised_bytes = "ALTER TABLE acquisition_storage_reservations ADD COLUMN advertised_bytes REAL",
    phase = "ALTER TABLE acquisition_storage_reservations ADD COLUMN phase TEXT NOT NULL DEFAULT 'stream'"
  )
  acquisition_catalog_transaction_connection(conn, function(connection) {
    fields <- DBI::dbListFields(connection, "acquisition_storage_reservations")
    missing <- names(additions)[!names(additions) %in% fields]
    if (length(missing) > 0L) {
      for (field in missing) DBI::dbExecute(connection, additions[[field]])
      if ("acquisition_storage_schema_migrations" %in% DBI::dbListTables(connection)) {
        DBI::dbExecute(
          connection,
          "INSERT OR IGNORE INTO acquisition_storage_schema_migrations(version, applied_at) VALUES (?, ?)",
          params = list(2L, acquisition_catalog_now())
        )
      }
    } else if ("acquisition_storage_schema_migrations" %in% DBI::dbListTables(connection)) {
      DBI::dbExecute(
        connection,
        "INSERT OR IGNORE INTO acquisition_storage_schema_migrations(version, applied_at) VALUES (?, ?)",
        params = list(2L, acquisition_catalog_now())
      )
    }
    invisible(TRUE)
  }, busy_timeout = busy_timeout)
  invisible(TRUE)
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
  # Keep a finite legacy estimate for storage jobs that do not carry their own
  # size. Download reservations use the live free-space headroom below instead
  # of treating this setting (or the leaf's historical 50 MiB option) as a
  # per-file ceiling.
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

#' Resolve a catalog storage location on this machine
#'
#' Portable catalog locations contain only a stable storage-root ID and a
#' relative path. This helper supplies the machine-local root paths to the
#' planscanR containment resolver. Legacy absolute and cache-relative paths are
#' also accepted while older sidecars and indexes are being adopted.
#'
#' @param catalog Acquisition catalog.
#' @param location Portable location list or legacy character path.
#' @param must_exist Require the target to exist.
#' @return Canonical absolute path below a registered root.
#' @export
acquisition_storage_resolve_location <- function(catalog, location,
                                                 must_exist = FALSE) {
  conn <- acquisition_catalog_connection(catalog)
  roots <- DBI::dbGetQuery(
    conn,
    "SELECT root_id, root_path FROM acquisition_storage_roots"
  )
  root_paths <- stats::setNames(as.character(roots$root_path), roots$root_id)
  planscanR::storage_resolve_location(
    location,
    roots = root_paths,
    must_exist = must_exist
  )
}

acquisition_storage_legacy_location <- function(catalog, path) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(NULL)
  }
  roots <- acquisition_storage_roots(catalog)
  if (grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)) {
    for (i in seq_len(nrow(roots))) {
      relative <- tryCatch(
        planscanR::storage_relative_path(path, roots$root_path[[i]]),
        error = function(error) NULL
      )
      if (!is.null(relative)) {
        return(list(storage_root_id = as.character(roots$root_id[[i]]), relative_path = relative))
      }
    }
    cache_root <- tryCatch(planscanR::cache_dir_default(), error = function(error) NULL)
    if (!is.null(cache_root)) {
      relative <- tryCatch(planscanR::storage_relative_path(path, cache_root), error = function(error) NULL)
      if (!is.null(relative)) return(list(storage_root_id = "cache", relative_path = relative))
    }
    return(NULL)
  }
  # The legacy sidecar contract stores cache-root-relative paths. Validate
  # them before including them in a portable manifest.
  relative <- tryCatch(
    planscanR::storage_relative_path(path, planscanR::cache_dir_default()),
    error = function(error) NULL
  )
  if (is.null(relative)) NULL else list(storage_root_id = "cache", relative_path = relative)
}

#' Write a portable manifest for one catalog source/project
#'
#' The current catalog models a project as its stable `source_ref_id`; it does
#' not create a second project UUID. Each logical attachment keeps its
#' `document_ref_id`, and each registered file claim is emitted as a version
#' claim identified by its actual `file_ref_id` and content SHA-256. The
#' manifest contains no machine-specific absolute storage paths.
#'
#' @param catalog Acquisition catalog.
#' @param source_ref_id Stable source/project ID.
#' @param path Optional output path. Defaults to
#'   `projects/<source_ref_id>/manifest.json` beside the catalog database.
#' @return The manifest list, invisibly carrying its written path as an
#'   attribute.
#' @export
acquisition_catalog_write_project_manifest <- function(catalog, source_ref_id,
                                                       path = NULL) {
  conn <- acquisition_catalog_connection(catalog)
  source_ref_id <- acquisition_catalog_scalar_character(source_ref_id, "source_ref_id")
  source <- acquisition_catalog_one(
    conn,
    "SELECT * FROM acquisition_sources WHERE source_ref_id = ?",
    list(source_ref_id)
  )
  if (is.null(source)) {
    acquisition_catalog_abort("The source/project ID is not present in the catalog.")
  }
  rows <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT d.document_ref_id, d.native_attachment_id, d.attachment_key,",
      "d.attachment_url, d.section, d.ordinal, d.identity_status, d.legacy_local_path,",
      "f.file_ref_id, f.root_id, f.relative_path, f.sha256, f.size_bytes,",
      "f.verification_status, f.provenance, f.observed_at, f.legacy_path",
      "FROM acquisition_documents d LEFT JOIN acquisition_files f",
      "ON f.document_ref_id = d.document_ref_id WHERE d.source_ref_id = ?",
      "ORDER BY d.ordinal, d.document_ref_id, f.observed_at DESC, f.file_ref_id"
    ),
    params = list(source_ref_id)
  )
  document_ids <- unique(as.character(rows$document_ref_id))
  documents <- lapply(document_ids, function(document_id) {
    doc_rows <- rows[rows$document_ref_id == document_id, , drop = FALSE]
    first <- doc_rows[1L, , drop = FALSE]
    preferred_sha <- vapply(seq_len(nrow(doc_rows)), function(i) {
      sha <- as.character(doc_rows$sha256[[i]])
      if (is.na(sha) || !identical(doc_rows$verification_status[[i]], "verified")) return(FALSE)
      !any(
        seq_len(i - 1L) > 0L &
          !is.na(doc_rows$sha256[seq_len(i - 1L)]) &
          tolower(doc_rows$sha256[seq_len(i - 1L)]) == tolower(sha) &
          doc_rows$verification_status[seq_len(i - 1L)] == "verified"
      )
    }, logical(1))
    versions <- lapply(which(!is.na(doc_rows$file_ref_id)), function(i) {
      row <- doc_rows[i, , drop = FALSE]
      list(
        version_id = as.character(row$file_ref_id[[1L]]),
        sha256 = if (is.na(row$sha256[[1L]])) NULL else as.character(row$sha256[[1L]]),
        size_bytes = if (is.na(row$size_bytes[[1L]])) NULL else as.numeric(row$size_bytes[[1L]]),
        observed_at = as.character(row$observed_at[[1L]]),
        preferred_location = isTRUE(preferred_sha[[i]]),
        verification_status = as.character(row$verification_status[[1L]]),
        provenance = as.character(row$provenance[[1L]]),
        original_filename = basename(if (!is.na(row$legacy_path[[1L]])) {
          as.character(row$legacy_path[[1L]])
        } else if (!is.na(first$legacy_local_path[[1L]])) {
          as.character(first$legacy_local_path[[1L]])
        } else {
          as.character(row$relative_path[[1L]])
        }),
        location = list(
          storage_root_id = as.character(row$root_id[[1L]]),
          relative_path = as.character(row$relative_path[[1L]])
        )
      )
    })
    list(
      document_id = as.character(first$document_ref_id[[1L]]),
      native_attachment_id = if (is.na(first$native_attachment_id[[1L]])) NULL else as.character(first$native_attachment_id[[1L]]),
      attachment_key = as.character(first$attachment_key[[1L]]),
      attachment_url = if (is.na(first$attachment_url[[1L]])) NULL else as.character(first$attachment_url[[1L]]),
      section = if (is.na(first$section[[1L]])) NULL else as.character(first$section[[1L]]),
      identity_status = as.character(first$identity_status[[1L]]),
      legacy_location = acquisition_storage_legacy_location(
        catalog, as.character(first$legacy_local_path[[1L]])
      ),
      versions = versions
    )
  })
  manifest <- list(
    schema = "planscanR.acquisition.project-manifest.v1",
    manifest_version = 1L,
    project_id = source_ref_id,
    source_references = list(list(
      source_ref_id = source_ref_id,
      country = as.character(source$country[[1L]]),
      portal = as.character(source$portal[[1L]]),
      native_project_id = as.character(source$native_project_id[[1L]]),
      source_url = if (is.na(source$source_url[[1L]])) NULL else as.character(source$source_url[[1L]])
    )),
    documents = documents
  )
  if (is.null(path)) {
    path <- file.path(dirname(catalog$path), "projects", source_ref_id, "manifest.json")
  }
  path <- acquisition_storage_scalar(path, "path")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    acquisition_storage_abort("Package {.pkg jsonlite} is required to write project manifests.")
  }
  jsonlite::write_json(
    manifest, path, auto_unbox = TRUE, pretty = TRUE,
    null = "null", na = "null"
  )
  attr(manifest, "path") <- normalizePath(path, winslash = "/", mustWork = TRUE)
  invisible(manifest)
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

acquisition_storage_reservation_initial_bytes <- function(conn, job) {
  bytes <- acquisition_storage_job_estimate(job, conn)
  # Start each download with a bounded budget. Estimates and HEAD values are
  # advisory; the guard grows this lease before a chunk or duplicate copy is
  # written. Keeping the initial lease small allows two downloads to overlap.
  bytes <- max(64 * 1024, min(bytes, 4 * 1024^2))
  if (identical(as.character(job$kind[[1L]]), "download") &&
    isTRUE(getOption("planscanR.content_addressed_downloads", FALSE))) {
    bytes <- bytes * 2
  }
  acquisition_storage_bytes(bytes, "reservation_bytes", allow_zero = FALSE)
}

acquisition_storage_reservation_growth_bytes <- function(conn) {
  configured <- acquisition_storage_setting_value_connection(
    conn, "default_estimate_bytes", 4 * 1024^2
  )
  configured <- suppressWarnings(as.numeric(configured))
  if (length(configured) != 1L || is.na(configured) || !is.finite(configured) || configured <= 0) {
    configured <- 4 * 1024^2
  }
  # Keep a small window ahead of an unknown-size transfer. The next guard call
  # grows it before another chunk is written, without reserving all headroom.
  max(64 * 1024, min(configured, 4 * 1024^2))
}

acquisition_storage_guard_values <- function(bytes_written, advertised_bytes) {
  written <- suppressWarnings(as.numeric(bytes_written))
  advertised <- suppressWarnings(as.numeric(advertised_bytes))
  if (length(written) != 1L || is.na(written) || !is.finite(written) || written < 0) {
    return(NULL)
  }
  if (length(advertised) != 1L || is.na(advertised) || !is.finite(advertised) || advertised < 0) {
    advertised <- NA_real_
  }
  list(written = written, advertised = advertised)
}

acquisition_storage_reservation_current_connection <- function(conn, job_id, lease_token) {
  now <- acquisition_catalog_now()
  job <- acquisition_catalog_one(
    conn,
    paste(
      "SELECT job_id, state, lease_token, lease_expires_at FROM acquisition_jobs",
      "WHERE job_id = ? AND state = 'running' AND lease_token = ? AND lease_expires_at > ?"
    ),
    list(as.character(job_id), as.character(lease_token), now)
  )
  if (is.null(job)) return(NULL)
  reservation <- acquisition_catalog_one(
    conn,
    paste(
      "SELECT reservation_id, job_id, root_id, bytes, written_bytes, pending_bytes, advertised_bytes, phase",
      "FROM acquisition_storage_reservations WHERE job_id = ? AND state = 'held'",
      "AND (lease_token IS NULL OR lease_token = ?)"
    ),
    list(as.character(job_id), as.character(lease_token))
  )
  if (is.null(reservation)) return(NULL)
  list(job = job, reservation = reservation, now = now)
}

acquisition_storage_reservation_lease_valid <- function(catalog, job_id, lease_token) {
  valid <- tryCatch({
    conn <- acquisition_catalog_connection(catalog)
    now <- acquisition_catalog_now()
    !is.null(acquisition_catalog_one(
      conn,
      paste(
        "SELECT r.reservation_id FROM acquisition_storage_reservations r",
        "JOIN acquisition_jobs j ON j.job_id = r.job_id",
        "WHERE r.job_id = ? AND r.state = 'held' AND r.lease_token = ?",
        "AND j.state = 'running' AND j.lease_token = ? AND j.lease_expires_at > ?"
      ),
      list(as.character(job_id), as.character(lease_token), as.character(lease_token), now)
    ))
  }, error = function(error) FALSE)
  isTRUE(valid)
}

acquisition_storage_reservation_update_connection <- function(
  conn,
  job_id,
  lease_token,
  bytes_written,
  advertised_bytes = NA_real_,
  phase = "stream",
  content_addressed = FALSE,
  pending_bytes = 0
) {
  phase <- match.arg(phase, c("stream", "publish", "promotion"))
  values <- acquisition_storage_guard_values(bytes_written, advertised_bytes)
  if (is.null(values)) return(list(ok = FALSE, reason = "invalid-storage-guard-bytes"))
  pending_bytes <- suppressWarnings(as.numeric(pending_bytes))
  if (length(pending_bytes) != 1L || is.na(pending_bytes) ||
    !is.finite(pending_bytes) || pending_bytes < 0) {
    return(list(ok = FALSE, reason = "invalid-storage-guard-pending-bytes"))
  }
  current <- acquisition_storage_reservation_current_connection(conn, job_id, lease_token)
  if (is.null(current)) return(list(ok = FALSE, reason = "lease-stale"))
  reservation <- current$reservation
  root <- acquisition_catalog_one(
    conn,
    "SELECT * FROM acquisition_storage_roots WHERE root_id = ? AND active = 1",
    list(as.character(reservation$root_id[[1L]]))
  )
  if (is.null(root)) return(list(ok = FALSE, reason = "storage-root-inactive"))
  current_bytes <- suppressWarnings(as.numeric(reservation$bytes[[1L]]))
  if (length(current_bytes) != 1L || is.na(current_bytes) || current_bytes < 0) {
    return(list(ok = FALSE, reason = "invalid-storage-reservation"))
  }
  prior_written <- suppressWarnings(as.numeric(reservation$written_bytes[[1L]]))
  if (length(prior_written) != 1L || is.na(prior_written) || prior_written < 0) prior_written <- 0
  prior_pending <- suppressWarnings(as.numeric(reservation$pending_bytes[[1L]]))
  if (length(prior_pending) != 1L || is.na(prior_pending) || prior_pending < 0) prior_pending <- 0
  prior_advertised <- suppressWarnings(as.numeric(reservation$advertised_bytes[[1L]]))
  if (length(prior_advertised) != 1L || is.na(prior_advertised) || prior_advertised < 0) prior_advertised <- NA_real_
  written <- max(values$written, prior_written)
  advertised <- if (is.na(values$advertised)) prior_advertised else {
    if (is.na(prior_advertised)) values$advertised else max(prior_advertised, values$advertised)
  }
  next_bytes <- current_bytes
  next_phase <- as.character(reservation$phase[[1L]] %||% "stream")
  stream_budget <- 0
  if (phase %in% c("stream", "publish")) {
    # The live filesystem free-space value already includes these bytes. Remove
    # them from the held future budget before deciding whether it must grow.
    delta <- max(0, written - prior_written)
    next_bytes <- max(0, current_bytes - delta)
    growth <- acquisition_storage_reservation_growth_bytes(conn)
    source_remaining <- if (is.na(advertised)) {
      pending_bytes + growth
    } else {
      max(pending_bytes, max(0, advertised - written))
    }
    duplicate_remaining <- if (isTRUE(content_addressed)) {
      if (is.na(advertised)) {
        max(growth, written + pending_bytes)
      } else {
        max(advertised, written + pending_bytes)
      }
    } else {
      0
    }
    slack <- if (is.na(advertised)) 0 else growth
    next_bytes <- max(next_bytes, source_remaining + duplicate_remaining + slack)
    stream_budget <- max(0, next_bytes - duplicate_remaining)
    if (isTRUE(content_addressed)) {
      # A future source increment x needs both x bytes for its stream and a
      # retained copy of max(advertised, written + x). These two bounds are a
      # conservative allowance for the callback cache: one assumes the
      # advertised size remains authoritative, the other protects an
      # underreported or unknown source whose duplicate grows with x.
      duplicate_floor <- if (is.na(advertised)) written else max(advertised, written)
      stream_budget <- min(
        max(0, next_bytes - duplicate_floor),
        max(0, (next_bytes - written) / 2)
      )
    }
    next_phase <- "stream"
  } else if (identical(phase, "promotion")) {
    # Cached sources have no stream bytes to account for. Reserve the retained
    # second copy before file.copy(); fresh streams already reserved it.
    # `bytes_written` describes the already materialized source. The pending
    # amount is the retained copy being staged; consume that amount only after
    # the post-copy fence reports pending = 0.
    promoted <- max(0, prior_pending - pending_bytes)
    next_bytes <- max(0, current_bytes - promoted)
    next_bytes <- max(next_bytes, pending_bytes + acquisition_storage_reservation_growth_bytes(conn))
    stream_budget <- 0
    next_phase <- "promotion"
  }
  capacity <- acquisition_storage_capacity(as.character(root$root_path[[1L]]))
  held_other <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT COALESCE(SUM(bytes), 0) AS bytes FROM acquisition_storage_reservations",
      "WHERE state = 'held' AND job_id <> ?"
    ),
    params = list(as.character(job_id))
  )$bytes[[1L]]
  reserve <- max(
    as.numeric(root$reserve_bytes[[1L]]),
    as.numeric(capacity$total_bytes) * as.numeric(root$reserve_fraction[[1L]])
  )
  if (!is.finite(next_bytes) || next_bytes < 0 ||
    as.numeric(capacity$free_bytes) - as.numeric(held_other) - next_bytes < reserve) {
    return(list(
      ok = FALSE,
      reason = "disk-reserve-exhausted",
      available_bytes = as.numeric(capacity$free_bytes),
      reserved_bytes = as.numeric(held_other),
      requested_bytes = next_bytes
    ))
  }
  changed <- DBI::dbExecute(
    conn,
    paste(
      "UPDATE acquisition_storage_reservations SET bytes = ?, written_bytes = ?,",
      "pending_bytes = ?, advertised_bytes = ?, phase = ? WHERE job_id = ? AND state = 'held'",
      "AND lease_token = ?"
    ),
    params = list(
      next_bytes, written, pending_bytes, advertised, next_phase,
      as.character(job_id), as.character(lease_token)
    )
  )
  if (changed != 1L) return(list(ok = FALSE, reason = "lease-stale"))
  list(
    ok = TRUE,
    bytes = next_bytes,
    written_bytes = written,
    advertised_bytes = advertised,
    stream_budget_bytes = stream_budget,
    root_id = as.character(root$root_id[[1L]])
  )
}

acquisition_storage_reservation_update <- function(
  catalog,
  job_id,
  lease_token,
  bytes_written,
  advertised_bytes = NA_real_,
  phase = "stream",
  content_addressed = FALSE,
  pending_bytes = 0
) {
  acquisition_catalog_transaction(catalog, function(conn) {
    acquisition_storage_reservation_update_connection(
      conn,
      job_id,
      lease_token,
      bytes_written,
      advertised_bytes,
      phase,
      content_addressed,
      pending_bytes
    )
  })
}

acquisition_storage_try_reserve_connection <- function(conn, job) {
  if (!identical(as.character(job$kind), "download") && !identical(as.character(job$kind), "migrate_storage")) {
    return(list(ok = TRUE, skipped = TRUE))
  }
  root <- acquisition_storage_root_for_job_connection(conn, job)
  if (is.null(root)) {
    return(list(ok = TRUE, skipped = TRUE))
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
  is_download <- identical(as.character(job$kind), "download")
  # `estimated_bytes` is advisory catalog metadata rather than a trusted
  # Content-Length. A download starts with a small bounded lease and grows it
  # through the guard before each materialized chunk or retained copy. The
  # lease is never a per-file transfer ceiling, and two claims can coexist on
  # the same root while the filesystem floor remains protected.
  if (is_download) {
    bytes <- acquisition_storage_reservation_initial_bytes(conn, job)
  } else {
    # Storage migration keeps its estimate semantics because it copies
    # already-materialized files.
    bytes <- acquisition_storage_job_estimate(job, conn)
  }
  if (!is.finite(bytes) || bytes <= 0) {
    return(list(
      ok = FALSE,
      reason = "disk-reserve-exhausted",
      root_id = as.character(root$root_id[[1L]]),
      root_path = as.character(root$root_path[[1L]]),
      available_bytes = available,
      reserved_bytes = held,
      requested_bytes = bytes
    ))
  }
  if (available - as.numeric(held) - bytes < reserve) {
    return(list(ok = FALSE, reason = "disk-reserve-exhausted", root_id = as.character(root$root_id[[1L]]), root_path = as.character(root$root_path[[1L]]), available_bytes = available, reserved_bytes = held, requested_bytes = bytes))
  }
  now <- acquisition_catalog_now()
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO acquisition_storage_reservations (reservation_id, job_id, root_id, bytes, state, created_at,",
      "lease_token, written_bytes, pending_bytes, advertised_bytes, phase)",
      "VALUES (?, ?, ?, ?, 'held', ?, NULL, 0, 0, NULL, 'stream')",
      "ON CONFLICT(job_id) DO UPDATE SET root_id = excluded.root_id,",
      "bytes = excluded.bytes, state = 'held', created_at = excluded.created_at, released_at = NULL,",
      "lease_token = NULL, written_bytes = 0, pending_bytes = 0, advertised_bytes = NULL, phase = 'stream'"
    ),
    params = list(paste0("reservation-", as.character(job$job_id[[1L]])), as.character(job$job_id[[1L]]), as.character(root$root_id[[1L]]), bytes, now)
  )
  list(ok = TRUE, root_id = as.character(root$root_id[[1L]]), root_path = as.character(root$root_path[[1L]]), bytes = bytes, available_bytes = available, reserve_bytes = reserve)
}

acquisition_storage_bind_reservation_connection <- function(conn, job_id, lease_token) {
  changed <- DBI::dbExecute(
    conn,
    paste(
      "UPDATE acquisition_storage_reservations SET lease_token = ?",
      "WHERE job_id = ? AND state = 'held' AND lease_token IS NULL"
    ),
    params = list(as.character(lease_token), as.character(job_id))
  )
  if (changed != 1L) {
    return(list(ok = FALSE, reason = "storage-reservation-lease-bind-failed"))
  }
  list(ok = TRUE)
}

acquisition_storage_release_connection <- function(conn, job_id, lease_token = NULL) {
  sql <- paste(
    "UPDATE acquisition_storage_reservations SET state = 'released', released_at = ?",
    "WHERE job_id = ? AND state = 'held'"
  )
  params <- list(acquisition_catalog_now(), as.character(job_id))
  if (!is.null(lease_token)) {
    sql <- paste(sql, "AND lease_token = ?")
    params <- c(params, list(as.character(lease_token)))
  }
  DBI::dbExecute(conn, sql, params = params)
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
      max_attempts = 1000L,
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
