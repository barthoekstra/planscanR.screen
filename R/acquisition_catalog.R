# ACQ-1a: the acquisition catalog is deliberately separate from the review
# repository and from the planscanR cache.  This file contains only the small
# catalog/import seam needed by later queue work; queue claims belong to ACQ-1b.

acquisition_catalog_schema_version <- 5L

acquisition_catalog_abort <- function(
  message,
  class = "planscanR_error_acquisition_catalog",
  ...
) {
  rlang::abort(message, class = unique(c(class, "planscanR_error_acquisition")), ...)
}

acquisition_catalog_require_packages <- function() {
  needed <- c("DBI", "RSQLite")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "The acquisition catalog needs {cli::qty(missing)} package{?s}: {.pkg {missing}}.",
        i = "Install the optional DBI and RSQLite dependencies and retry."
      ),
      class = "planscanR_error_missing_dep"
    )
  }
  invisible(TRUE)
}

acquisition_catalog_scalar_character <- function(value, arg, allow_null = FALSE) {
  if (is.null(value) && isTRUE(allow_null)) {
    return(NULL)
  }
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value))
  ) {
    acquisition_catalog_abort(
      paste0("{.arg ", arg, "} must be one non-empty character string.")
    )
  }
  trimws(value)
}

acquisition_catalog_optional_character <- function(value, arg) {
  if (is.null(value) || (length(value) == 1L && is.na(value))) {
    return(NA_character_)
  }
  acquisition_catalog_scalar_character(value, arg)
}

acquisition_catalog_scalar_integer <- function(value, arg, lower = 1L, upper = 600000L) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value != trunc(value) ||
      value < lower ||
      value > upper
  ) {
    acquisition_catalog_abort(
      paste0("{.arg ", arg, "} must be an integer between ", lower, " and ", upper, ".")
    )
  }
  as.integer(value)
}

acquisition_catalog_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
}

acquisition_catalog_digest <- function(value) {
  bytes <- serialize(value, NULL, version = 2)
  path <- tempfile("planscanR-acquisition-digest-")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  writeBin(bytes, path)
  unname(as.character(tools::md5sum(path)))
}

acquisition_catalog_blob <- function(value) {
  bytes <- serialize(value, NULL, version = 2)
  if (length(bytes) == 0L) {
    return("")
  }
  paste(sprintf("%02x", as.integer(bytes)), collapse = "")
}

acquisition_catalog_sha256 <- function(path) {
  if (!requireNamespace("openssl", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "The acquisition model registry needs the {.pkg openssl} package to fingerprint artifacts.",
        i = "Install openssl and retry."
      ),
      class = "planscanR_error_missing_dep"
    )
  }
  size <- file.info(path)$size
  bytes <- readBin(path, what = "raw", n = size)
  paste0(openssl::sha256(bytes))
}

acquisition_catalog_id <- function(prefix, ...) {
  paste0(prefix, "-", acquisition_catalog_digest(list(...)))
}

acquisition_catalog_busy <- function(error) {
  grepl(
    "(database is locked|database table is locked|database is busy|SQLITE_BUSY)",
    conditionMessage(error),
    ignore.case = TRUE
  )
}

acquisition_catalog_connection <- function(catalog) {
  if (!inherits(catalog, "planscanR_acquisition_catalog")) {
    acquisition_catalog_abort("{.arg catalog} must be a planscanR_acquisition_catalog.")
  }
  if (isTRUE(catalog$state$closed) || !DBI::dbIsValid(catalog$conn)) {
    acquisition_catalog_abort("The acquisition catalog is closed.")
  }
  catalog$conn
}

acquisition_catalog_transaction_connection <- function(conn, fn, busy_timeout) {
  deadline <- as.numeric(Sys.time()) + (busy_timeout / 1000)
  repeat {
    started <- FALSE
    committed <- FALSE
    result <- tryCatch(
      {
        DBI::dbExecute(conn, "BEGIN IMMEDIATE")
        started <- TRUE
        value <- fn(conn)
        DBI::dbCommit(conn)
        committed <- TRUE
        value
      },
      error = function(error) {
        if (isTRUE(started) && !isTRUE(committed) && DBI::dbIsValid(conn)) {
          try(DBI::dbRollback(conn), silent = TRUE)
        }
        error
      }
    )
    if (!inherits(result, "error")) {
      return(result)
    }
    if (!acquisition_catalog_busy(result) || as.numeric(Sys.time()) >= deadline) {
      if (acquisition_catalog_busy(result)) {
        acquisition_catalog_abort(
          "The acquisition SQLite database remained busy for the configured bounded timeout.",
          class = "planscanR_error_acquisition_busy",
          parent = result
        )
      }
      stop(result)
    }
    Sys.sleep(min(0.01, max(0, deadline - as.numeric(Sys.time()))))
  }
}

acquisition_catalog_apply_migration <- function(conn, version) {
  if (identical(version, 1L)) {
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS acquisition_schema_migrations (",
        "version INTEGER PRIMARY KEY,",
        "applied_at TEXT NOT NULL",
        ")"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS acquisition_sources (",
        "source_ref_id TEXT PRIMARY KEY,",
        "country TEXT NOT NULL,",
        "portal TEXT NOT NULL,",
        "native_project_id TEXT NOT NULL,",
        "source_url TEXT,",
        "legacy_sidecar_path TEXT,",
        "metadata_blob TEXT,",
        "created_at TEXT NOT NULL,",
        "UNIQUE (country, portal, native_project_id)",
        ")"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS acquisition_documents (",
        "document_ref_id TEXT PRIMARY KEY,",
        "source_ref_id TEXT NOT NULL,",
        "native_attachment_id TEXT,",
        "attachment_key TEXT NOT NULL,",
        "attachment_url TEXT,",
        "section TEXT,",
        "ordinal INTEGER NOT NULL,",
        "identity_status TEXT NOT NULL CHECK (identity_status IN ('identified', 'ambiguous')),",
        "legacy_local_path TEXT,",
        "metadata_blob TEXT,",
        "created_at TEXT NOT NULL,",
        "UNIQUE (source_ref_id, attachment_key),",
        "FOREIGN KEY (source_ref_id) REFERENCES acquisition_sources(source_ref_id)",
        ")"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS acquisition_files (",
        "file_ref_id TEXT PRIMARY KEY,",
        "document_ref_id TEXT NOT NULL,",
        "root_id TEXT NOT NULL,",
        "relative_path TEXT NOT NULL,",
        "sha256 TEXT,",
        "size_bytes INTEGER,",
        "verification_status TEXT NOT NULL CHECK (verification_status IN ('unverified', 'verified')),",
        "provenance TEXT NOT NULL,",
        "legacy_path TEXT,",
        "observed_at TEXT NOT NULL,",
        "UNIQUE (document_ref_id, root_id, relative_path),",
        "FOREIGN KEY (document_ref_id) REFERENCES acquisition_documents(document_ref_id)",
        ")"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS acquisition_import_ledger (",
        "import_source TEXT NOT NULL,",
        "import_row_id TEXT NOT NULL,",
        "entity_type TEXT NOT NULL,",
        "entity_id TEXT NOT NULL,",
        "payload_fingerprint TEXT NOT NULL,",
        "imported_at TEXT NOT NULL,",
        "PRIMARY KEY (import_source, import_row_id)",
        ")"
      )
    )
    DBI::dbExecute(
      conn,
      "CREATE INDEX IF NOT EXISTS acquisition_documents_source_idx ON acquisition_documents(source_ref_id)"
    )
    DBI::dbExecute(
      conn,
      "CREATE INDEX IF NOT EXISTS acquisition_files_document_idx ON acquisition_files(document_ref_id)"
    )
  } else if (identical(version, 2L)) {
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS acquisition_models (",
        "model_id TEXT PRIMARY KEY,",
        "artifact_path TEXT NOT NULL,",
        "source_artifact_path TEXT NOT NULL,",
        "artifact_sha256 TEXT NOT NULL,",
        "config_id TEXT NOT NULL,",
        "input_snapshot_id TEXT NOT NULL,",
        "metadata_blob TEXT,",
        "payload_fingerprint TEXT NOT NULL,",
        "created_by TEXT,",
        "created_at TEXT NOT NULL",
        ")"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS acquisition_selections (",
        "selection_id TEXT PRIMARY KEY,",
        "input_snapshot_id TEXT NOT NULL,",
        "model_id TEXT NOT NULL,",
        "metadata_blob TEXT,",
        "payload_fingerprint TEXT NOT NULL,",
        "submitted_by TEXT,",
        "submitted_at TEXT NOT NULL,",
        "FOREIGN KEY (model_id) REFERENCES acquisition_models(model_id)",
        ")"
      )
    )
    DBI::dbExecute(
      conn,
      "CREATE INDEX IF NOT EXISTS acquisition_selections_model_idx ON acquisition_selections(model_id)"
    )
  } else if (identical(version, 3L)) {
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS acquisition_jobs (",
        "job_id TEXT PRIMARY KEY,",
        "kind TEXT NOT NULL CHECK (kind IN ('discover', 'download')),",
        "target_type TEXT NOT NULL CHECK (target_type IN ('source', 'document', 'file')),",
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
        "UNIQUE (kind, target_type, target_ref_id)",
        ")"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE INDEX IF NOT EXISTS acquisition_jobs_claim_idx",
        "ON acquisition_jobs (state, available_at, priority, created_at, job_id)"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE INDEX IF NOT EXISTS acquisition_jobs_host_idx",
        "ON acquisition_jobs (host, state, lease_expires_at)"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS acquisition_job_attempts (",
        "job_id TEXT NOT NULL,",
        "attempt_no INTEGER NOT NULL CHECK (attempt_no >= 1),",
        "lease_token TEXT NOT NULL UNIQUE,",
        "worker_id TEXT NOT NULL,",
        "host TEXT,",
        "claimed_at TEXT NOT NULL,",
        "lease_expires_at TEXT NOT NULL,",
        "heartbeat_at TEXT NOT NULL,",
        "finished_at TEXT,",
        "outcome TEXT CHECK (outcome IN ('running', 'expired', 'succeeded', 'retryable', 'blocked', 'failed', 'cancelled')),",
        "error_category TEXT,",
        "error_message TEXT,",
        "retry_at TEXT,",
        "bytes INTEGER,",
        "sha256 TEXT,",
        "local_path TEXT,",
        "result_blob TEXT,",
        "PRIMARY KEY (job_id, attempt_no),",
        "FOREIGN KEY (job_id) REFERENCES acquisition_jobs(job_id)",
        ")"
      )
    )
    DBI::dbExecute(
      conn,
      "CREATE INDEX IF NOT EXISTS acquisition_job_attempts_token_idx ON acquisition_job_attempts (lease_token)"
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS acquisition_host_limits (",
        "host TEXT PRIMARY KEY,",
        "max_concurrency INTEGER NOT NULL CHECK (max_concurrency >= 1),",
        "active_count INTEGER NOT NULL DEFAULT 0 CHECK (active_count >= 0),",
        "cooldown_until TEXT,",
        "updated_at TEXT NOT NULL",
        ")"
      )
    )
  } else if (identical(version, 4L)) {
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS acquisition_campaigns (",
        "campaign_id TEXT PRIMARY KEY,",
        "selection_id TEXT NOT NULL UNIQUE,",
        "model_id TEXT NOT NULL,",
        "input_snapshot_id TEXT NOT NULL,",
        "threshold REAL NOT NULL,",
        "policy_version TEXT NOT NULL,",
        "submitted_by TEXT,",
        "submitted_at TEXT NOT NULL,",
        "payload_fingerprint TEXT NOT NULL UNIQUE,",
        "priority_revision INTEGER NOT NULL DEFAULT 0,",
        "metadata_blob TEXT,",
        "FOREIGN KEY (selection_id) REFERENCES acquisition_selections(selection_id),",
        "FOREIGN KEY (model_id) REFERENCES acquisition_models(model_id)",
        ")"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS acquisition_priority_rules (",
        "campaign_id TEXT NOT NULL,",
        "revision INTEGER NOT NULL,",
        "rule_id TEXT NOT NULL,",
        "ordinal INTEGER NOT NULL,",
        "name TEXT NOT NULL,",
        "countries_blob TEXT NOT NULL,",
        "energy_blob TEXT NOT NULL,",
        "priority INTEGER NOT NULL,",
        "explanation TEXT NOT NULL,",
        "created_at TEXT NOT NULL,",
        "PRIMARY KEY (campaign_id, revision, rule_id),",
        "UNIQUE (campaign_id, revision, ordinal),",
        "FOREIGN KEY (campaign_id) REFERENCES acquisition_campaigns(campaign_id)",
        ")"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS acquisition_campaign_items (",
        "campaign_id TEXT NOT NULL,",
        "item_key TEXT NOT NULL,",
        "source_ref_id TEXT NOT NULL,",
        "selected INTEGER NOT NULL CHECK (selected IN (0, 1)),",
        "probability REAL,",
        "energy_kind TEXT NOT NULL CHECK (energy_kind IN ('wind', 'solar', 'mixed', 'unknown')),",
        "energy_tags_blob TEXT NOT NULL,",
        "energy_evidence_blob TEXT,",
        "energy_confidence REAL,",
        "manual_energy_tag TEXT,",
        "manual_override TEXT,",
        "priority INTEGER NOT NULL,",
        "priority_rule_id TEXT NOT NULL,",
        "priority_explanation TEXT NOT NULL,",
        "priority_revision INTEGER NOT NULL DEFAULT 0,",
        "job_id TEXT,",
        "work_status TEXT NOT NULL CHECK (work_status IN ('enqueued', 'reused', 'active', 'complete', 'not_selected')),",
        "created_at TEXT NOT NULL,",
        "updated_at TEXT NOT NULL,",
        "PRIMARY KEY (campaign_id, item_key),",
        "FOREIGN KEY (campaign_id) REFERENCES acquisition_campaigns(campaign_id),",
        "FOREIGN KEY (source_ref_id) REFERENCES acquisition_sources(source_ref_id)",
        ")"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS acquisition_campaign_reprioritizations (",
        "reprioritization_id TEXT PRIMARY KEY,",
        "campaign_id TEXT NOT NULL,",
        "revision INTEGER NOT NULL,",
        "rules_blob TEXT NOT NULL,",
        "changed_by TEXT,",
        "changed_at TEXT NOT NULL,",
        "payload_fingerprint TEXT NOT NULL UNIQUE,",
        "FOREIGN KEY (campaign_id) REFERENCES acquisition_campaigns(campaign_id),",
        "UNIQUE (campaign_id, revision)",
        ")"
      )
    )
    for (column in c("campaign_id TEXT", "campaign_item_key TEXT", "priority_rule_id TEXT", "priority_explanation TEXT")) {
      DBI::dbExecute(conn, paste("ALTER TABLE acquisition_jobs ADD COLUMN", column))
    }
    DBI::dbExecute(
      conn,
      "CREATE INDEX IF NOT EXISTS acquisition_jobs_campaign_idx ON acquisition_jobs (campaign_id, state, priority)"
    )
    DBI::dbExecute(
      conn,
      "CREATE INDEX IF NOT EXISTS acquisition_campaign_items_job_idx ON acquisition_campaign_items (job_id)"
    )
  } else if (identical(version, 5L)) {
    # Queue CHECK expansion runs after this numbered transaction. SQLite only
    # permits the safe rebuild with foreign keys temporarily disabled.
  } else {
    acquisition_catalog_abort(paste0("Unknown acquisition catalog migration: ", version))
  }
  DBI::dbExecute(
    conn,
    "INSERT OR IGNORE INTO acquisition_schema_migrations (version, applied_at) VALUES (?, ?)",
    params = list(as.integer(version), acquisition_catalog_now())
  )
  DBI::dbExecute(conn, paste0("PRAGMA user_version = ", as.integer(version)))
  invisible(TRUE)
}

acquisition_catalog_migrate <- function(conn, busy_timeout) {
  user_version <- as.integer(DBI::dbGetQuery(conn, "PRAGMA user_version")[[1L]])
  tables <- DBI::dbListTables(conn)
  applied <- if ("acquisition_schema_migrations" %in% tables) {
    DBI::dbGetQuery(conn, "SELECT version FROM acquisition_schema_migrations")$version
  } else {
    integer(0)
  }
  known_version <- max(c(0L, user_version, as.integer(applied)), na.rm = TRUE)
  if (known_version > acquisition_catalog_schema_version) {
    acquisition_catalog_abort(
      paste0(
        "The acquisition catalog schema version ", known_version,
        " is newer than this package supports (",
        acquisition_catalog_schema_version, ")."
      ),
      class = "planscanR_error_acquisition_schema"
    )
  }
  if (known_version < acquisition_catalog_schema_version) {
    acquisition_catalog_transaction_connection(conn, function(connection) {
      for (version in seq.int(known_version + 1L, acquisition_catalog_schema_version)) {
        acquisition_catalog_apply_migration(connection, version)
      }
      invisible(TRUE)
    }, busy_timeout = busy_timeout)
  }
  invisible(acquisition_catalog_schema_version)
}

#' Open the separate SQLite acquisition catalog.
#'
#' The catalog uses local-disk WAL mode and numbered idempotent migrations. A
#' file containing the existing review tables is rejected so review state keeps
#' one authoritative SQLite store.
#'
#' @param path File path for the acquisition database.
#' @param busy_timeout Bounded busy timeout in milliseconds.
#' @return A `planscanR_acquisition_catalog` object with `conn`, `path`,
#'   `schema_version`, `journal_mode`, `foreign_keys`, `close()`, and
#'   `transaction()` members.
#' @export
acquisition_catalog_open <- function(path, busy_timeout = 5000L) {
  acquisition_catalog_require_packages()
  path <- acquisition_catalog_scalar_character(path, "path")
  busy_timeout <- acquisition_catalog_scalar_integer(busy_timeout, "busy_timeout")
  path <- normalizePath(path, mustWork = FALSE)
  parent <- dirname(path)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    acquisition_catalog_abort(paste0("Could not create the catalog directory: ", parent))
  }
  conn <- DBI::dbConnect(RSQLite::SQLite(), path)
  state <- new.env(parent = emptyenv())
  state$closed <- FALSE
  state$busy_timeout <- busy_timeout
  state$conn <- conn
  on.exit(
    if (!isTRUE(state$ready) && DBI::dbIsValid(conn)) {
      DBI::dbDisconnect(conn)
    },
    add = TRUE
  )
  tables <- DBI::dbListTables(conn)
  if (
    "curation_reviews" %in% tables ||
      ("schema_migrations" %in% tables && any(grepl("^curation_", tables)))
  ) {
    acquisition_catalog_abort(
      "The supplied SQLite file is a review database; acquisition state requires a separate file.",
      class = "planscanR_error_acquisition_review_database"
    )
  }
  DBI::dbExecute(conn, paste0("PRAGMA busy_timeout = ", busy_timeout))
  journal_mode <- tolower(as.character(DBI::dbGetQuery(conn, "PRAGMA journal_mode = WAL")[[1L]]))
  if (!identical(journal_mode, "wal")) {
    acquisition_catalog_abort(
      "The acquisition catalog requires a file-backed SQLite database in WAL mode.",
      class = "planscanR_error_acquisition_schema"
    )
  }
  DBI::dbExecute(conn, "PRAGMA foreign_keys = ON")
  foreign_keys <- as.integer(DBI::dbGetQuery(conn, "PRAGMA foreign_keys")[[1L]]) == 1L
  if (!isTRUE(foreign_keys)) {
    acquisition_catalog_abort(
      "SQLite foreign-key enforcement could not be enabled.",
      class = "planscanR_error_acquisition_schema"
    )
  }
  acquisition_catalog_migrate(conn, busy_timeout)
  # ACQ-2c keeps storage metadata in its own migration ledger so later
  # campaign/catalog migrations can advance independently.  The function is
  # defined by acquisition_storage.R and is called only after catalog setup.
  if (exists("acquisition_storage_migrate", mode = "function")) {
    acquisition_storage_migrate(conn, busy_timeout = busy_timeout)
  }
  catalog <- structure(
    list(
      conn = conn,
      path = path,
      schema_version = acquisition_catalog_schema_version,
      journal_mode = journal_mode,
      foreign_keys = foreign_keys,
      busy_timeout = busy_timeout,
      state = state
    ),
    class = "planscanR_acquisition_catalog"
  )
  catalog$close <- function() {
    if (!isTRUE(state$closed) && DBI::dbIsValid(state$conn)) {
      DBI::dbDisconnect(state$conn)
      state$closed <- TRUE
    }
    invisible(TRUE)
  }
  catalog$transaction <- function(fn) {
    acquisition_catalog_transaction(catalog, fn)
  }
  state$ready <- TRUE
  catalog
}

#' Run a short transaction against the acquisition catalog.
#'
#' `fn` receives the DBI connection. Keep callbacks free of network, model,
#' and long-running file work; ACQ-1b uses this seam for short claims.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param fn Function receiving the DBI connection.
#' @return The value returned by `fn`.
#' @export
acquisition_catalog_transaction <- function(catalog, fn) {
  acquisition_catalog_require_packages()
  acquisition_catalog_connection(catalog)
  if (!is.function(fn)) {
    acquisition_catalog_abort("{.arg fn} must be a function accepting a DBI connection.")
  }
  acquisition_catalog_transaction_connection(
    catalog$conn,
    fn,
    busy_timeout = catalog$busy_timeout
  )
}

#' Close an acquisition catalog.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @return Invisibly, `TRUE`.
#' @export
acquisition_catalog_close <- function(catalog) {
  acquisition_catalog_connection(catalog)
  catalog$close()
}

acquisition_catalog_one <- function(conn, sql, params = list()) {
  out <- DBI::dbGetQuery(conn, sql, params = params)
  if (nrow(out) == 0L) NULL else out[1L, , drop = FALSE]
}

acquisition_catalog_row_value <- function(row, name, default = NA_character_) {
  if (!name %in% names(row)) {
    return(default)
  }
  value <- row[[name]]
  if (length(value) == 0L || is.na(value[[1L]])) default else value[[1L]]
}

acquisition_catalog_register_source_connection <- function(
  conn,
  country,
  portal,
  native_project_id,
  source_url = NULL,
  legacy_sidecar_path = NULL,
  metadata = NULL
) {
  country <- acquisition_catalog_scalar_character(country, "country")
  portal <- acquisition_catalog_scalar_character(portal, "portal")
  native_project_id <- acquisition_catalog_scalar_character(native_project_id, "native_project_id")
  source_url <- acquisition_catalog_optional_character(source_url, "source_url")
  legacy_sidecar_path <- acquisition_catalog_optional_character(legacy_sidecar_path, "legacy_sidecar_path")
  source_ref_id <- acquisition_catalog_id("source", country, portal, native_project_id)
  existing <- acquisition_catalog_one(
    conn,
    "SELECT * FROM acquisition_sources WHERE source_ref_id = ?",
    list(source_ref_id)
  )
  if (!is.null(existing)) {
    if (
      !identical(as.character(existing$country[[1L]]), country) ||
        !identical(as.character(existing$portal[[1L]]), portal) ||
        !identical(as.character(existing$native_project_id[[1L]]), native_project_id)
    ) {
      acquisition_catalog_abort(
        "The source reference ID does not match its stored composite identity.",
        class = "planscanR_error_acquisition_immutable"
      )
    }
    return(list(source_ref_id = source_ref_id, status = "replayed"))
  }
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO acquisition_sources (source_ref_id, country, portal, native_project_id,",
      "source_url, legacy_sidecar_path, metadata_blob, created_at)",
      "VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
    ),
    params = list(
      source_ref_id,
      country,
      portal,
      native_project_id,
      source_url,
      legacy_sidecar_path,
      if (is.null(metadata)) NA_character_ else acquisition_catalog_blob(metadata),
      acquisition_catalog_now()
    )
  )
  list(source_ref_id = source_ref_id, status = "inserted")
}

#' Register one source/project reference.
#'
#' Source identity is the composite `(country, portal, native_project_id)`.
#' Repeating the same reference is a no-op.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param country Source-country namespace.
#' @param portal Source portal identifier.
#' @param native_project_id Portal-native project ID.
#' @param source_url Optional canonical source URL.
#' @param legacy_sidecar_path Optional legacy sidecar path.
#' @param metadata Optional provenance metadata.
#' @return A list with `source_ref_id` and `status`.
#' @export
acquisition_catalog_register_source <- function(
  catalog,
  country,
  portal,
  native_project_id,
  source_url = NULL,
  legacy_sidecar_path = NULL,
  metadata = NULL
) {
  acquisition_catalog_transaction(catalog, function(conn) {
    acquisition_catalog_register_source_connection(
      conn,
      country,
      portal,
      native_project_id,
      source_url,
      legacy_sidecar_path,
      metadata
    )
  })
}

acquisition_catalog_register_document_connection <- function(
  conn,
  source_ref_id,
  native_attachment_id = NULL,
  attachment_key = NULL,
  attachment_url = NULL,
  section = NULL,
  ordinal = 1L,
  identity_status = NULL,
  legacy_local_path = NULL,
  metadata = NULL
) {
  source_ref_id <- acquisition_catalog_scalar_character(source_ref_id, "source_ref_id")
  native_attachment_id <- acquisition_catalog_optional_character(native_attachment_id, "native_attachment_id")
  attachment_url <- acquisition_catalog_optional_character(attachment_url, "attachment_url")
  section <- acquisition_catalog_optional_character(section, "section")
  legacy_local_path <- acquisition_catalog_optional_character(legacy_local_path, "legacy_local_path")
  ordinal <- acquisition_catalog_scalar_integer(ordinal, "ordinal", lower = 1L, upper = 100000000L)
  if (is.null(attachment_key)) {
    attachment_key <- if (!is.na(native_attachment_id)) {
      paste0("native:", native_attachment_id)
    } else {
      paste0("ambiguous:", section %||% "default", ":", ordinal)
    }
  }
  attachment_key <- acquisition_catalog_scalar_character(attachment_key, "attachment_key")
  identity_status <- identity_status %||% if (is.na(native_attachment_id)) "ambiguous" else "identified"
  identity_status <- acquisition_catalog_scalar_character(identity_status, "identity_status")
  if (!identity_status %in% c("identified", "ambiguous")) {
    acquisition_catalog_abort("{.arg identity_status} must be `identified` or `ambiguous`.")
  }
  document_ref_id <- acquisition_catalog_id("document", source_ref_id, attachment_key)
  source <- acquisition_catalog_one(
    conn,
    "SELECT source_ref_id FROM acquisition_sources WHERE source_ref_id = ?",
    list(source_ref_id)
  )
  if (is.null(source)) {
    acquisition_catalog_abort("The source reference must be registered before its documents.")
  }
  existing <- acquisition_catalog_one(
    conn,
    "SELECT * FROM acquisition_documents WHERE document_ref_id = ?",
    list(document_ref_id)
  )
  if (!is.null(existing)) {
    same <- identical(as.character(existing$source_ref_id[[1L]]), source_ref_id) &&
      identical(as.character(existing$attachment_key[[1L]]), attachment_key) &&
      identical(if (is.na(existing$native_attachment_id[[1L]])) NA_character_ else as.character(existing$native_attachment_id[[1L]]), native_attachment_id) &&
      identical(if (is.na(existing$attachment_url[[1L]])) NA_character_ else as.character(existing$attachment_url[[1L]]), attachment_url)
    if (!same) {
      acquisition_catalog_abort(
        "An immutable document reference would be replaced with different source data.",
        class = "planscanR_error_acquisition_immutable"
      )
    }
    return(list(document_ref_id = document_ref_id, status = "replayed"))
  }
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO acquisition_documents (document_ref_id, source_ref_id, native_attachment_id,",
      "attachment_key, attachment_url, section, ordinal, identity_status, legacy_local_path,",
      "metadata_blob, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ),
    params = list(
      document_ref_id,
      source_ref_id,
      native_attachment_id,
      attachment_key,
      attachment_url,
      section,
      ordinal,
      identity_status,
      legacy_local_path,
      if (is.null(metadata)) NA_character_ else acquisition_catalog_blob(metadata),
      acquisition_catalog_now()
    )
  )
  list(document_ref_id = document_ref_id, status = "inserted")
}

#' Register one logical attachment reference.
#'
#' Attachments without a native ID default to `identity_status = "ambiguous"`
#' and are keyed by source and position, so matching URLs cannot collapse
#' unrelated records.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param source_ref_id Registered source reference ID.
#' @param native_attachment_id Optional trusted native attachment ID.
#' @param attachment_key Stable key when no native ID is available.
#' @param attachment_url Optional source URL.
#' @param section Optional source section.
#' @param ordinal One-based source ordering.
#' @param identity_status `identified` or `ambiguous`.
#' @param legacy_local_path Optional legacy path retained as provenance.
#' @param metadata Optional provenance metadata.
#' @return A list with `document_ref_id` and `status`.
#' @export
acquisition_catalog_register_document <- function(
  catalog,
  source_ref_id,
  native_attachment_id = NULL,
  attachment_key = NULL,
  attachment_url = NULL,
  section = NULL,
  ordinal = 1L,
  identity_status = NULL,
  legacy_local_path = NULL,
  metadata = NULL
) {
  acquisition_catalog_transaction(catalog, function(conn) {
    acquisition_catalog_register_document_connection(
      conn,
      source_ref_id,
      native_attachment_id,
      attachment_key,
      attachment_url,
      section,
      ordinal,
      identity_status,
      legacy_local_path,
      metadata
    )
  })
}

acquisition_catalog_register_file_connection <- function(
  conn,
  document_ref_id,
  root_id,
  relative_path,
  sha256 = NULL,
  size_bytes = NULL,
  verification_status = "unverified",
  provenance = "catalog",
  legacy_path = NULL
) {
  document_ref_id <- acquisition_catalog_scalar_character(document_ref_id, "document_ref_id")
  root_id <- acquisition_catalog_scalar_character(root_id, "root_id")
  relative_path <- acquisition_catalog_scalar_character(relative_path, "relative_path")
  if (grepl("(^|/)(\\.\\.?)(/|$)", relative_path) || grepl("^/", relative_path)) {
    acquisition_catalog_abort("{.arg relative_path} must be a portable relative path.")
  }
  sha256 <- acquisition_catalog_optional_character(sha256, "sha256")
  legacy_path <- acquisition_catalog_optional_character(legacy_path, "legacy_path")
  provenance <- acquisition_catalog_scalar_character(provenance, "provenance")
  verification_status <- acquisition_catalog_scalar_character(verification_status, "verification_status")
  if (!verification_status %in% c("unverified", "verified")) {
    acquisition_catalog_abort("{.arg verification_status} must be `unverified` or `verified`.")
  }
  if (is.null(size_bytes) || (length(size_bytes) == 1L && is.na(size_bytes))) {
    size_bytes <- NA_integer_
  } else {
    size_bytes <- acquisition_catalog_scalar_integer(size_bytes, "size_bytes", lower = 0L, upper = .Machine$integer.max)
  }
  document <- acquisition_catalog_one(
    conn,
    "SELECT document_ref_id FROM acquisition_documents WHERE document_ref_id = ?",
    list(document_ref_id)
  )
  if (is.null(document)) {
    acquisition_catalog_abort("The document reference must be registered before its files.")
  }
  file_ref_id <- acquisition_catalog_id("file", document_ref_id, root_id, relative_path)
  existing <- acquisition_catalog_one(
    conn,
    paste(
      "SELECT * FROM acquisition_files WHERE document_ref_id = ?",
      "AND root_id = ? AND relative_path = ?"
    ),
    list(document_ref_id, root_id, relative_path)
  )
  if (!is.null(existing)) {
    if (identical(as.character(existing$verification_status[[1L]]), "verified") &&
      identical(verification_status, "unverified")) {
      # A legacy replay must never downgrade or rewrite a verified claim.
      return(list(file_ref_id = file_ref_id, status = "replayed"))
    }
    if (
      identical(as.character(existing$verification_status[[1L]]), "verified") &&
        identical(verification_status, "verified")
    ) {
      verification_status <- "verified"
    }
    if (
      identical(as.character(existing$verification_status[[1L]]), "verified") &&
        !is.na(existing$sha256[[1L]]) &&
        !is.na(sha256) &&
        !identical(as.character(existing$sha256[[1L]]), sha256)
    ) {
      acquisition_catalog_abort(
        "A verified file reference cannot be replaced by different bytes.",
        class = "planscanR_error_acquisition_immutable"
      )
    }
    DBI::dbExecute(
      conn,
      paste(
        "UPDATE acquisition_files SET sha256 = COALESCE(?, sha256), size_bytes = COALESCE(?, size_bytes),",
        "verification_status = ?, provenance = ?, legacy_path = COALESCE(?, legacy_path), observed_at = ?",
        "WHERE file_ref_id = ?"
      ),
      params = list(
        sha256,
        size_bytes,
        verification_status,
        provenance,
        legacy_path,
        acquisition_catalog_now(),
        as.character(existing$file_ref_id[[1L]])
      )
    )
    return(list(file_ref_id = file_ref_id, status = "replayed"))
  }
  DBI::dbExecute(
    conn,
    paste(
      "INSERT INTO acquisition_files (file_ref_id, document_ref_id, root_id, relative_path,",
      "sha256, size_bytes, verification_status, provenance, legacy_path, observed_at)",
      "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ),
    params = list(
      file_ref_id,
      document_ref_id,
      root_id,
      relative_path,
      sha256,
      size_bytes,
      verification_status,
      provenance,
      legacy_path,
      acquisition_catalog_now()
    )
  )
  list(file_ref_id = file_ref_id, status = "inserted")
}

#' Register a portable file claim for a logical attachment.
#'
#' The path is always relative to the caller-owned `root_id`. Existing legacy
#' claims should use `verification_status = "unverified"` until a later check.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param document_ref_id Registered document reference ID.
#' @param root_id Stable storage-root identifier.
#' @param relative_path Portable path below `root_id`.
#' @param sha256 Optional content digest.
#' @param size_bytes Optional file size.
#' @param verification_status `unverified` or `verified`.
#' @param provenance Non-empty provenance label.
#' @param legacy_path Optional original path retained for audit.
#' @return A list with `file_ref_id` and `status`.
#' @export
acquisition_catalog_register_file <- function(
  catalog,
  document_ref_id,
  root_id,
  relative_path,
  sha256 = NULL,
  size_bytes = NULL,
  verification_status = "unverified",
  provenance = "catalog",
  legacy_path = NULL
) {
  acquisition_catalog_transaction(catalog, function(conn) {
    acquisition_catalog_register_file_connection(
      conn,
      document_ref_id,
      root_id,
      relative_path,
      sha256,
      size_bytes,
      verification_status,
      provenance,
      legacy_path
    )
  })
}

acquisition_catalog_attachment_values <- function(row, column) {
  if (!column %in% names(row)) {
    return(character(0))
  }
  value <- row[[column]]
  if (is.list(value)) {
    value <- value[[1L]]
  }
  if (is.null(value)) {
    return(character(0))
  }
  as.character(value)
}

acquisition_catalog_attachment_rows <- function(row) {
  status <- NULL
  if ("download_status" %in% names(row) && is.list(row$download_status)) {
    status <- row$download_status[[1L]]
    if (!is.data.frame(status) || nrow(status) == 0L) {
      status <- NULL
    }
  }
  rows <- list()
  append_row <- function(url = NA_character_, local_path = NA_character_, section = "default", native_id = NA_character_, sha256 = NA_character_, size_bytes = NA_real_, status = NA_character_) {
    rows[[length(rows) + 1L]] <<- list(
      url = if (is.na(url) || !nzchar(url)) NA_character_ else url,
      local_path = if (is.na(local_path) || !nzchar(local_path)) NA_character_ else local_path,
      section = if (is.na(section) || !nzchar(section)) "default" else section,
      native_id = if (is.na(native_id) || !nzchar(native_id)) NA_character_ else native_id,
      sha256 = if (is.na(sha256) || !nzchar(sha256)) NA_character_ else sha256,
      size_bytes = if (length(size_bytes) == 0L || is.na(size_bytes)) NA_real_ else as.numeric(size_bytes),
      status = if (is.na(status) || !nzchar(status)) NA_character_ else status
    )
  }
  if (!is.null(status)) {
    for (i in seq_len(nrow(status))) {
      pick <- function(name, default = NA_character_) {
        if (!name %in% names(status)) {
          return(default)
        }
        value <- status[[name]][[i]]
        if (length(value) == 0L || is.na(value)) default else as.character(value)
      }
      append_row(
        url = pick("url"),
        local_path = pick("local_path"),
        section = pick("section", "default"),
        native_id = pick("native_attachment_id", pick("attachment_id")),
        sha256 = pick("sha256"),
        size_bytes = suppressWarnings(as.numeric(pick("size_bytes", NA_character_))),
        status = pick("status")
      )
    }
  } else {
    columns <- grep("^attachment_urls($|_)", names(row), value = TRUE)
    if (length(columns) == 0L) {
      columns <- if ("local_path" %in% names(row)) "attachment_urls" else character(0)
    }
    for (column in columns) {
      suffix <- sub("^attachment_urls", "", column)
      section <- if (!nzchar(suffix)) "default" else sub("^_", "", suffix)
      urls <- acquisition_catalog_attachment_values(row, column)
      path_column <- paste0("local_path", suffix)
      paths <- acquisition_catalog_attachment_values(row, path_column)
      if (length(urls) == 0L && length(paths) > 0L) {
        urls <- rep(NA_character_, length(paths))
      }
      if (length(urls) > 0L) {
        for (i in seq_along(urls)) {
          append_row(
            url = urls[[i]],
            local_path = if (i <= length(paths)) paths[[i]] else NA_character_,
            section = section
          )
        }
      }
    }
  }
  if (length(rows) == 0L) {
    return(rows)
  }
  lapply(seq_along(rows), function(i) {
    rows[[i]]$ordinal <- i
    rows[[i]]$identity_status <- if (is.na(rows[[i]]$native_id)) "ambiguous" else "identified"
    rows[[i]]$attachment_key <- if (is.na(rows[[i]]$native_id)) {
      paste0("ambiguous:", rows[[i]]$section, ":", i)
    } else {
      paste0("native:", rows[[i]]$native_id)
    }
    rows[[i]]
  })
}

acquisition_catalog_relative_path <- function(path, root, arg = "file_root") {
  if (is.na(path) || !nzchar(path)) {
    return(NULL)
  }
  path <- as.character(path)
  root <- normalizePath(root, mustWork = FALSE)
  if (!grepl("^/", path)) {
    relative <- path
  } else {
    absolute <- normalizePath(path, mustWork = FALSE)
    prefix <- paste0(root, "/")
    if (!identical(absolute, root) && !startsWith(absolute, prefix)) {
      acquisition_catalog_abort(
        paste0("Absolute file claims must be below {.arg ", arg, "} to remain portable.")
      )
    }
    relative <- if (identical(absolute, root)) "" else substring(absolute, nchar(prefix) + 1L)
  }
  relative <- gsub("\\\\", "/", relative)
  relative <- sub("^\\./", "", relative)
  if (!nzchar(relative) || grepl("(^|/)(\\.\\.?)(/|$)", relative)) {
    acquisition_catalog_abort("Legacy file claims must resolve to a portable relative path.")
  }
  relative
}

acquisition_catalog_import_plan <- function(row, import_source, file_root = NULL, file_root_id = "legacy-root") {
  country <- acquisition_catalog_scalar_character(acquisition_catalog_row_value(row, "country"), "records$country")
  portal <- acquisition_catalog_scalar_character(
    acquisition_catalog_row_value(row, "source_portal", acquisition_catalog_row_value(row, "portal")),
    "records$source_portal"
  )
  native_project_id <- acquisition_catalog_scalar_character(
    acquisition_catalog_row_value(row, "native_project_id", acquisition_catalog_row_value(row, "document_id")),
    "records$document_id"
  )
  source_url <- acquisition_catalog_optional_character(
    acquisition_catalog_row_value(row, "url"),
    "records$url"
  )
  sidecar_path <- acquisition_catalog_row_value(row, "sidecar_path", acquisition_catalog_row_value(row, "legacy_sidecar_path"))
  source_ref_id <- acquisition_catalog_id("source", country, portal, native_project_id)
  attachments <- acquisition_catalog_attachment_rows(row)
  record_metadata <- lapply(names(row), function(name) row[[name]])
  names(record_metadata) <- names(row)
  payload <- acquisition_catalog_digest(list(
    country = country,
    portal = portal,
    native_project_id = native_project_id,
    record = record_metadata,
    attachments = attachments
  ))
  files <- list()
  if (!is.null(file_root) && length(attachments) > 0L) {
    for (attachment in attachments) {
      if (!is.na(attachment$local_path)) {
        relative <- acquisition_catalog_relative_path(attachment$local_path, file_root)
        size <- if (grepl("^/", attachment$local_path) && file.exists(attachment$local_path)) {
          file.info(attachment$local_path)$size
        } else {
          attachment$size_bytes
        }
        files[[length(files) + 1L]] <- list(
          document_key = attachment$attachment_key,
          relative_path = relative,
          sha256 = attachment$sha256,
          size_bytes = size,
          legacy_path = attachment$local_path
        )
      }
    }
  }
  list(
    country = country,
    portal = portal,
    native_project_id = native_project_id,
    source_url = source_url,
    legacy_sidecar_path = sidecar_path,
    source_ref_id = source_ref_id,
    attachments = attachments,
    files = files,
    payload_fingerprint = payload,
    import_row_id = paste(country, portal, native_project_id, sep = "::"),
    import_source = import_source,
    file_root_id = file_root_id,
    metadata = record_metadata
  )
}

#' Import offline planscanR records into the acquisition catalog.
#'
#' With `records = NULL`, the importer calls exported
#' [planscanR::index_cache()] using the supplied cache root. No portal or cache
#' writes occur. Legacy absolute file claims are converted below `file_root`
#' and remain `unverified`; the catalog itself must live outside that root.
#' Replaying an unchanged `(import_source, import_row_id)` is a no-op, while a
#' changed payload is rejected.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param records A data frame from [planscanR::index_cache()], or `NULL` to
#'   call `index_fn`.
#' @param cache_dir Optional cache root passed to `index_fn`.
#' @param index_fn Exported planscanR-compatible index function.
#' @param import_source Stable source namespace for the import ledger.
#' @param file_root Optional legacy file root used to make paths portable.
#' @param file_root_id Stable identifier for `file_root`.
#' @return A list of counts (`imported`, `replayed`, `sources`, `documents`,
#'   `files`) and imported reference IDs.
#' @export
acquisition_catalog_import_records <- function(
  catalog,
  records = NULL,
  cache_dir = NULL,
  index_fn = planscanR::index_cache,
  import_source = "planscanR-index-v1",
  file_root = NULL,
  file_root_id = "legacy-root"
) {
  conn <- acquisition_catalog_connection(catalog)
  import_source <- acquisition_catalog_scalar_character(import_source, "import_source")
  file_root_id <- acquisition_catalog_scalar_character(file_root_id, "file_root_id")
  if (is.null(records)) {
    if (!is.function(index_fn)) {
      acquisition_catalog_abort("{.arg index_fn} must be a function compatible with planscanR::index_cache().")
    }
    records <- index_fn(cache_dir = cache_dir)
  }
  if (!is.data.frame(records)) {
    acquisition_catalog_abort("{.arg records} must be a data frame.")
  }
  if (!is.null(file_root)) {
    file_root <- acquisition_catalog_scalar_character(file_root, "file_root")
    file_root <- normalizePath(file_root, mustWork = FALSE)
    catalog_path <- normalizePath(catalog$path, mustWork = FALSE)
    if (identical(catalog_path, file_root) || startsWith(catalog_path, paste0(file_root, "/"))) {
      acquisition_catalog_abort(
        "The acquisition catalog must live outside the imported cache/file root.",
        class = "planscanR_error_acquisition_catalog_location"
      )
    }
  }
  plans <- lapply(seq_len(nrow(records)), function(i) {
    acquisition_catalog_import_plan(records[i, , drop = FALSE], import_source, file_root, file_root_id)
  })
  row_ids <- vapply(plans, `[[`, character(1), "import_row_id")
  if (anyDuplicated(row_ids)) {
    acquisition_catalog_abort(
      "The imported records contain duplicate source identities; split the import or resolve them first.",
      class = "planscanR_error_acquisition_import_conflict"
    )
  }
  result <- acquisition_catalog_transaction(catalog, function(connection) {
    counts <- list(imported = 0L, replayed = 0L, sources = 0L, documents = 0L, files = 0L)
    source_ids <- character(0)
    document_ids <- character(0)
    file_ids <- character(0)
    for (plan in plans) {
      ledger <- acquisition_catalog_one(
        connection,
        paste(
          "SELECT entity_id, payload_fingerprint FROM acquisition_import_ledger",
          "WHERE import_source = ? AND import_row_id = ?"
        ),
        list(plan$import_source, plan$import_row_id)
      )
      if (!is.null(ledger)) {
        if (!identical(as.character(ledger$payload_fingerprint[[1L]]), plan$payload_fingerprint)) {
          acquisition_catalog_abort(
            paste0("Import row ", plan$import_row_id, " changed since its previous import."),
            class = "planscanR_error_acquisition_import_conflict"
          )
        }
        counts$replayed <- counts$replayed + 1L
        source_ids <- c(source_ids, plan$source_ref_id)
        next
      }
      source <- acquisition_catalog_register_source_connection(
        connection,
        plan$country,
        plan$portal,
        plan$native_project_id,
        plan$source_url,
        plan$legacy_sidecar_path,
        plan$metadata
      )
      counts$sources <- counts$sources + as.integer(identical(source$status, "inserted"))
      source_ids <- c(source_ids, source$source_ref_id)
      for (attachment in plan$attachments) {
        document <- acquisition_catalog_register_document_connection(
          connection,
          source$source_ref_id,
          attachment$native_id,
          attachment$attachment_key,
          attachment$url,
          attachment$section,
          attachment$ordinal,
          attachment$identity_status,
          attachment$local_path,
          plan$metadata
        )
        counts$documents <- counts$documents + as.integer(identical(document$status, "inserted"))
        document_ids <- c(document_ids, document$document_ref_id)
        file_plan <- Filter(function(x) identical(x$document_key, attachment$attachment_key), plan$files)
        for (file_claim in file_plan) {
          file <- acquisition_catalog_register_file_connection(
            connection,
            document$document_ref_id,
            plan$file_root_id,
            file_claim$relative_path,
            file_claim$sha256,
            file_claim$size_bytes,
            verification_status = "unverified",
            provenance = plan$import_source,
            legacy_path = file_claim$legacy_path
          )
          counts$files <- counts$files + as.integer(identical(file$status, "inserted"))
          file_ids <- c(file_ids, file$file_ref_id)
        }
      }
      DBI::dbExecute(
        connection,
        paste(
          "INSERT INTO acquisition_import_ledger (import_source, import_row_id, entity_type,",
          "entity_id, payload_fingerprint, imported_at) VALUES (?, ?, ?, ?, ?, ?)"
        ),
        params = list(
          plan$import_source,
          plan$import_row_id,
          "source_record",
          source$source_ref_id,
          plan$payload_fingerprint,
          acquisition_catalog_now()
        )
      )
      counts$imported <- counts$imported + 1L
    }
    c(counts, list(source_ref_ids = unique(source_ids), document_ref_ids = unique(document_ids), file_ref_ids = unique(file_ids)))
  })
  result
}

acquisition_catalog_stage_model <- function(source_path, stage_dir) {
  temp_path <- tempfile(".acquisition-model-", tmpdir = stage_dir, fileext = ".tmp")
  if (!file.copy(source_path, temp_path, overwrite = FALSE)) {
    acquisition_catalog_abort("Could not stage a managed copy of the model artifact.")
  }
  list(path = temp_path, sha256 = acquisition_catalog_sha256(temp_path))
}

acquisition_catalog_publish_model <- function(
  staged_path,
  managed_path,
  expected_sha256 = NULL
) {
  staged_sha <- acquisition_catalog_sha256(staged_path)
  if (!is.null(expected_sha256) && !identical(staged_sha, expected_sha256)) {
    acquisition_catalog_abort(
      "The source artifact does not match the immutable model reference.",
      class = "planscanR_error_acquisition_immutable"
    )
  }
  if (file.exists(managed_path)) {
    current_sha <- acquisition_catalog_sha256(managed_path)
    if (identical(current_sha, staged_sha)) {
      unlink(staged_path, force = TRUE)
      return(staged_sha)
    }
    # The path is either an uncommitted stale stage or a corrupt referenced
    # copy. Preserve it before repairing the managed path; never overwrite a
    # different model artifact in place.
    orphan <- paste0(
      managed_path,
      ".orphan-",
      format(Sys.time(), "%Y%m%d%H%M%S"),
      "-",
      Sys.getpid()
    )
    if (!file.rename(managed_path, orphan)) {
      acquisition_catalog_abort("Could not preserve the previous managed model artifact.")
    }
    if (!file.rename(staged_path, managed_path)) {
      file.rename(orphan, managed_path)
      acquisition_catalog_abort("Could not publish the managed model artifact.")
    }
    return(staged_sha)
  }
  if (!file.rename(staged_path, managed_path)) {
    # Another process may have published the same content between the check
    # and rename. Reuse that immutable copy when its digest agrees.
    if (file.exists(managed_path) && identical(acquisition_catalog_sha256(managed_path), staged_sha)) {
      unlink(staged_path, force = TRUE)
      return(staged_sha)
    }
    acquisition_catalog_abort("Could not publish the managed model artifact.")
  }
  staged_sha
}

#' Register an immutable model artifact reference.
#'
#' The artifact is copied into a managed `models/` directory beside the catalog
#' before its SHA-256 reference is recorded, so the selection reference retains
#' a readable model even if the caller later removes the source path.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param model_id Immutable model identifier.
#' @param artifact_path Existing model artifact path.
#' @param config_id Configuration/policy identity.
#' @param input_snapshot_id Training input snapshot identity.
#' @param metadata Optional provenance metadata.
#' @param created_by Optional trusted creator ID.
#' @return A list containing the model reference and `status`.
#' @export
acquisition_catalog_register_model <- function(
  catalog,
  model_id,
  artifact_path,
  config_id,
  input_snapshot_id,
  metadata = NULL,
  created_by = NULL
) {
  acquisition_catalog_connection(catalog)
  model_id <- acquisition_catalog_scalar_character(model_id, "model_id")
  artifact_path <- acquisition_catalog_scalar_character(artifact_path, "artifact_path")
  config_id <- acquisition_catalog_scalar_character(config_id, "config_id")
  input_snapshot_id <- acquisition_catalog_scalar_character(input_snapshot_id, "input_snapshot_id")
  created_by <- acquisition_catalog_optional_character(created_by, "created_by")
  if (!file.exists(artifact_path) || isTRUE(file.info(artifact_path)$isdir)) {
    acquisition_catalog_abort("{.arg artifact_path} must name an existing file.")
  }
  artifact_path <- normalizePath(artifact_path, mustWork = TRUE)
  target_dir <- file.path(dirname(catalog$path), "models")
  if (!dir.exists(target_dir) && !dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)) {
    acquisition_catalog_abort(paste0("Could not create the managed model directory: ", target_dir))
  }
  existing_model <- acquisition_catalog_one(
    catalog$conn,
    "SELECT artifact_path, artifact_sha256 FROM acquisition_models WHERE model_id = ?",
    list(model_id)
  )
  expected_sha256 <- if (is.null(existing_model)) NULL else as.character(existing_model$artifact_sha256[[1L]])
  managed_path <- if (is.null(existing_model)) {
    NULL
  } else {
    normalizePath(as.character(existing_model$artifact_path[[1L]]), mustWork = FALSE)
  }
  staged <- acquisition_catalog_stage_model(artifact_path, target_dir)
  on.exit(unlink(staged$path, force = TRUE), add = TRUE)
  artifact_sha256 <- staged$sha256
  if (!is.null(expected_sha256)) {
    if (!identical(artifact_sha256, expected_sha256)) {
      acquisition_catalog_abort(
        "The source artifact does not match the immutable model reference.",
        class = "planscanR_error_acquisition_immutable"
      )
    }
  } else {
    managed_path <- normalizePath(
      file.path(target_dir, paste0("model-", artifact_sha256, ".rds")),
      mustWork = FALSE
    )
  }
  acquisition_catalog_publish_model(staged$path, managed_path, expected_sha256 = expected_sha256)
  payload_fingerprint <- acquisition_catalog_digest(list(
    model_id = model_id,
    artifact_sha256 = artifact_sha256,
    config_id = config_id,
    input_snapshot_id = input_snapshot_id,
    metadata = metadata,
    created_by = created_by
  ))
  result <- acquisition_catalog_transaction(catalog, function(conn) {
    existing <- acquisition_catalog_one(
      conn,
      "SELECT * FROM acquisition_models WHERE model_id = ?",
      list(model_id)
    )
    if (!is.null(existing)) {
      if (!identical(as.character(existing$payload_fingerprint[[1L]]), payload_fingerprint)) {
        acquisition_catalog_abort(
          "An immutable model reference cannot be replaced.",
          class = "planscanR_error_acquisition_immutable"
        )
      }
      return(list(
        model_id = model_id,
        artifact_path = as.character(existing$artifact_path[[1L]]),
        source_artifact_path = as.character(existing$source_artifact_path[[1L]]),
        artifact_sha256 = as.character(existing$artifact_sha256[[1L]]),
        status = "replayed"
      ))
    }
    DBI::dbExecute(
      conn,
      paste(
        "INSERT INTO acquisition_models (model_id, artifact_path, source_artifact_path, artifact_sha256,",
        "config_id, input_snapshot_id, metadata_blob, payload_fingerprint, created_by, created_at)",
        "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ),
      params = list(
        model_id,
        managed_path,
        artifact_path,
        artifact_sha256,
        config_id,
        input_snapshot_id,
        if (is.null(metadata)) NA_character_ else acquisition_catalog_blob(metadata),
        payload_fingerprint,
        created_by,
        acquisition_catalog_now()
      )
    )
    list(
      model_id = model_id,
      artifact_path = managed_path,
      source_artifact_path = artifact_path,
      artifact_sha256 = artifact_sha256,
      status = "inserted"
    )
  })
  result
}

#' Register an immutable selection reference.
#'
#' The referenced model must already be registered. Reusing a selection ID with
#' different snapshot, model, or metadata is rejected.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param selection_id Immutable selection ID.
#' @param input_snapshot_id Input corpus snapshot identity.
#' @param model_id Registered model ID.
#' @param metadata Selection threshold/policy metadata.
#' @param submitted_by Optional trusted actor ID.
#' @return A list with `selection_id` and `status`.
#' @export
acquisition_catalog_register_selection <- function(
  catalog,
  selection_id,
  input_snapshot_id,
  model_id,
  metadata = NULL,
  submitted_by = NULL
) {
  acquisition_catalog_connection(catalog)
  selection_id <- acquisition_catalog_scalar_character(selection_id, "selection_id")
  input_snapshot_id <- acquisition_catalog_scalar_character(input_snapshot_id, "input_snapshot_id")
  model_id <- acquisition_catalog_scalar_character(model_id, "model_id")
  submitted_by <- acquisition_catalog_optional_character(submitted_by, "submitted_by")
  payload_fingerprint <- acquisition_catalog_digest(list(
    selection_id = selection_id,
    input_snapshot_id = input_snapshot_id,
    model_id = model_id,
    metadata = metadata,
    submitted_by = submitted_by
  ))
  acquisition_catalog_transaction(catalog, function(conn) {
    model <- acquisition_catalog_one(conn, "SELECT model_id FROM acquisition_models WHERE model_id = ?", list(model_id))
    if (is.null(model)) {
      acquisition_catalog_abort("The selection model must be registered before the selection reference.")
    }
    existing <- acquisition_catalog_one(
      conn,
      "SELECT * FROM acquisition_selections WHERE selection_id = ?",
      list(selection_id)
    )
    if (!is.null(existing)) {
      if (!identical(as.character(existing$payload_fingerprint[[1L]]), payload_fingerprint)) {
        acquisition_catalog_abort(
          "An immutable selection reference cannot be replaced.",
          class = "planscanR_error_acquisition_immutable"
        )
      }
      return(list(selection_id = selection_id, status = "replayed"))
    }
    DBI::dbExecute(
      conn,
      paste(
        "INSERT INTO acquisition_selections (selection_id, input_snapshot_id, model_id, metadata_blob,",
        "payload_fingerprint, submitted_by, submitted_at) VALUES (?, ?, ?, ?, ?, ?, ?)"
      ),
      params = list(
        selection_id,
        input_snapshot_id,
        model_id,
        if (is.null(metadata)) NA_character_ else acquisition_catalog_blob(metadata),
        payload_fingerprint,
        submitted_by,
        acquisition_catalog_now()
      )
    )
    list(selection_id = selection_id, status = "inserted")
  })
}

#' List source references in the acquisition catalog.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param country Optional country filter.
#' @param portal Optional portal filter.
#' @return A data frame of source references.
#' @export
acquisition_catalog_sources <- function(catalog, country = NULL, portal = NULL) {
  conn <- acquisition_catalog_connection(catalog)
  sql <- "SELECT * FROM acquisition_sources"
  params <- list()
  clauses <- character(0)
  if (!is.null(country)) {
    clauses <- c(clauses, "country = ?")
    params <- c(params, list(acquisition_catalog_scalar_character(country, "country")))
  }
  if (!is.null(portal)) {
    clauses <- c(clauses, "portal = ?")
    params <- c(params, list(acquisition_catalog_scalar_character(portal, "portal")))
  }
  if (length(clauses) > 0L) sql <- paste(sql, "WHERE", paste(clauses, collapse = " AND "))
  if (length(params) == 0L) DBI::dbGetQuery(conn, sql) else DBI::dbGetQuery(conn, sql, params = params)
}

#' List document references in the acquisition catalog.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param source_ref_id Optional source filter.
#' @param identity_status Optional `identified`/`ambiguous` filter.
#' @return A data frame of document references.
#' @export
acquisition_catalog_documents <- function(catalog, source_ref_id = NULL, identity_status = NULL) {
  conn <- acquisition_catalog_connection(catalog)
  sql <- "SELECT * FROM acquisition_documents"
  params <- list()
  clauses <- character(0)
  if (!is.null(source_ref_id)) {
    clauses <- c(clauses, "source_ref_id = ?")
    params <- c(params, list(acquisition_catalog_scalar_character(source_ref_id, "source_ref_id")))
  }
  if (!is.null(identity_status)) {
    identity_status <- acquisition_catalog_scalar_character(identity_status, "identity_status")
    if (!identity_status %in% c("identified", "ambiguous")) {
      acquisition_catalog_abort("{.arg identity_status} must be `identified` or `ambiguous`.")
    }
    clauses <- c(clauses, "identity_status = ?")
    params <- c(params, list(identity_status))
  }
  if (length(clauses) > 0L) sql <- paste(sql, "WHERE", paste(clauses, collapse = " AND "))
  if (length(params) == 0L) DBI::dbGetQuery(conn, sql) else DBI::dbGetQuery(conn, sql, params = params)
}

#' List file claims in the acquisition catalog.
#'
#' @param catalog A catalog from [acquisition_catalog_open()].
#' @param document_ref_id Optional document filter.
#' @param verification_status Optional verification filter.
#' @return A data frame of file claims.
#' @export
acquisition_catalog_files <- function(catalog, document_ref_id = NULL, verification_status = NULL) {
  conn <- acquisition_catalog_connection(catalog)
  sql <- "SELECT * FROM acquisition_files"
  params <- list()
  clauses <- character(0)
  if (!is.null(document_ref_id)) {
    clauses <- c(clauses, "document_ref_id = ?")
    params <- c(params, list(acquisition_catalog_scalar_character(document_ref_id, "document_ref_id")))
  }
  if (!is.null(verification_status)) {
    verification_status <- acquisition_catalog_scalar_character(verification_status, "verification_status")
    if (!verification_status %in% c("unverified", "verified")) {
      acquisition_catalog_abort("{.arg verification_status} must be `unverified` or `verified`.")
    }
    clauses <- c(clauses, "verification_status = ?")
    params <- c(params, list(verification_status))
  }
  if (length(clauses) > 0L) sql <- paste(sql, "WHERE", paste(clauses, collapse = " AND "))
  if (length(params) == 0L) DBI::dbGetQuery(conn, sql) else DBI::dbGetQuery(conn, sql, params = params)
}
