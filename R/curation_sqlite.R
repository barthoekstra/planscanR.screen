# SQLite review repository.  This is deliberately a small local-disk backend:
# authored review state has one authoritative store, and all mutations go
# through short transactions on the injected repository contract.

curation_sqlite_schema_version <- 4L

curation_sqlite_require_packages <- function() {
  needed <- c("DBI", "RSQLite")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "The SQLite curation repository needs {cli::qty(missing)} package{?s}: {.pkg {missing}}.",
        i = "Install the optional DBI and RSQLite dependencies and retry."
      ),
      class = "planscanR_error_missing_dep"
    )
  }
  invisible(TRUE)
}

curation_sqlite_abort <- function(
  message,
  class = "planscanR_error_curation_sqlite",
  ...
) {
  rlang::abort(
    message,
    class = unique(c(class, "planscanR_error_curation_repository")),
    ...
  )
}

curation_sqlite_scalar_integer <- function(value, arg, lower = 0L, upper = 60000L) {
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      !is.finite(value) ||
      value != trunc(value) ||
      value < lower ||
      value > upper
  ) {
    curation_sqlite_abort(
      paste0(
        "{.arg ", arg, "} must be an integer between ", lower, " and ", upper, "."
      )
    )
  }
  as.integer(value)
}

curation_sqlite_connection <- function(state) {
  if (isTRUE(state$closed) || !DBI::dbIsValid(state$conn)) {
    curation_sqlite_abort("The SQLite curation repository is closed.")
  }
  state$conn
}

curation_sqlite_is_busy <- function(error) {
  grepl("(database is locked|database table is locked|database is busy|SQLITE_BUSY)",
    conditionMessage(error),
    ignore.case = TRUE
  )
}

curation_sqlite_busy_deadline <- function(timeout_ms) {
  as.numeric(Sys.time()) + (timeout_ms / 1000)
}

curation_sqlite_busy_sleep <- function(deadline) {
  remaining <- deadline - as.numeric(Sys.time())
  if (remaining > 0) {
    Sys.sleep(min(0.01, remaining))
  }
}

curation_sqlite_with_busy_retry <- function(fn, busy_timeout) {
  deadline <- curation_sqlite_busy_deadline(busy_timeout)
  repeat {
    result <- tryCatch(fn(), error = function(error) error)
    if (!inherits(result, "error")) {
      return(result)
    }
    if (!curation_sqlite_is_busy(result) || as.numeric(Sys.time()) >= deadline) {
      curation_sqlite_abort(
        "The SQLite curation database remained busy for the configured bounded timeout.",
        class = if (curation_sqlite_is_busy(result)) {
          "planscanR_error_curation_busy"
        } else {
          "planscanR_error_curation_sqlite"
        },
        parent = result
      )
    }
    curation_sqlite_busy_sleep(deadline)
  }
}

curation_sqlite_with_transaction <- function(conn, fn, busy_timeout = 5000L) {
  deadline <- curation_sqlite_busy_deadline(busy_timeout)
  run_attempt <- function() {
    started <- FALSE
    committed <- FALSE
    on.exit(
      if (isTRUE(started) && !isTRUE(committed) && DBI::dbIsValid(conn)) {
        try(DBI::dbRollback(conn), silent = TRUE)
      },
      add = TRUE
    )
    DBI::dbExecute(conn, "BEGIN IMMEDIATE")
    started <- TRUE
    value <- fn()
    DBI::dbCommit(conn)
    committed <- TRUE
    value
  }
  repeat {
    result <- tryCatch(run_attempt(), error = function(error) error)
    if (!inherits(result, "error")) {
      return(result)
    }
    if (!curation_sqlite_is_busy(result)) {
      stop(result)
    }
    if (as.numeric(Sys.time()) >= deadline) {
      curation_sqlite_abort(
        "The SQLite curation database remained busy for the configured bounded timeout.",
        class = "planscanR_error_curation_busy",
        parent = result
      )
    }
    curation_sqlite_busy_sleep(deadline)
  }
}

curation_sqlite_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
}

curation_sqlite_timestamp <- function(value) {
  if (length(value) == 0L || is.na(value[[1L]])) {
    return(NA_character_)
  }
  parsed <- if (inherits(value, "POSIXct")) {
    as.POSIXct(value[[1L]], tz = "UTC")
  } else {
    curation_parse_timestamp(value[[1L]])
  }
  if (is.na(parsed)) {
    curation_sqlite_abort("Review timestamps must be valid POSIXct or ISO-8601 values.")
  }
  format(parsed, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
}

curation_sqlite_set_pragmas <- function(conn, busy_timeout) {
  # Set the finite busy handler before changing journal mode: two independent
  # processes may initialize the same new file at the same time.
  DBI::dbExecute(conn, paste0("PRAGMA busy_timeout = ", busy_timeout))
  journal_mode <- curation_sqlite_with_busy_retry(
    function() tolower(as.character(DBI::dbGetQuery(conn, "PRAGMA journal_mode = WAL")[[1L]])),
    busy_timeout
  )
  if (!identical(journal_mode, "wal")) {
    curation_sqlite_abort(
      "The SQLite curation repository requires a file-backed WAL database."
    )
  }
  DBI::dbExecute(conn, "PRAGMA foreign_keys = ON")
  foreign_keys <- as.integer(DBI::dbGetQuery(conn, "PRAGMA foreign_keys")[[1L]])
  if (!identical(foreign_keys, 1L)) {
    curation_sqlite_abort("SQLite foreign-key enforcement could not be enabled.")
  }
  invisible(list(journal_mode = journal_mode, foreign_keys = TRUE))
}

curation_sqlite_user_version <- function(conn) {
  value <- suppressWarnings(as.numeric(DBI::dbGetQuery(conn, "PRAGMA user_version")[[1L]]))
  if (length(value) != 1L || !is.finite(value) || value < 0 || value != trunc(value)) {
    curation_sqlite_abort("SQLite user_version is not a finite integer.")
  }
  as.integer(value)
}

curation_sqlite_apply_migration <- function(conn, version) {
  if (identical(version, 1L)) {
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS schema_migrations (",
        "version INTEGER PRIMARY KEY,",
        "applied_at TEXT NOT NULL",
        ")"
      )
    )
  } else if (identical(version, 2L)) {
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS curation_reviews (",
        "project_id TEXT NOT NULL,",
        "corpus_id TEXT NOT NULL,",
        "config_id TEXT NOT NULL,",
        "country TEXT NOT NULL,",
        "document_id TEXT NOT NULL,",
        "reviewer_id TEXT NOT NULL,",
        "decision TEXT,",
        "source TEXT NOT NULL,",
        "note TEXT,",
        "reviewed_at TEXT,",
        "sidecar_path TEXT,",
        "revision INTEGER NOT NULL CHECK (revision >= 1),",
        "deleted INTEGER NOT NULL CHECK (deleted IN (0, 1)),",
        "import_source TEXT,",
        "import_row_id TEXT,",
        "import_fingerprint TEXT,",
        "CHECK (deleted = 1 OR (decision IS NOT NULL AND length(trim(decision)) > 0)),",
        "PRIMARY KEY (project_id, corpus_id, country, document_id, reviewer_id)",
        ")"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE INDEX IF NOT EXISTS curation_reviews_scope_idx",
        "ON curation_reviews (project_id, corpus_id, country, document_id)"
      )
    )
  } else if (identical(version, 3L)) {
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS curation_import_ledger (",
        "project_id TEXT NOT NULL,",
        "corpus_id TEXT NOT NULL,",
        "import_source TEXT NOT NULL,",
        "import_row_id TEXT NOT NULL,",
        "country TEXT NOT NULL,",
        "document_id TEXT NOT NULL,",
        "reviewer_id TEXT NOT NULL,",
        "payload_fingerprint TEXT NOT NULL,",
        "import_fingerprint TEXT,",
        "config_id TEXT NOT NULL,",
        "imported_at TEXT NOT NULL,",
        "PRIMARY KEY (project_id, corpus_id, import_source, import_row_id),",
        "FOREIGN KEY (project_id, corpus_id, country, document_id, reviewer_id)",
        "REFERENCES curation_reviews(project_id, corpus_id, country, document_id, reviewer_id)",
        ")"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE INDEX IF NOT EXISTS curation_import_ledger_review_idx",
        "ON curation_import_ledger (project_id, corpus_id, country, document_id, reviewer_id)"
      )
    )
  } else if (identical(version, 4L)) {
    DBI::dbExecute(
      conn,
      "ALTER TABLE curation_reviews ADD COLUMN modified_at TEXT"
    )
    DBI::dbExecute(
      conn,
      paste(
        "UPDATE curation_reviews SET modified_at = COALESCE(reviewed_at, ?)",
        "WHERE modified_at IS NULL"
      ),
      params = list(curation_sqlite_now())
    )
  } else {
    curation_sqlite_abort("Unknown SQLite curation migration requested.")
  }
}

curation_sqlite_migrate <- function(conn, busy_timeout = 5000L) {
  user_version <- curation_sqlite_user_version(conn)
  if (user_version > curation_sqlite_schema_version) {
    curation_sqlite_abort(
      paste0(
        "SQLite curation schema version ", user_version,
        " is newer than this package supports (", curation_sqlite_schema_version, ")."
      ),
      class = "planscanR_error_curation_schema"
    )
  }

  curation_sqlite_with_transaction(conn, function() {
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS schema_migrations (",
        "version INTEGER PRIMARY KEY,",
        "applied_at TEXT NOT NULL",
        ")"
      )
    )
    migrations <- DBI::dbGetQuery(
      conn,
      "SELECT version FROM schema_migrations ORDER BY version"
    )
    versions <- if (nrow(migrations) == 0L) integer(0) else as.integer(migrations$version)
    if (any(!is.finite(versions)) || any(versions < 1L) || any(versions > curation_sqlite_schema_version)) {
      curation_sqlite_abort(
        "SQLite curation schema_migrations contains an unsupported version.",
        class = "planscanR_error_curation_schema"
      )
    }
    for (version in seq_len(curation_sqlite_schema_version)) {
      if (!version %in% versions) {
        curation_sqlite_apply_migration(conn, version)
        DBI::dbExecute(
          conn,
          "INSERT INTO schema_migrations(version, applied_at) VALUES (?, ?)",
          params = list(as.integer(version), curation_sqlite_now())
        )
      }
    }
    DBI::dbExecute(
      conn,
      paste0("PRAGMA user_version = ", curation_sqlite_schema_version)
    )
    invisible(TRUE)
  }, busy_timeout = busy_timeout)
  invisible(curation_sqlite_schema_version)
}

curation_sqlite_params <- function(values) {
  unname(as.list(values))
}

curation_sqlite_key_where <- function(scope, key) {
  list(
    project_id = scope$project_id,
    corpus_id = scope$corpus_id,
    country = key$country,
    document_id = key$document_id,
    reviewer_id = key$reviewer_id
  )
}

curation_sqlite_review_select <- function(conn, scope, key) {
  values <- curation_sqlite_key_where(scope, key)
  raw <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT project_id, corpus_id, config_id, country, document_id, reviewer_id,",
      "decision, source, note, reviewed_at, sidecar_path, revision, deleted,",
      "import_source, import_row_id, import_fingerprint, modified_at",
      "FROM curation_reviews",
      "WHERE project_id = ? AND corpus_id = ? AND country = ? AND document_id = ? AND reviewer_id = ?"
    ),
    params = curation_sqlite_params(values)
  )
  if (nrow(raw) == 0L) {
    return(NULL)
  }
  curation_sqlite_decode_rows(raw[1L, , drop = FALSE])
}

curation_sqlite_decode_rows <- function(raw) {
  if (!is.data.frame(raw) || nrow(raw) == 0L) {
    return(data.frame(
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
      config_id = character(0),
      import_source = character(0),
      import_row_id = character(0),
      import_fingerprint = character(0),
      modified_at = character(0),
      stringsAsFactors = FALSE
    ))
  }
  out <- data.frame(
    country = as.character(raw$country),
    document_id = as.character(raw$document_id),
    reviewer_id = as.character(raw$reviewer_id),
    decision = as.character(raw$decision),
    source = as.character(raw$source),
    note = as.character(raw$note),
    reviewed_at = as.POSIXct(rep(NA_real_, nrow(raw)), origin = "1970-01-01", tz = "UTC"),
    sidecar_path = as.character(raw$sidecar_path),
    revision = as.integer(raw$revision),
    deleted = as.logical(as.integer(raw$deleted)),
    config_id = as.character(raw$config_id),
    import_source = as.character(raw$import_source),
    import_row_id = as.character(raw$import_row_id),
    import_fingerprint = as.character(raw$import_fingerprint),
    modified_at = as.character(raw$modified_at),
    stringsAsFactors = FALSE
  )
  has_time <- !is.na(raw$reviewed_at) & nzchar(as.character(raw$reviewed_at))
  if (any(has_time)) {
    out$reviewed_at[has_time] <- curation_parse_timestamp(as.character(raw$reviewed_at[has_time]))
  }
  out
}

curation_sqlite_select_rows <- function(conn, scope, filter, limit, offset) {
  clauses <- c("project_id = ?", "corpus_id = ?")
  params <- list(scope$project_id, scope$corpus_id)
  scalar_filters <- c("country", "document_id", "reviewer_id", "source", "config_id")
  for (name in scalar_filters) {
    value <- filter[[name]]
    if (is.null(value)) {
      next
    }
    if (!is.character(value) || length(value) == 0L || anyNA(value) || any(!nzchar(value))) {
      curation_sqlite_abort(paste0("Review filter {.field ", name, "} must contain non-empty text."))
    }
    clauses <- c(clauses, if (length(value) == 1L) {
      paste0(name, " = ?")
    } else {
      paste0(name, " IN (", paste(rep("?", length(value)), collapse = ", "), ")")
    })
    params <- c(params, as.list(value))
  }
  if (!is.null(filter$deleted)) {
    if (!is.logical(filter$deleted) || length(filter$deleted) != 1L || is.na(filter$deleted)) {
      curation_sqlite_abort("Review filter {.field deleted} must be one non-missing logical value.")
    }
    clauses <- c(clauses, "deleted = ?")
    params <- c(params, list(as.integer(filter$deleted)))
  } else if (identical(filter$include_deleted, FALSE)) {
    clauses <- c(clauses, "deleted = 0")
  }
  query <- paste(
    "SELECT project_id, corpus_id, config_id, country, document_id, reviewer_id,",
    "decision, source, note, reviewed_at, sidecar_path, revision, deleted,",
    "import_source, import_row_id, import_fingerprint, modified_at",
    "FROM curation_reviews WHERE", paste(clauses, collapse = " AND "),
    "ORDER BY country, document_id, reviewer_id"
  )
  if (!is.null(limit)) {
    query <- paste0(query, " LIMIT ", as.integer(limit), " OFFSET ", as.integer(offset))
  } else if (offset > 0L) {
    query <- paste0(query, " LIMIT -1 OFFSET ", as.integer(offset))
  }
  raw <- DBI::dbGetQuery(conn, query, params = params)
  curation_sqlite_decode_rows(raw)
}

curation_sqlite_row_value <- function(row, name, default = NA_character_) {
  if (!name %in% names(row)) {
    return(default)
  }
  value <- row[[name]][[1L]]
  if (length(value) == 0L) default else value
}

curation_sqlite_build_db_row <- function(
  row,
  scope,
  revision,
  existing = NULL,
  tombstone = FALSE,
  preserve_missing_time = FALSE
) {
  get <- function(name, default = NA_character_) {
    value <- curation_sqlite_row_value(row, name, default)
    if (length(value) == 1L && is.na(value) && !is.null(existing)) {
      old <- curation_sqlite_row_value(existing, name, default)
      if (!(length(old) == 1L && is.na(old))) {
        return(old)
      }
    }
    value
  }
  deleted <- isTRUE(tombstone) || isTRUE(curation_sqlite_row_value(row, "deleted", FALSE))
  reviewed_at <- curation_sqlite_row_value(row, "reviewed_at", NA)
  if (is.na(reviewed_at) && isTRUE(tombstone) && !is.null(existing)) {
    reviewed_at <- curation_sqlite_row_value(existing, "reviewed_at", NA)
  }
  if (is.na(reviewed_at) && !isTRUE(preserve_missing_time) && !isTRUE(tombstone)) {
    reviewed_at <- Sys.time()
  }
  if (isTRUE(tombstone) && is.null(existing)) {
    reviewed_at <- Sys.time()
  }
  decision <- if (deleted) NA_character_ else as.character(get("decision"))
  source <- if (isTRUE(tombstone)) {
    previous_source <- curation_sqlite_row_value(existing, "source")
    if (is.null(existing) || is.na(previous_source) || !nzchar(previous_source)) {
      "clear"
    } else {
      as.character(previous_source)
    }
  } else {
    as.character(get("source"))
  }
  out <- list(
    project_id = scope$project_id,
    corpus_id = scope$corpus_id,
    config_id = scope$config_id,
    country = as.character(get("country")),
    document_id = as.character(get("document_id")),
    reviewer_id = as.character(get("reviewer_id")),
    decision = decision,
    source = source,
    note = get("note"),
    reviewed_at = curation_sqlite_timestamp(reviewed_at),
    sidecar_path = get("sidecar_path"),
    revision = as.integer(revision),
    deleted = as.integer(deleted),
    import_source = get("import_source"),
    import_row_id = get("import_row_id"),
    import_fingerprint = get("import_fingerprint"),
    modified_at = curation_sqlite_now()
  )
  out
}

curation_sqlite_write_row <- function(conn, row, existing = NULL) {
  values <- unname(row)
  if (is.null(existing)) {
    DBI::dbExecute(
      conn,
      paste(
        "INSERT INTO curation_reviews (project_id, corpus_id, config_id, country,",
        "document_id, reviewer_id, decision, source, note, reviewed_at, sidecar_path,",
        "revision, deleted, import_source, import_row_id, import_fingerprint, modified_at)",
        "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ),
      params = curation_sqlite_params(values)
    )
  } else {
    DBI::dbExecute(
      conn,
      paste(
        "UPDATE curation_reviews SET config_id = ?, decision = ?, source = ?, note = ?,",
        "reviewed_at = ?, sidecar_path = ?, revision = ?, deleted = ?, import_source = ?,",
        "import_row_id = ?, import_fingerprint = ?, modified_at = ?",
        "WHERE project_id = ? AND corpus_id = ? AND country = ? AND document_id = ? AND reviewer_id = ?"
      ),
      params = curation_sqlite_params(c(
        row[c("config_id", "decision", "source", "note", "reviewed_at", "sidecar_path", "revision", "deleted", "import_source", "import_row_id", "import_fingerprint", "modified_at")],
        row[c("project_id", "corpus_id", "country", "document_id", "reviewer_id")]
      ))
    )
  }
  invisible(row)
}

curation_sqlite_revision_conflict <- function(key, expected, current) {
  curation_sqlite_abort(
    paste0(
      "Review revision conflict for ", key$country, "/", key$document_id,
      " (reviewer ", key$reviewer_id, "): expected ", expected,
      ", current ", current, ". Refresh before saving."
    ),
    class = "planscanR_error_curation_conflict",
    expected_revision = as.integer(expected),
    current_revision = as.integer(current),
    key = key
  )
}

curation_sqlite_duplicate_abort <- function(what) {
  curation_sqlite_abort(
    paste0("Duplicate ", what, " values are not allowed in one atomic operation."),
    class = "planscanR_error_curation_duplicate"
  )
}

curation_sqlite_assert_role <- function(actor, roles, operation) {
  if (!any(actor$roles %in% roles)) {
    curation_sqlite_abort(
      paste0("Trusted actor is not authorized to ", operation, "."),
      class = "planscanR_error_curation_authorization"
    )
  }
  invisible(TRUE)
}

curation_sqlite_assert_write_actor <- function(actor, reviewer_ids, import = FALSE) {
  if (isTRUE(import)) {
    curation_sqlite_assert_role(
      actor,
      c("admin", "curation_admin", "review_admin", "importer", "curation_import"),
      "import review rows"
    )
    return(invisible(TRUE))
  }
  curation_sqlite_assert_role(
    actor,
    c("reviewer", "admin", "curation_admin", "review_admin"),
    "edit review decisions"
  )
  reviewer_ids <- unique(as.character(reviewer_ids))
  if (all(reviewer_ids %in% actor$id)) {
    return(invisible(TRUE))
  }
  curation_sqlite_assert_role(
    actor,
    c("admin", "curation_admin", "review_admin"),
    "edit another reviewer's decision"
  )
}

curation_sqlite_result <- function(row, no_op = FALSE) {
  list(
    ok = TRUE,
    key = curation_review_key(row$country, row$document_id, row$reviewer_id),
    revision = as.integer(row$revision),
    deleted = identical(as.integer(row$deleted), 1L),
    no_op = isTRUE(no_op)
  )
}

curation_sqlite_check_revision <- function(expected, current, key) {
  if (!identical(as.integer(expected), as.integer(current))) {
    curation_sqlite_revision_conflict(key, expected, current)
  }
  invisible(TRUE)
}

curation_sqlite_upsert <- function(state, scope, decision, actor, expected_revision) {
  conn <- curation_sqlite_connection(state)
  curation_check_scope(scope)
  curation_check_actor(actor)
  curation_sqlite_assert_write_actor(actor, decision$reviewer_id)
  key <- curation_review_key(decision$country, decision$document_id, decision$reviewer_id)
  curation_sqlite_with_transaction(conn, function() {
    current <- curation_sqlite_review_select(conn, scope, key)
    current_revision <- if (is.null(current)) 0L else current$revision[[1L]]
    curation_sqlite_check_revision(expected_revision, current_revision, key)
    next_revision <- if (is.null(current)) 1L else current_revision + 1L
    if (next_revision > .Machine$integer.max) {
      curation_sqlite_abort("Review revision would exceed the supported integer range.")
    }
    db_row <- curation_sqlite_build_db_row(decision, scope, next_revision, current)
    curation_sqlite_write_row(conn, db_row, current)
    curation_sqlite_result(
      data.frame(
        country = db_row$country,
        document_id = db_row$document_id,
        reviewer_id = db_row$reviewer_id,
        revision = db_row$revision,
        deleted = db_row$deleted,
        stringsAsFactors = FALSE
      )
    )
  }, busy_timeout = state$busy_timeout)
}

curation_sqlite_clear <- function(state, scope, key, actor, expected_revision) {
  conn <- curation_sqlite_connection(state)
  curation_check_scope(scope)
  curation_check_actor(actor)
  curation_sqlite_assert_write_actor(actor, key$reviewer_id)
  curation_sqlite_with_transaction(conn, function() {
    current <- curation_sqlite_review_select(conn, scope, key)
    current_revision <- if (is.null(current)) 0L else current$revision[[1L]]
    curation_sqlite_check_revision(expected_revision, current_revision, key)
    next_revision <- if (is.null(current)) 1L else current_revision + 1L
    if (next_revision > .Machine$integer.max) {
      curation_sqlite_abort("Review revision would exceed the supported integer range.")
    }
    tombstone <- data.frame(
      country = key$country,
      document_id = key$document_id,
      reviewer_id = key$reviewer_id,
      decision = NA_character_,
      source = "clear",
      stringsAsFactors = FALSE
    )
    db_row <- curation_sqlite_build_db_row(
      tombstone, scope, next_revision, current,
      tombstone = TRUE
    )
    curation_sqlite_write_row(conn, db_row, current)
    curation_sqlite_result(
      data.frame(
        country = db_row$country,
        document_id = db_row$document_id,
        reviewer_id = db_row$reviewer_id,
        revision = db_row$revision,
        deleted = db_row$deleted,
        stringsAsFactors = FALSE
      )
    )
  }, busy_timeout = state$busy_timeout)
}

curation_sqlite_bulk_upsert <- function(state, scope, decisions, actor, expected_revision) {
  curation_check_scope(scope)
  curation_check_actor(actor)
  if (nrow(decisions) == 0L) {
    return(list(ok = TRUE, results = data.frame(), revision = integer(0), deleted = logical(0)))
  }
  key_strings <- paste(decisions$country, decisions$document_id, decisions$reviewer_id, sep = "\r")
  if (anyDuplicated(key_strings)) {
    curation_sqlite_duplicate_abort("review key")
  }
  curation_sqlite_assert_write_actor(actor, decisions$reviewer_id)
  conn <- curation_sqlite_connection(state)
  curation_sqlite_with_transaction(conn, function() {
    keys <- lapply(seq_len(nrow(decisions)), function(i) {
      curation_review_key(decisions$country[[i]], decisions$document_id[[i]], decisions$reviewer_id[[i]])
    })
    current <- lapply(keys, function(key) curation_sqlite_review_select(conn, scope, key))
    current_revisions <- vapply(current, function(row) {
      if (is.null(row)) 0L else as.integer(row$revision[[1L]])
    }, integer(1))
    for (i in seq_along(keys)) {
      curation_sqlite_check_revision(expected_revision[[i]], current_revisions[[i]], keys[[i]])
    }
    results <- vector("list", length(keys))
    for (i in seq_along(keys)) {
      next_revision <- if (is.null(current[[i]])) 1L else current_revisions[[i]] + 1L
      if (next_revision > .Machine$integer.max) {
        curation_sqlite_abort("Review revision would exceed the supported integer range.")
      }
      db_row <- curation_sqlite_build_db_row(
        decisions[i, , drop = FALSE], scope, next_revision, current[[i]]
      )
      curation_sqlite_write_row(conn, db_row, current[[i]])
      results[[i]] <- data.frame(
        country = db_row$country,
        document_id = db_row$document_id,
        reviewer_id = db_row$reviewer_id,
        revision = db_row$revision,
        deleted = db_row$deleted,
        stringsAsFactors = FALSE
      )
    }
    result_rows <- do.call(rbind, results)
    list(
      ok = TRUE,
      results = result_rows,
      revision = as.integer(result_rows$revision),
      deleted = as.logical(result_rows$deleted)
    )
  }, busy_timeout = state$busy_timeout)
}

curation_sqlite_canonical_import_payload <- function(row) {
  fields <- c(
    "country", "document_id", "reviewer_id", "decision", "source", "note",
    "reviewed_at", "sidecar_path", "deleted", "import_source", "import_row_id",
    "import_fingerprint"
  )
  out <- stats::setNames(vector("list", length(fields)), fields)
  for (field in fields) {
    value <- curation_sqlite_row_value(row, field, NA_character_)
    if (identical(field, "reviewed_at")) {
      value <- curation_sqlite_timestamp(value)
    } else if (identical(field, "deleted")) {
      value <- as.logical(value)
    } else {
      value <- as.character(value)
    }
    out[[field]] <- value
  }
  out
}

curation_sqlite_payload_fingerprint <- function(row) {
  payload <- serialize(curation_sqlite_canonical_import_payload(row), NULL, version = 2)
  file <- tempfile("planscanR-curation-fingerprint-")
  on.exit(unlink(file, force = TRUE), add = TRUE)
  writeBin(payload, file)
  unname(as.character(tools::md5sum(file)))
}

curation_sqlite_ledger_select <- function(conn, scope, row) {
  result <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT project_id, corpus_id, import_source, import_row_id, country,",
      "document_id, reviewer_id, payload_fingerprint, import_fingerprint, config_id",
      "FROM curation_import_ledger",
      "WHERE project_id = ? AND corpus_id = ? AND import_source = ? AND import_row_id = ?"
    ),
    params = list(
      scope$project_id,
      scope$corpus_id,
      as.character(row$import_source[[1L]]),
      as.character(row$import_row_id[[1L]])
    )
  )
  if (nrow(result) == 0L) NULL else result[1L, , drop = FALSE]
}

curation_sqlite_import_conflict <- function(message) {
  curation_sqlite_abort(
    message,
    class = "planscanR_error_curation_import_conflict"
  )
}

curation_sqlite_import <- function(state, scope, rows, actor, expected_revision) {
  curation_check_scope(scope)
  curation_check_actor(actor)
  if (nrow(rows) == 0L) {
    return(list(ok = TRUE, results = data.frame(), revision = integer(0), deleted = logical(0)))
  }
  curation_sqlite_assert_write_actor(actor, rows$reviewer_id, import = TRUE)
  event_keys <- paste(rows$import_source, rows$import_row_id, sep = "\r")
  if (anyDuplicated(event_keys)) {
    curation_sqlite_duplicate_abort("import source/row identity")
  }
  review_keys <- paste(rows$country, rows$document_id, rows$reviewer_id, sep = "\r")
  if (anyDuplicated(review_keys)) {
    curation_sqlite_duplicate_abort("review key")
  }
  conn <- curation_sqlite_connection(state)
  curation_sqlite_with_transaction(conn, function() {
    results <- vector("list", nrow(rows))
    for (i in seq_len(nrow(rows))) {
      row <- rows[i, , drop = FALSE]
      key <- curation_review_key(row$country, row$document_id, row$reviewer_id)
      fingerprint <- curation_sqlite_payload_fingerprint(row)
      ledger <- curation_sqlite_ledger_select(conn, scope, row)
      current <- curation_sqlite_review_select(conn, scope, key)
      if (!is.null(ledger)) {
        if (
          !identical(as.character(ledger$payload_fingerprint[[1L]]), fingerprint) ||
            !identical(as.character(ledger$country[[1L]]), key$country) ||
            !identical(as.character(ledger$document_id[[1L]]), key$document_id) ||
            !identical(as.character(ledger$reviewer_id[[1L]]), key$reviewer_id)
        ) {
          curation_sqlite_import_conflict(
            paste0(
              "Imported row ", row$import_source[[1L]], "/", row$import_row_id[[1L]],
              " changed after its first import."
            )
          )
        }
        if (is.null(current)) {
          curation_sqlite_import_conflict(
            "The import ledger points to a missing review row; refusing to recreate it."
          )
        }
        results[[i]] <- data.frame(
          country = current$country,
          document_id = current$document_id,
          reviewer_id = current$reviewer_id,
          revision = current$revision,
          deleted = current$deleted,
          no_op = TRUE,
          stringsAsFactors = FALSE
        )
        next
      }
      current_revision <- if (is.null(current)) 0L else as.integer(current$revision[[1L]])
      if (!is.null(current)) {
        curation_sqlite_import_conflict(
          paste0(
            "Import row ", row$import_source[[1L]], "/", row$import_row_id[[1L]],
            " would overwrite an existing live review."
          )
        )
      }
      curation_sqlite_check_revision(expected_revision[[i]], current_revision, key)
      db_row <- curation_sqlite_build_db_row(
        row, scope, 1L,
        existing = NULL, preserve_missing_time = TRUE
      )
      curation_sqlite_write_row(conn, db_row)
      DBI::dbExecute(
        conn,
        paste(
          "INSERT INTO curation_import_ledger (project_id, corpus_id, import_source,",
          "import_row_id, country, document_id, reviewer_id, payload_fingerprint,",
          "import_fingerprint, config_id, imported_at)",
          "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        ),
        params = list(
          scope$project_id,
          scope$corpus_id,
          as.character(row$import_source[[1L]]),
          as.character(row$import_row_id[[1L]]),
          key$country,
          key$document_id,
          key$reviewer_id,
          fingerprint,
          if (is.na(curation_sqlite_row_value(row, "import_fingerprint"))) {
            NA_character_
          } else {
            as.character(curation_sqlite_row_value(row, "import_fingerprint"))
          },
          scope$config_id,
          curation_sqlite_now()
        )
      )
      results[[i]] <- data.frame(
        country = db_row$country,
        document_id = db_row$document_id,
        reviewer_id = db_row$reviewer_id,
        revision = db_row$revision,
        deleted = db_row$deleted,
        no_op = FALSE,
        stringsAsFactors = FALSE
      )
    }
    result_rows <- do.call(rbind, results)
    list(
      ok = TRUE,
      results = result_rows,
      revision = as.integer(result_rows$revision),
      deleted = as.logical(result_rows$deleted)
    )
  }, busy_timeout = state$busy_timeout)
}

curation_sqlite_export <- function(state, scope, actor, format) {
  curation_check_scope(scope)
  curation_check_actor(actor)
  format <- tolower(format)
  if (!format %in% c("csv", "data.frame", "dataframe", "tibble")) {
    curation_sqlite_abort("SQLite curation export format must be csv or data.frame.")
  }
  rows <- curation_sqlite_select_rows(
    curation_sqlite_connection(state),
    scope,
    list(),
    limit = NULL,
    offset = 0L
  )
  if (identical(format, "csv")) {
    rows$reviewed_at <- if (nrow(rows) == 0L) {
      character(0)
    } else {
      vapply(rows$reviewed_at, function(value) {
        if (is.na(value)) NA_character_ else format(value, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
      }, character(1))
    }
  }
  rows
}

curation_sqlite_write_csv <- function(state, scope, actor, path) {
  path <- curation_scalar_character(path, "path", class = "planscanR_error_curation_repository")
  path <- normalizePath(path, mustWork = FALSE)
  protected <- c(
    state$path,
    paste0(state$path, "-wal"),
    paste0(state$path, "-shm")
  )
  if (path %in% normalizePath(protected, mustWork = FALSE)) {
    curation_sqlite_abort(
      "The CSV export path must not overwrite the SQLite database or its sidecar files."
    )
  }
  parent <- dirname(path)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    curation_sqlite_abort("The CSV export directory could not be created.")
  }
  output <- curation_sqlite_export(state, scope, actor, "csv")
  temp <- tempfile("planscanR-curation-export-", tmpdir = parent)
  on.exit(unlink(temp, force = TRUE), add = TRUE)
  utils::write.csv(output, temp, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  if (!file.rename(temp, path)) {
    curation_sqlite_abort("The CSV export could not be published atomically.")
  }
  invisible(path)
}

#' Open a file-backed SQLite review repository.
#'
#' The database is initialized on local disk with WAL mode, foreign keys, a
#' bounded busy timeout, and transactional numbered migrations. `config_id` is
#' retained as provenance but the review key is project/corpus/country/document/
#' reviewer, so a policy revision can continue to read authored decisions.
#'
#' @param path Explicit path to a local SQLite database file.
#' @param busy_timeout Milliseconds SQLite may wait for a competing writer.
#' @return A `planscanR_curation_review_repository` with SQLite metadata and a
#'   `close()` method.
#' @export
curation_review_repository_sqlite <- function(path, busy_timeout = 5000L) {
  curation_sqlite_require_packages()
  path <- curation_scalar_character(
    path,
    "path",
    class = "planscanR_error_curation_repository"
  )
  if (identical(path, ":memory:") || identical(path, "file::memory:")) {
    curation_sqlite_abort("The SQLite curation repository requires an explicit local file path.")
  }
  busy_timeout <- curation_sqlite_scalar_integer(busy_timeout, "busy_timeout")
  parent <- dirname(path)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    curation_sqlite_abort("The SQLite database parent directory could not be created.")
  }
  if (dir.exists(path)) {
    curation_sqlite_abort("The SQLite database path must be a file, not a directory.")
  }
  path <- normalizePath(path, mustWork = FALSE)
  conn <- tryCatch(
    DBI::dbConnect(RSQLite::SQLite(), dbname = path),
    error = function(error) {
      curation_sqlite_abort(
        paste0("Could not open the SQLite curation database: ", conditionMessage(error)),
        parent = error
      )
    }
  )
  path <- normalizePath(path, mustWork = TRUE)
  keep_open <- FALSE
  on.exit(
    if (!isTRUE(keep_open) && DBI::dbIsValid(conn)) {
      try(DBI::dbDisconnect(conn), silent = TRUE)
    },
    add = TRUE
  )
  pragma <- curation_sqlite_set_pragmas(conn, busy_timeout)
  curation_sqlite_migrate(conn, busy_timeout = busy_timeout)
  state <- new.env(parent = emptyenv())
  state$conn <- conn
  state$path <- path
  state$closed <- FALSE
  state$busy_timeout <- busy_timeout
  callbacks <- curation_review_repository(
    read_fn = function(scope, key, actor) {
      curation_check_scope(scope)
      curation_check_actor(actor)
      curation_sqlite_review_select(curation_sqlite_connection(state), scope, key)
    },
    list_fn = function(scope, filter, actor, limit, offset) {
      curation_check_scope(scope)
      curation_check_actor(actor)
      curation_sqlite_select_rows(
        curation_sqlite_connection(state), scope, filter, limit, offset
      )
    },
    upsert_fn = function(scope, decision, actor, expected_revision) {
      curation_sqlite_upsert(state, scope, decision, actor, expected_revision)
    },
    clear_fn = function(scope, key, actor, expected_revision) {
      curation_sqlite_clear(state, scope, key, actor, expected_revision)
    },
    bulk_upsert_fn = function(scope, decisions, actor, expected_revision) {
      curation_sqlite_bulk_upsert(state, scope, decisions, actor, expected_revision)
    },
    import_fn = function(scope, rows, actor, expected_revision) {
      curation_sqlite_import(state, scope, rows, actor, expected_revision)
    },
    export_fn = function(scope, actor, format) {
      curation_check_scope(scope)
      curation_check_actor(actor)
      curation_sqlite_export(state, scope, actor, format)
    }
  )
  class(callbacks) <- c(
    "planscanR_curation_review_repository_sqlite",
    "planscanR_curation_review_repository"
  )
  callbacks$path <- path
  callbacks$connection <- conn
  callbacks$journal_mode <- pragma$journal_mode
  callbacks$foreign_keys <- pragma$foreign_keys
  callbacks$busy_timeout <- busy_timeout
  callbacks$schema_version <- curation_sqlite_schema_version
  callbacks$close <- function() {
    if (!isTRUE(state$closed) && DBI::dbIsValid(state$conn)) {
      DBI::dbDisconnect(state$conn)
    }
    state$closed <- TRUE
    invisible(TRUE)
  }
  callbacks$export_csv <- function(scope, actor, path = NULL) {
    if (is.null(path)) {
      return(curation_sqlite_export(state, scope, actor, "csv"))
    }
    curation_sqlite_write_csv(state, scope, actor, path)
  }
  keep_open <- TRUE
  callbacks
}

#' @rdname curation_review_repository_sqlite
#' @export
curation_sqlite_review_repository <- function(path, busy_timeout = 5000L) {
  curation_review_repository_sqlite(path = path, busy_timeout = busy_timeout)
}
