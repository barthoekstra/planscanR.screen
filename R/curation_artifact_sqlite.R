# Concrete C3 artifact persistence. Metadata, revisions, and active model
# references live in SQLite; payloads are immutable RDS files below the
# caller's explicit artifact root.

curation_artifact_sqlite_schema_version <- 1L
curation_artifact_enrichment_schema <- "curation-enrichment.v1"

curation_artifact_sqlite_abort <- function(
  message,
  class = "planscanR_error_curation_artifact",
  parent = NULL
) {
  classes <- unique(c(class, "planscanR_error_curation_artifact", "planscanR_error_curation_repository"))
  rlang::abort(message, class = classes, parent = parent)
}

curation_artifact_sqlite_conflict <- function(message) {
  curation_artifact_sqlite_abort(message, class = "planscanR_error_curation_conflict")
}

curation_artifact_sqlite_authorization <- function(message) {
  curation_artifact_sqlite_abort(message, class = "planscanR_error_curation_authorization")
}

curation_artifact_sqlite_require_packages <- function() {
  needed <- c("DBI", "RSQLite")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "The SQLite curation artifact store needs {cli::qty(missing)} package{?s}: {.pkg {missing}}.",
        i = "Install the optional DBI and RSQLite dependencies and retry."
      ),
      class = "planscanR_error_missing_dep"
    )
  }
  invisible(TRUE)
}

curation_artifact_hash <- function(value) {
  bytes <- serialize(value, NULL, version = 3)
  path <- tempfile("planscanR-artifact-hash-")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  connection <- file(path, open = "wb")
  writeBin(bytes, connection)
  close(connection)
  unname(as.character(tools::md5sum(path)))
}

curation_artifact_content_fingerprint <- function(value) {
  if (is.data.frame(value)) {
    value <- as.data.frame(value, stringsAsFactors = FALSE)
    rownames(value) <- NULL
    if (all(c("country", "document_id") %in% names(value))) {
      order_key <- paste(value$country, value$document_id, sep = "\r")
      value <- value[order(order_key, method = "radix"), , drop = FALSE]
      rownames(value) <- NULL
    }
  }
  curation_artifact_hash(value)
}

curation_artifact_fingerprint_value <- function(value, depth = 0L) {
  if (depth > 8L) {
    return(list(type = typeof(value), class = class(value), truncated = TRUE))
  }
  if (is.function(value)) {
    return(list(function_signature = curation_artifact_function_signature(value)))
  }
  if (is.environment(value) || identical(typeof(value), "externalptr")) {
    return(list(type = typeof(value), class = class(value)))
  }
  if (is.list(value)) {
    out <- lapply(value, curation_artifact_fingerprint_value, depth = depth + 1L)
    names(out) <- names(value)
    return(out)
  }
  value
}

curation_artifact_closure_fingerprint <- function(fn) {
  env <- environment(fn)
  if (is.null(env)) {
    return(NULL)
  }
  variables <- tryCatch(
    codetools::findGlobals(fn, merge = FALSE)$variables,
    error = function(error) character()
  )
  variables <- sort(unique(variables))
  if (length(variables) == 0L) {
    return(list())
  }
  values <- lapply(variables, function(name) {
    if (!exists(name, envir = env, inherits = TRUE)) {
      return(list(unbound = TRUE))
    }
    curation_artifact_fingerprint_value(get(name, envir = env, inherits = TRUE))
  })
  names(values) <- variables
  values
}

curation_artifact_function_signature <- function(fn) {
  if (!is.function(fn)) {
    return(NULL)
  }
  list(
    formals = paste(suppressWarnings(utils::capture.output(dput(formals(fn), control = "all"))), collapse = "\n"),
    body = paste(suppressWarnings(utils::capture.output(dput(body(fn), control = "all"))), collapse = "\n")
  )
}

curation_artifact_function_fingerprint <- function(fn) {
  signature <- curation_artifact_function_signature(fn)
  if (is.null(signature)) {
    return(NULL)
  }
  c(signature, list(closure = curation_artifact_closure_fingerprint(fn)))
}

#' Fingerprint a curation configuration's policy content.
#'
#' The stable configuration identifier is retained as provenance, but it is
#' not sufficient for invalidation when a caller changes policy content in
#' place. This helper hashes the configuration values that affect enrichment,
#' selection, funneling, branding, and translation. Policy callbacks include
#' their formals, body, and statically referenced captured bindings; callers
#' should keep those bindings serializable policy values rather than opaque
#' external state.
#'
#' @param config A `planscanR_curation_config`.
#' @return One character configuration fingerprint.
#' @export
curation_config_fingerprint <- function(config) {
  curation_check_config(config)
  stages <- lapply(config$funnel_stages, function(stage) {
    list(
      key = stage$key,
      label = stage$label,
      fn = curation_artifact_function_fingerprint(stage$fn)
    )
  })
  translator <- if (is.null(config$translator)) {
    NULL
  } else {
    list(
      service_id = config$translator$service_id,
      write_policy = config$translator$write_policy
    )
  }
  curation_artifact_hash(list(
    project_id = config$project_id,
    corpus_id = config$corpus_id,
    config_id = config$config_id,
    topics = config$topics,
    labels = config$labels,
    lexicon = config$lexicon,
    selection_controls = config$selection_controls,
    select_fn = curation_artifact_function_fingerprint(config$select_fn),
    funnel_stages = stages,
    branding = config$branding,
    translator = translator
  ))
}

curation_artifact_filter_coverage <- function(snapshot, countries = NULL) {
  if (!is.data.frame(snapshot) || !"country" %in% names(snapshot)) {
    return(list(countries = countries, n_by_country = integer(0), n = NA_integer_))
  }
  observed <- as.character(snapshot$country)
  requested <- if (is.null(countries)) {
    sort(unique(observed))
  } else {
    as.character(countries)
  }
  counts <- table(factor(observed, levels = requested))
  list(
    countries = requested,
    n_by_country = as.integer(counts),
    n = as.integer(nrow(snapshot))
  )
}

curation_artifact_snapshot_metadata <- function(snapshot) {
  metadata <- attr(snapshot, "curation_snapshot_metadata", exact = TRUE)
  if (is.null(metadata)) {
    metadata <- list()
  }
  if (!is.list(metadata)) {
    curation_artifact_sqlite_abort(
      "The snapshot metadata attribute must be a list."
    )
  }
  metadata
}

#' Compute an invalidation-aware snapshot version.
#'
#' The version combines payload content, configuration policy content,
#' enrichment schema, and filter coverage. Native document IDs alone never
#' identify a reusable snapshot.
#'
#' @param snapshot A snapshot data frame or serializable snapshot payload.
#' @param scope A `planscanR_curation_scope`.
#' @param config Optional `planscanR_curation_config`; metadata attached by
#'   [curation_records_snapshot()] is used when omitted.
#' @param countries Optional requested country filter.
#' @return One character immutable snapshot version.
#' @export
curation_snapshot_version <- function(snapshot, scope, config = NULL, countries = NULL) {
  curation_check_scope(scope, class = "planscanR_error_curation_artifact")
  if (!is.null(config)) {
    curation_check_config(config)
  }
  metadata <- curation_artifact_snapshot_metadata(snapshot)
  config_fingerprint <- if (!is.null(config)) {
    curation_config_fingerprint(config)
  } else {
    metadata$config_fingerprint %||% curation_artifact_hash(scope$config_id)
  }
  coverage <- metadata$filter_coverage %||% curation_artifact_filter_coverage(snapshot, countries)
  if (!is.null(countries)) {
    coverage <- curation_artifact_filter_coverage(snapshot, countries)
  }
  payload_fingerprint <- curation_artifact_content_fingerprint(snapshot)
  corpus_fingerprint <- metadata$corpus_fingerprint %||% payload_fingerprint
  enrichment_schema <- metadata$enrichment_schema %||% curation_artifact_enrichment_schema
  fingerprint <- curation_artifact_hash(list(
    schema = "curation-snapshot.v1",
    project_id = scope$project_id,
    corpus_id = scope$corpus_id,
    config_id = scope$config_id,
    corpus_fingerprint = corpus_fingerprint,
    config_fingerprint = config_fingerprint,
    enrichment_schema = enrichment_schema,
    payload_fingerprint = payload_fingerprint,
    filter_coverage = coverage
  ))
  paste0("snapshot-v1-", fingerprint)
}

curation_artifact_sqlite_paths <- function(paths) {
  paths <- curation_validate_artifact_paths(paths)
  root <- paths$root
  if (!grepl("^(/|[A-Za-z]:[/\\\\])", root)) {
    root <- file.path(getwd(), root)
  }
  root <- normalizePath(root, mustWork = FALSE)
  within_root <- function(path) {
    identical(path, root) || startsWith(path, paste0(root, .Platform$file.sep))
  }
  resolve <- function(path, default) {
    path <- path %||% default
    if (!grepl("^(/|[A-Za-z]:[/\\\\])", path)) {
      path <- file.path(root, path)
    }
    path <- normalizePath(path, mustWork = FALSE)
    if (!within_root(path)) {
      curation_artifact_sqlite_abort(
        "Every concrete artifact path must be inside the explicit artifact root."
      )
    }
    path
  }
  database <- resolve(paths$database, file.path(root, "curation-artifacts.sqlite"))
  snapshots <- resolve(paths$snapshots, file.path(root, "snapshots"))
  samples <- resolve(paths$samples, file.path(root, "samples"))
  models <- resolve(paths$models, file.path(root, "models"))
  list(
    root = root,
    database = database,
    snapshots = snapshots,
    samples = samples,
    models = models
  )
}

curation_artifact_sqlite_state_connection <- function(state) {
  if (isTRUE(state$closed) || !DBI::dbIsValid(state$conn)) {
    curation_artifact_sqlite_abort("The SQLite curation artifact store is closed.")
  }
  state$conn
}

curation_artifact_sqlite_serialize <- function(value) {
  raw <- serialize(value, NULL, version = 3)
  paste(sprintf("%02x", as.integer(raw)), collapse = "")
}

curation_artifact_sqlite_unserialize <- function(value, default = NULL) {
  if (is.null(value) || (length(value) == 1L && is.na(value))) {
    return(default)
  }
  raw <- if (is.raw(value)) {
    value
  } else if (is.character(value) && length(value) == 1L && nzchar(value)) {
    pieces <- substring(value, seq.int(1L, nchar(value), by = 2L), seq.int(2L, nchar(value), by = 2L))
    if (any(nchar(pieces) != 2L)) {
      return(default)
    }
    as.raw(strtoi(pieces, base = 16L))
  } else if (is.list(value) && length(value) == 1L && is.raw(value[[1L]])) {
    value[[1L]]
  } else {
    return(default)
  }
  tryCatch(unserialize(raw), error = function(error) default)
}

curation_artifact_sqlite_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
}

curation_artifact_sqlite_authorized <- function(actor, owner_id) {
  curation_check_actor(actor)
  elevated <- intersect(actor$roles, "admin")
  if (!identical(actor$id, owner_id) && length(elevated) == 0L) {
    curation_artifact_sqlite_authorization(
      "The authenticated actor is not authorized for this reviewer-scoped artifact."
    )
  }
  invisible(TRUE)
}

curation_artifact_sqlite_atomic_save <- function(value, path) {
  directory <- dirname(path)
  if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE)) {
    curation_artifact_sqlite_abort(
      paste0("Could not create artifact directory: ", directory)
    )
  }
  temporary <- tempfile(
    pattern = paste0(".", basename(path), "-"),
    tmpdir = directory
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(value, temporary)
  # A hard link fails when the destination appears between the existence check
  # and publication, so a concurrent writer can never replace the winner's
  # immutable bytes.
  linked <- if (file.exists(path)) {
    FALSE
  } else {
    isTRUE(file.link(temporary, path))
  }
  if (!linked) {
    curation_artifact_sqlite_conflict(
      paste0("The immutable artifact path already exists or could not be published: ", path)
    )
  }
  invisible(path)
}

curation_artifact_sqlite_model_file_fingerprint_prefix <- "rds-file-sha256-v1:"

curation_artifact_sqlite_require_file_digest <- function() {
  if (!requireNamespace("openssl", quietly = TRUE)) {
    curation_artifact_sqlite_abort(
      "The SQLite curation model store needs the openssl package to verify model files.",
      class = "planscanR_error_missing_dep"
    )
  }
  invisible(TRUE)
}

curation_artifact_sqlite_read_file_bytes <- function(path) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  chunks <- list()
  repeat {
    chunk <- readBin(connection, what = "raw", n = 1024L * 1024L)
    if (length(chunk) == 0L) {
      break
    }
    chunks[[length(chunks) + 1L]] <- chunk
  }
  if (length(chunks) == 0L) {
    return(raw(0))
  }
  do.call(c, chunks)
}

curation_artifact_sqlite_sha256_hex <- function(bytes) {
  curation_artifact_sqlite_require_file_digest()
  as.character(openssl::sha256(bytes))
}

curation_artifact_sqlite_model_file_fingerprint <- function(path) {
  bytes <- curation_artifact_sqlite_read_file_bytes(path)
  paste0(
    curation_artifact_sqlite_model_file_fingerprint_prefix,
    curation_artifact_sqlite_sha256_hex(bytes)
  )
}

curation_artifact_sqlite_parse_model_fingerprint <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    curation_artifact_sqlite_abort(
      "The model artifact fingerprint metadata is malformed."
    )
  }
  prefix <- curation_artifact_sqlite_model_file_fingerprint_prefix
  if (startsWith(value, prefix)) {
    digest <- substring(value, nchar(prefix) + 1L)
    if (!grepl("^[0-9a-f]{64}$", digest)) {
      curation_artifact_sqlite_abort(
        "The model artifact file fingerprint metadata is malformed."
      )
    }
    return(list(kind = "file", digest = digest))
  }
  if (grepl(":", value, fixed = TRUE)) {
    curation_artifact_sqlite_abort(
      "The model artifact fingerprint metadata uses an unknown tag."
    )
  }
  list(kind = "legacy", digest = value)
}

curation_artifact_sqlite_read_model_bytes <- function(bytes, path) {
  connection <- rawConnection(bytes, open = "rb")
  reader <- if (length(bytes) >= 2L && identical(as.integer(bytes[1:2]), c(31L, 139L))) {
    gzcon(connection)
  } else {
    connection
  }
  on.exit(try(close(reader), silent = TRUE), add = TRUE)
  read_result <- tryCatch(
    {
      # curation_artifact_sqlite_atomic_save() uses the default gzip RDS
      # compression. Detect it so the digest and deserialization consume the
      # same captured byte stream, avoiding a path replacement between reads.
      readRDS(reader)
    },
    error = function(error) {
      curation_artifact_sqlite_abort(
        paste0("The model artifact could not be read: ", path),
        parent = error
      )
    }
  )
  read_result
}

curation_artifact_sqlite_migrate <- function(conn, busy_timeout) {
  curation_sqlite_with_transaction(conn, function() {
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS curation_artifact_migrations (",
        "version INTEGER PRIMARY KEY, applied_at TEXT NOT NULL)"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS curation_artifact_snapshots (",
        "project_id TEXT NOT NULL, corpus_id TEXT NOT NULL, config_id TEXT NOT NULL,",
        "version TEXT NOT NULL, snapshot_id TEXT NOT NULL, revision INTEGER NOT NULL,",
        "path TEXT NOT NULL UNIQUE, content_fingerprint TEXT NOT NULL,",
        "config_fingerprint TEXT NOT NULL, enrichment_schema TEXT NOT NULL,",
        "metadata_blob BLOB NOT NULL, created_by TEXT NOT NULL, created_at TEXT NOT NULL,",
        "PRIMARY KEY (project_id, corpus_id, config_id, version))"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS curation_artifact_samples (",
        "project_id TEXT NOT NULL, corpus_id TEXT NOT NULL, config_id TEXT NOT NULL,",
        "reviewer_id TEXT NOT NULL, sample_id TEXT NOT NULL, revision INTEGER NOT NULL,",
        "path TEXT NOT NULL UNIQUE, metadata_blob BLOB NOT NULL, created_by TEXT NOT NULL,",
        "created_at TEXT NOT NULL,",
        "PRIMARY KEY (project_id, corpus_id, config_id, reviewer_id, sample_id))"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS curation_artifact_models (",
        "project_id TEXT NOT NULL, corpus_id TEXT NOT NULL, config_id TEXT NOT NULL,",
        "model_id TEXT NOT NULL, revision INTEGER NOT NULL, path TEXT NOT NULL UNIQUE,",
        "model_fingerprint TEXT NOT NULL, provenance_blob BLOB NOT NULL,",
        "created_by TEXT NOT NULL, created_at TEXT NOT NULL,",
        "PRIMARY KEY (project_id, corpus_id, config_id, model_id))"
      )
    )
    DBI::dbExecute(
      conn,
      paste(
        "CREATE TABLE IF NOT EXISTS curation_artifact_active_models (",
        "project_id TEXT NOT NULL, corpus_id TEXT NOT NULL, config_id TEXT NOT NULL,",
        "active_model_id TEXT NOT NULL, active_revision INTEGER NOT NULL,",
        "provenance_blob BLOB NOT NULL, published_by TEXT NOT NULL, published_at TEXT NOT NULL,",
        "PRIMARY KEY (project_id, corpus_id, config_id))"
      )
    )
    DBI::dbExecute(
      conn,
      "INSERT OR IGNORE INTO curation_artifact_migrations (version, applied_at) VALUES (?, ?)",
      params = list(curation_artifact_sqlite_schema_version, curation_artifact_sqlite_now())
    )
  }, busy_timeout = busy_timeout)
  invisible(curation_artifact_sqlite_schema_version)
}

curation_artifact_sqlite_scope_params <- function(scope) {
  list(scope$project_id, scope$corpus_id, scope$config_id)
}

curation_artifact_sqlite_revision_conflict <- function(expected, current, what) {
  curation_artifact_sqlite_conflict(
    paste0(
      what, " revision conflict: expected ", expected,
      ", current ", current, "."
    )
  )
}

curation_artifact_sqlite_check_creation_revision <- function(expected_revision, what) {
  if (!identical(as.integer(expected_revision), 0L)) {
    curation_artifact_sqlite_revision_conflict(expected_revision, 1L, what)
  }
  invisible(TRUE)
}

curation_artifact_sqlite_snapshot_parts <- function(scope, snapshot) {
  metadata <- curation_artifact_snapshot_metadata(snapshot)
  version <- curation_snapshot_version(snapshot, scope)
  version <- curation_scalar_character(
    version,
    "snapshot version",
    class = "planscanR_error_curation_artifact"
  )
  metadata$version <- version
  metadata$schema_version <- metadata$schema_version %||% "curation-snapshot.v1"
  metadata$enrichment_schema <- metadata$enrichment_schema %||% curation_artifact_enrichment_schema
  metadata$corpus_fingerprint <- metadata$corpus_fingerprint %||% curation_artifact_content_fingerprint(snapshot)
  metadata$config_fingerprint <- metadata$config_fingerprint %||% curation_artifact_hash(scope$config_id)
  metadata$filter_coverage <- metadata$filter_coverage %||% curation_artifact_filter_coverage(snapshot)
  list(
    metadata = metadata,
    version = version,
    snapshot_id = paste0(
      "snapshot-",
      curation_artifact_hash(list(
        project_id = scope$project_id,
        corpus_id = scope$corpus_id,
        config_id = scope$config_id,
        version = version
      ))
    )
  )
}

curation_artifact_sqlite_snapshot_read <- function(state, scope, version, actor) {
  curation_check_scope(scope, class = "planscanR_error_curation_artifact")
  curation_check_actor(actor)
  conn <- curation_artifact_sqlite_state_connection(state)
  params <- curation_artifact_sqlite_scope_params(scope)
  if (is.null(version)) {
    query <- paste(
      "SELECT * FROM curation_artifact_snapshots",
      "WHERE project_id = ? AND corpus_id = ? AND config_id = ?",
      "ORDER BY rowid DESC LIMIT 1"
    )
  } else {
    version <- curation_scalar_character(
      version,
      "version",
      class = "planscanR_error_curation_artifact"
    )
    query <- paste(
      "SELECT * FROM curation_artifact_snapshots",
      "WHERE project_id = ? AND corpus_id = ? AND config_id = ? AND version = ?"
    )
    params <- c(params, version)
  }
  row <- DBI::dbGetQuery(conn, query, params = params)
  if (nrow(row) == 0L) {
    return(NULL)
  }
  payload <- tryCatch(
    readRDS(row$path[[1L]]),
    error = function(error) {
      curation_artifact_sqlite_abort(
        paste0("The snapshot artifact could not be read: ", row$path[[1L]]),
        parent = error
      )
    }
  )
  list(
    ok = TRUE,
    snapshot_id = row$snapshot_id[[1L]],
    project_id = row$project_id[[1L]],
    corpus_id = row$corpus_id[[1L]],
    config_id = row$config_id[[1L]],
    version = row$version[[1L]],
    revision = as.integer(row$revision[[1L]]),
    path = row$path[[1L]],
    payload = payload,
    metadata = curation_artifact_sqlite_unserialize(row$metadata_blob[[1L]], list())
  )
}

curation_artifact_sqlite_snapshot_write <- function(
  state,
  scope,
  snapshot,
  actor,
  expected_revision
) {
  curation_check_scope(scope, class = "planscanR_error_curation_artifact")
  curation_check_actor(actor)
  curation_artifact_sqlite_check_creation_revision(expected_revision, "snapshot")
  parts <- curation_artifact_sqlite_snapshot_parts(scope, snapshot)
  paths <- state$paths
  path <- file.path(paths$snapshots, paste0(parts$snapshot_id, ".rds"))
  curation_artifact_sqlite_atomic_save(snapshot, path)
  metadata <- parts$metadata
  # A failed metadata registration leaves only an invisible immutable orphan.
  # It must never remove or replace a visible prior artifact.
  curation_sqlite_with_transaction(
    curation_artifact_sqlite_state_connection(state),
    function() {
      DBI::dbExecute(
        curation_artifact_sqlite_state_connection(state),
        paste(
          "INSERT INTO curation_artifact_snapshots (",
          "project_id, corpus_id, config_id, version, snapshot_id, revision,",
          "path, content_fingerprint, config_fingerprint, enrichment_schema,",
          "metadata_blob, created_by, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        ),
        params = list(
          scope$project_id,
          scope$corpus_id,
          scope$config_id,
          parts$version,
          parts$snapshot_id,
          1L,
          path,
          metadata$corpus_fingerprint,
          metadata$config_fingerprint,
          metadata$enrichment_schema,
          curation_artifact_sqlite_serialize(metadata),
          actor$id,
          curation_artifact_sqlite_now()
        )
      )
      list(
        ok = TRUE,
        snapshot_id = parts$snapshot_id,
        version = parts$version,
        revision = 1L,
        path = path
      )
    },
    busy_timeout = state$busy_timeout
  )
}

curation_artifact_sqlite_sample_id <- function(scope, reviewer_id, sample) {
  paste0(
    "sample-",
    curation_artifact_hash(list(
      project_id = scope$project_id,
      corpus_id = scope$corpus_id,
      config_id = scope$config_id,
      reviewer_id = reviewer_id,
      payload = sample
    ))
  )
}

curation_artifact_sqlite_sample_metadata <- function(sample) {
  metadata <- attr(sample, "curation_sample_metadata", exact = TRUE)
  if (is.null(metadata)) {
    metadata <- list()
  }
  if (!is.list(metadata)) {
    curation_artifact_sqlite_abort("Sample metadata must be a list.")
  }
  metadata
}

curation_artifact_sqlite_sample_list <- function(state, scope, reviewer_id, actor) {
  curation_check_scope(scope, class = "planscanR_error_curation_artifact")
  curation_check_actor(actor)
  if (is.null(reviewer_id)) {
    reviewer_id <- if (length(intersect(actor$roles, "admin")) > 0L) {
      NULL
    } else {
      actor$id
    }
  } else {
    reviewer_id <- curation_scalar_character(
      reviewer_id,
      "reviewer_id",
      class = "planscanR_error_curation_artifact"
    )
    curation_artifact_sqlite_authorized(actor, reviewer_id)
  }
  conn <- curation_artifact_sqlite_state_connection(state)
  where <- paste(
    "project_id = ? AND corpus_id = ? AND config_id = ?",
    if (is.null(reviewer_id)) "" else "AND reviewer_id = ?"
  )
  params <- curation_artifact_sqlite_scope_params(scope)
  if (!is.null(reviewer_id)) {
    params <- c(params, reviewer_id)
  }
  rows <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT project_id, corpus_id, config_id, reviewer_id, sample_id,",
      "revision, path, created_by, created_at FROM curation_artifact_samples",
      "WHERE", where, "ORDER BY rowid"
    ),
    params = params
  )
  rows$revision <- as.integer(rows$revision)
  rows
}

curation_artifact_sqlite_sample_read <- function(state, scope, sample_id, actor) {
  curation_check_scope(scope, class = "planscanR_error_curation_artifact")
  curation_check_actor(actor)
  sample_id <- curation_scalar_character(
    sample_id,
    "sample_id",
    class = "planscanR_error_curation_artifact"
  )
  conn <- curation_artifact_sqlite_state_connection(state)
  rows <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT * FROM curation_artifact_samples",
      "WHERE project_id = ? AND corpus_id = ? AND config_id = ? AND sample_id = ?",
      "ORDER BY reviewer_id"
    ),
    params = c(curation_artifact_sqlite_scope_params(scope), sample_id)
  )
  if (nrow(rows) == 0L) {
    return(NULL)
  }
  elevated <- length(intersect(actor$roles, "admin")) > 0L
  if (elevated) {
    if (nrow(rows) != 1L) {
      curation_artifact_sqlite_abort(
        "The sample ID is ambiguous across reviewers; use a reviewer-scoped list."
      )
    }
    row <- rows[1L, , drop = FALSE]
  } else {
    own <- rows[rows$reviewer_id == actor$id, , drop = FALSE]
    if (nrow(own) == 0L) {
      curation_artifact_sqlite_authorized(actor, rows$reviewer_id[[1L]])
    }
    row <- own[1L, , drop = FALSE]
  }
  curation_artifact_sqlite_authorized(actor, row$reviewer_id[[1L]])
  payload <- tryCatch(
    readRDS(row$path[[1L]]),
    error = function(error) {
      curation_artifact_sqlite_abort(
        paste0("The sample artifact could not be read: ", row$path[[1L]]),
        parent = error
      )
    }
  )
  list(
    ok = TRUE,
    sample_id = row$sample_id[[1L]],
    project_id = row$project_id[[1L]],
    corpus_id = row$corpus_id[[1L]],
    config_id = row$config_id[[1L]],
    reviewer_id = row$reviewer_id[[1L]],
    revision = as.integer(row$revision[[1L]]),
    path = row$path[[1L]],
    payload = payload,
    metadata = curation_artifact_sqlite_unserialize(row$metadata_blob[[1L]], list())
  )
}

curation_artifact_sqlite_sample_write <- function(
  state,
  scope,
  sample,
  reviewer_id,
  actor,
  expected_revision
) {
  curation_check_scope(scope, class = "planscanR_error_curation_artifact")
  curation_artifact_sqlite_authorized(actor, reviewer_id)
  curation_artifact_sqlite_check_creation_revision(expected_revision, "sample")
  reviewer_id <- curation_scalar_character(
    reviewer_id,
    "reviewer_id",
    class = "planscanR_error_curation_artifact"
  )
  metadata <- curation_artifact_sqlite_sample_metadata(sample)
  sample_id <- metadata$sample_id %||% curation_artifact_sqlite_sample_id(scope, reviewer_id, sample)
  sample_id <- curation_scalar_character(
    sample_id,
    "sample_id",
    class = "planscanR_error_curation_artifact"
  )
  path <- file.path(
    state$paths$samples,
    paste0(
      "sample-",
      curation_artifact_hash(list(
        project_id = scope$project_id,
        corpus_id = scope$corpus_id,
        config_id = scope$config_id,
        reviewer_id = reviewer_id,
        sample_id = sample_id
      )),
      ".rds"
    )
  )
  curation_artifact_sqlite_atomic_save(sample, path)
  curation_sqlite_with_transaction(
    curation_artifact_sqlite_state_connection(state),
    function() {
      DBI::dbExecute(
        curation_artifact_sqlite_state_connection(state),
        paste(
          "INSERT INTO curation_artifact_samples (",
          "project_id, corpus_id, config_id, reviewer_id, sample_id, revision,",
          "path, metadata_blob, created_by, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        ),
        params = list(
          scope$project_id,
          scope$corpus_id,
          scope$config_id,
          reviewer_id,
          sample_id,
          1L,
          path,
          curation_artifact_sqlite_serialize(metadata),
          actor$id,
          curation_artifact_sqlite_now()
        )
      )
      list(
        ok = TRUE,
        sample_id = sample_id,
        reviewer_id = reviewer_id,
        revision = 1L,
        path = path
      )
    },
    busy_timeout = state$busy_timeout
  )
}

curation_artifact_sqlite_model_provenance <- function(provenance, scope, actor) {
  if (!is.list(provenance)) {
    curation_artifact_sqlite_abort("Model provenance must be a list.")
  }
  if (!is.null(provenance$creator) && !identical(as.character(provenance$creator), actor$id)) {
    curation_artifact_sqlite_authorization(
      "Model provenance creator must match the authenticated actor."
    )
  }
  required <- c(
    "review_fingerprint",
    "snapshot_version",
    "config_fingerprint",
    "corpus_snapshot",
    "training_parameters"
  )
  missing <- required[!required %in% names(provenance)]
  if (length(missing) > 0L) {
    curation_artifact_sqlite_abort(
      paste0(
        "Model provenance is missing required field",
        if (length(missing) > 1L) "s" else "",
        ": ", paste(missing, collapse = ", "), "."
      )
    )
  }
  for (field in c("review_fingerprint", "snapshot_version", "config_fingerprint")) {
    value <- provenance[[field]]
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
      curation_artifact_sqlite_abort(
        paste0("Model provenance field ", field, " must be one non-empty string.")
      )
    }
  }
  corpus_version <- if (is.list(provenance$corpus_snapshot)) {
    provenance$corpus_snapshot$version
  } else {
    NULL
  }
  if (!is.character(corpus_version) || length(corpus_version) != 1L || is.na(corpus_version) || !nzchar(corpus_version)) {
    curation_artifact_sqlite_abort(
      "Model provenance corpus_snapshot must include an immutable version."
    )
  }
  if (!is.list(provenance$training_parameters)) {
    curation_artifact_sqlite_abort(
      "Model provenance training_parameters must be a list."
    )
  }
  out <- provenance
  out$project_id <- scope$project_id
  out$corpus_id <- scope$corpus_id
  out$config_id <- scope$config_id
  out$creator <- actor$id
  out$created_at <- curation_artifact_sqlite_now()
  out$provenance_schema <- "curation-model-provenance.v1"
  out
}

curation_artifact_sqlite_model_read_row <- function(state, scope, model_id) {
  conn <- curation_artifact_sqlite_state_connection(state)
  DBI::dbGetQuery(
    conn,
    paste(
      "SELECT * FROM curation_artifact_models",
      "WHERE project_id = ? AND corpus_id = ? AND config_id = ? AND model_id = ?",
      "LIMIT 1"
    ),
    params = c(curation_artifact_sqlite_scope_params(scope), model_id)
  )
}

curation_artifact_sqlite_validate_model_file <- function(row) {
  if (nrow(row) == 0L || !file.exists(row$path[[1L]])) {
    curation_artifact_sqlite_abort("The requested model artifact is missing.")
  }
  fingerprint <- curation_artifact_sqlite_parse_model_fingerprint(row$model_fingerprint[[1L]])
  if (identical(fingerprint$kind, "file")) {
    bytes <- curation_artifact_sqlite_read_file_bytes(row$path[[1L]])
    actual <- paste0(
      curation_artifact_sqlite_model_file_fingerprint_prefix,
      curation_artifact_sqlite_sha256_hex(bytes)
    )
    if (!identical(actual, row$model_fingerprint[[1L]])) {
      curation_artifact_sqlite_abort(
        "The model artifact file fingerprint does not match its immutable metadata."
      )
    }
    model <- curation_artifact_sqlite_read_model_bytes(bytes, row$path[[1L]])
  } else {
    model <- tryCatch(
      readRDS(row$path[[1L]]),
      error = function(error) {
        curation_artifact_sqlite_abort(
          paste0("The model artifact could not be read: ", row$path[[1L]]),
          parent = error
        )
      }
    )
  }
  if (!inherits(model, "planscanR_selection_model")) {
    curation_artifact_sqlite_abort(
      paste0("The model artifact is not a planscanR_selection_model: ", row$path[[1L]])
    )
  }
  if (identical(fingerprint$kind, "legacy")) {
    actual_fingerprint <- curation_artifact_content_fingerprint(model)
    if (!identical(as.character(actual_fingerprint), as.character(row$model_fingerprint[[1L]]))) {
      curation_artifact_sqlite_abort(
        "The model artifact fingerprint does not match its immutable metadata."
      )
    }
  }
  model
}

curation_artifact_sqlite_model_read <- function(state, scope, model_id, actor) {
  curation_check_scope(scope, class = "planscanR_error_curation_artifact")
  curation_check_actor(actor)
  model_id <- curation_scalar_character(
    model_id,
    "model_id",
    class = "planscanR_error_curation_artifact"
  )
  row <- curation_artifact_sqlite_model_read_row(state, scope, model_id)
  if (nrow(row) == 0L) {
    return(NULL)
  }
  model <- curation_artifact_sqlite_validate_model_file(row)
  list(
    ok = TRUE,
    model_id = row$model_id[[1L]],
    project_id = row$project_id[[1L]],
    corpus_id = row$corpus_id[[1L]],
    config_id = row$config_id[[1L]],
    revision = as.integer(row$revision[[1L]]),
    path = row$path[[1L]],
    payload = model,
    provenance = curation_artifact_sqlite_unserialize(row$provenance_blob[[1L]], list()),
    created_by = row$created_by[[1L]],
    created_at = row$created_at[[1L]]
  )
}

curation_artifact_sqlite_model_list <- function(state, scope, actor) {
  curation_check_scope(scope, class = "planscanR_error_curation_artifact")
  curation_check_actor(actor)
  rows <- DBI::dbGetQuery(
    curation_artifact_sqlite_state_connection(state),
    paste(
      "SELECT project_id, corpus_id, config_id, model_id, revision, path,",
      "provenance_blob, created_by, created_at FROM curation_artifact_models",
      "WHERE project_id = ? AND corpus_id = ? AND config_id = ? ORDER BY rowid"
    ),
    params = curation_artifact_sqlite_scope_params(scope)
  )
  if (nrow(rows) == 0L) {
    rows$revision <- integer(0)
    rows$provenance <- list()
    rows$provenance_blob <- NULL
    return(rows[, setdiff(names(rows), "provenance_blob"), drop = FALSE])
  }
  rows$revision <- as.integer(rows$revision)
  rows$provenance <- lapply(rows$provenance_blob, curation_artifact_sqlite_unserialize)
  rows$provenance_blob <- NULL
  rows
}

curation_artifact_sqlite_model_write <- function(
  state,
  scope,
  model,
  provenance,
  actor,
  expected_revision
) {
  curation_check_scope(scope, class = "planscanR_error_curation_artifact")
  curation_check_actor(actor)
  curation_artifact_sqlite_check_creation_revision(expected_revision, "model")
  if (!inherits(model, "planscanR_selection_model")) {
    curation_artifact_sqlite_abort(
      "The model payload must be a planscanR_selection_model."
    )
  }
  provenance <- curation_artifact_sqlite_model_provenance(provenance, scope, actor)
  model_id <- paste0(
    "model-",
    curation_artifact_hash(list(
      scope = scope,
      creator = actor$id,
      timestamp = format(Sys.time(), "%s.%OS6"),
      process = Sys.getpid(),
      random = stats::runif(1L)
    ))
  )
  path <- file.path(state$paths$models, paste0(model_id, ".rds"))
  curation_artifact_sqlite_atomic_save(model, path)
  model_fingerprint <- curation_artifact_sqlite_model_file_fingerprint(path)
  curation_sqlite_with_transaction(
    curation_artifact_sqlite_state_connection(state),
    function() {
      DBI::dbExecute(
        curation_artifact_sqlite_state_connection(state),
        paste(
          "INSERT INTO curation_artifact_models (",
          "project_id, corpus_id, config_id, model_id, revision, path,",
          "model_fingerprint, provenance_blob, created_by, created_at)",
          "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        ),
        params = list(
          scope$project_id,
          scope$corpus_id,
          scope$config_id,
          model_id,
          1L,
          path,
          model_fingerprint,
          curation_artifact_sqlite_serialize(provenance),
          actor$id,
          provenance$created_at
        )
      )
      list(
        ok = TRUE,
        model_id = model_id,
        revision = 1L,
        path = path,
        provenance = provenance
      )
    },
    busy_timeout = state$busy_timeout
  )
}

curation_artifact_sqlite_active_read <- function(state, scope, actor) {
  curation_check_scope(scope, class = "planscanR_error_curation_artifact")
  curation_check_actor(actor)
  row <- DBI::dbGetQuery(
    curation_artifact_sqlite_state_connection(state),
    paste(
      "SELECT * FROM curation_artifact_active_models",
      "WHERE project_id = ? AND corpus_id = ? AND config_id = ? LIMIT 1"
    ),
    params = curation_artifact_sqlite_scope_params(scope)
  )
  if (nrow(row) == 0L) {
    return(NULL)
  }
  list(
    ok = TRUE,
    active_model_id = row$active_model_id[[1L]],
    project_id = row$project_id[[1L]],
    corpus_id = row$corpus_id[[1L]],
    config_id = row$config_id[[1L]],
    active_revision = as.integer(row$active_revision[[1L]]),
    provenance = curation_artifact_sqlite_unserialize(row$provenance_blob[[1L]], list()),
    published_by = row$published_by[[1L]],
    published_at = row$published_at[[1L]]
  )
}

curation_artifact_sqlite_active_publish <- function(
  state,
  scope,
  model_id,
  actor,
  expected_active_revision
) {
  curation_check_scope(scope, class = "planscanR_error_curation_artifact")
  curation_check_actor(actor)
  model_id <- curation_scalar_character(
    model_id,
    "model_id",
    class = "planscanR_error_curation_artifact"
  )
  expected_active_revision <- curation_revision_vector(expected_active_revision, 1L)[[1L]]
  # Validate the target before beginning the active-reference transaction. The
  # model table is immutable, so this remains valid after the short CAS below.
  target <- curation_artifact_sqlite_model_read_row(state, scope, model_id)
  if (nrow(target) == 0L) {
    curation_artifact_sqlite_abort("The requested model is absent from this scope.")
  }
  model <- curation_artifact_sqlite_validate_model_file(target)
  invisible(model)
  curation_sqlite_with_transaction(
    curation_artifact_sqlite_state_connection(state),
    function() {
      conn <- curation_artifact_sqlite_state_connection(state)
      current <- DBI::dbGetQuery(
        conn,
        paste(
          "SELECT * FROM curation_artifact_active_models",
          "WHERE project_id = ? AND corpus_id = ? AND config_id = ? LIMIT 1"
        ),
        params = curation_artifact_sqlite_scope_params(scope)
      )
      current_revision <- if (nrow(current) == 0L) 0L else as.integer(current$active_revision[[1L]])
      if (!identical(current_revision, expected_active_revision)) {
        curation_artifact_sqlite_revision_conflict(
          expected_active_revision,
          current_revision,
          "active model"
        )
      }
      next_revision <- current_revision + 1L
      provenance <- curation_artifact_sqlite_unserialize(target$provenance_blob[[1L]], list())
      if (nrow(current) == 0L) {
        DBI::dbExecute(
          conn,
          paste(
            "INSERT INTO curation_artifact_active_models (",
            "project_id, corpus_id, config_id, active_model_id, active_revision,",
            "provenance_blob, published_by, published_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
          ),
          params = list(
            scope$project_id,
            scope$corpus_id,
            scope$config_id,
            model_id,
            next_revision,
            curation_artifact_sqlite_serialize(provenance),
            actor$id,
            curation_artifact_sqlite_now()
          )
        )
      } else {
        DBI::dbExecute(
          conn,
          paste(
            "UPDATE curation_artifact_active_models SET active_model_id = ?,",
            "active_revision = ?, provenance_blob = ?, published_by = ?, published_at = ?",
            "WHERE project_id = ? AND corpus_id = ? AND config_id = ?"
          ),
          params = list(
            model_id,
            next_revision,
            curation_artifact_sqlite_serialize(provenance),
            actor$id,
            curation_artifact_sqlite_now(),
            scope$project_id,
            scope$corpus_id,
            scope$config_id
          )
        )
      }
      list(
        ok = TRUE,
        active_model_id = model_id,
        active_revision = next_revision,
        provenance = provenance
      )
    },
    busy_timeout = state$busy_timeout
  )
}

#' Build the concrete SQLite/RDS curation artifact store.
#'
#' SQLite stores scoped artifact metadata, immutable revisions, and the active
#' model reference. Snapshot, sample, and model payloads are written atomically
#' as RDS files below the explicit artifact root. Model publication validates
#' the target and uses a compare-and-swap active revision, so failed or stale
#' publications leave the prior active model usable.
#'
#' @param paths Explicit named artifact paths. `root` is required; optional
#' `database`, `snapshots`, `samples`, and `models` values default to children
#' of that root.
#' @param busy_timeout SQLite busy timeout in milliseconds.
#' @return A `planscanR_curation_artifact_store_sqlite`, which also implements
#' the generic `planscanR_curation_artifact_store` callbacks.
#' @export
curation_artifact_store_sqlite <- function(paths, busy_timeout = 5000L) {
  curation_artifact_sqlite_require_packages()
  paths <- curation_artifact_sqlite_paths(paths)
  busy_timeout <- curation_sqlite_scalar_integer(busy_timeout, "busy_timeout")
  if (!dir.exists(paths$root) && !dir.create(paths$root, recursive = TRUE)) {
    curation_artifact_sqlite_abort(
      paste0("Could not create artifact root: ", paths$root)
    )
  }
  for (directory in c(paths$snapshots, paths$samples, paths$models)) {
    if (!dir.exists(directory) && !dir.create(directory, recursive = TRUE)) {
      curation_artifact_sqlite_abort(
        paste0("Could not create artifact directory: ", directory)
      )
    }
  }
  database_parent <- dirname(paths$database)
  if (!dir.exists(database_parent) && !dir.create(database_parent, recursive = TRUE)) {
    curation_artifact_sqlite_abort(
      paste0("Could not create artifact database directory: ", database_parent)
    )
  }
  conn <- tryCatch(
    DBI::dbConnect(RSQLite::SQLite(), dbname = paths$database),
    error = function(error) {
      curation_artifact_sqlite_abort(
        paste0("Could not open the curation artifact database: ", paths$database),
        parent = error
      )
    }
  )
  state <- new.env(parent = emptyenv())
  state$conn <- conn
  state$paths <- paths
  state$busy_timeout <- busy_timeout
  state$closed <- FALSE
  on.exit(
    if (!isTRUE(state$initialized) && DBI::dbIsValid(conn)) {
      try(DBI::dbDisconnect(conn), silent = TRUE)
    },
    add = TRUE
  )
  tryCatch(
    {
      curation_sqlite_set_pragmas(conn, busy_timeout)
      curation_artifact_sqlite_migrate(conn, busy_timeout)
      state$initialized <- TRUE
    },
    error = function(error) {
      if (DBI::dbIsValid(conn)) {
        try(DBI::dbDisconnect(conn), silent = TRUE)
      }
      stop(error)
    }
  )
  store <- curation_artifact_store(
    paths = paths,
    snapshot_read_fn = function(scope, version, actor) {
      curation_artifact_sqlite_snapshot_read(state, scope, version, actor)
    },
    snapshot_write_fn = function(scope, snapshot, actor, expected_revision) {
      curation_artifact_sqlite_snapshot_write(state, scope, snapshot, actor, expected_revision)
    },
    sample_list_fn = function(scope, reviewer_id, actor) {
      curation_artifact_sqlite_sample_list(state, scope, reviewer_id, actor)
    },
    sample_read_fn = function(scope, sample_id, actor) {
      curation_artifact_sqlite_sample_read(state, scope, sample_id, actor)
    },
    sample_write_fn = function(scope, sample, reviewer_id, actor, expected_revision) {
      curation_artifact_sqlite_sample_write(
        state, scope, sample, reviewer_id, actor, expected_revision
      )
    },
    model_list_fn = function(scope, actor) {
      curation_artifact_sqlite_model_list(state, scope, actor)
    },
    model_read_fn = function(scope, model_id, actor) {
      curation_artifact_sqlite_model_read(state, scope, model_id, actor)
    },
    model_write_fn = function(scope, model, provenance, actor, expected_revision) {
      curation_artifact_sqlite_model_write(
        state, scope, model, provenance, actor, expected_revision
      )
    },
    model_publish_fn = function(scope, model_id, actor, expected_active_revision) {
      curation_artifact_sqlite_active_publish(
        state, scope, model_id, actor, expected_active_revision
      )
    },
    active_model_fn = function(scope, actor) {
      curation_artifact_sqlite_active_read(state, scope, actor)
    }
  )
  store$connection <- conn
  store$path <- paths$database
  store$close <- function() {
    if (!isTRUE(state$closed) && DBI::dbIsValid(state$conn)) {
      DBI::dbDisconnect(state$conn)
    }
    state$closed <- TRUE
    invisible(TRUE)
  }
  store$state <- state
  class(store) <- c("planscanR_curation_artifact_store_sqlite", class(store))
  store
}
