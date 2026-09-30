# Review-repository interface. C1b defines the callback boundary only; C2 owns
# the SQLite implementation, migrations, transactions, and import semantics.

#' Build the composite key for one reviewer's decision on one record.
#'
#' The native document ID is scoped by country, and a reviewer's decision is a
#' separate row. The authenticated actor is passed separately to repository
#' operations; `reviewer_id` here is the stable row identity, never a display
#' name supplied by a UI.
#'
#' @param country Source-country identifier.
#' @param document_id Native document identifier within `country`.
#' @param reviewer_id Stable authenticated reviewer identifier.
#' @return A `planscanR_curation_review_key` object.
#' @export
curation_review_key <- function(country, document_id, reviewer_id) {
  out <- list(
    country = curation_scalar_character(
      country,
      "country",
      class = "planscanR_error_curation_repository"
    ),
    document_id = curation_scalar_character(
      document_id,
      "document_id",
      class = "planscanR_error_curation_repository"
    ),
    reviewer_id = curation_scalar_character(
      reviewer_id,
      "reviewer_id",
      class = "planscanR_error_curation_repository"
    )
  )
  structure(out, class = "planscanR_curation_review_key")
}

curation_validate_review_key <- function(key) {
  if (!is.list(key) || is.null(names(key))) {
    curation_abort(
      "{.arg key} must be a curation review key with country, document_id, and reviewer_id.",
      class = "planscanR_error_curation_repository"
    )
  }
  curation_review_key(
    country = key$country,
    document_id = key$document_id,
    reviewer_id = key$reviewer_id
  )
}

curation_review_rows <- function(rows, actor, require_reviewer = FALSE) {
  if (!is.data.frame(rows) || nrow(rows) == 0L) {
    if (is.data.frame(rows) && nrow(rows) == 0L) {
      return(rows)
    }
    curation_abort(
      "Review rows must be a non-empty data frame.",
      class = "planscanR_error_curation_repository"
    )
  }
  required <- c("country", "document_id", "decision", "source")
  if (!all(required %in% names(rows))) {
    curation_abort(
      "Review rows must include country, document_id, decision, and source.",
      class = "planscanR_error_curation_repository"
    )
  }
  if (!"reviewer_id" %in% names(rows)) {
    if (isTRUE(require_reviewer)) {
      curation_abort(
        "Review import rows must include an explicit reviewer_id mapping.",
        class = "planscanR_error_curation_repository"
      )
    }
    rows$reviewer_id <- actor$id
  }
  if (isTRUE(require_reviewer)) {
    required_import <- c("import_source", "import_row_id")
    if (!all(required_import %in% names(rows))) {
      curation_abort(
        "Review import rows must include explicit import_source and import_row_id values.",
        class = "planscanR_error_curation_repository"
      )
    }
    for (nm in required_import) {
      value <- as.character(rows[[nm]])
      if (anyNA(value) || any(!nzchar(trimws(value)))) {
        curation_abort(
          paste0("Review import {.field ", nm, "} values must be non-empty."),
          class = "planscanR_error_curation_repository"
        )
      }
      rows[[nm]] <- trimws(value)
    }
    if ("import_fingerprint" %in% names(rows)) {
      fingerprint <- as.character(rows$import_fingerprint)
      bad <- !is.na(fingerprint) & !nzchar(trimws(fingerprint))
      if (any(bad)) {
        curation_abort(
          "Review import {.field import_fingerprint} values must be non-empty when present.",
          class = "planscanR_error_curation_repository"
        )
      }
      rows$import_fingerprint <- fingerprint
    }
  }
  for (nm in c("country", "document_id", "reviewer_id", "source")) {
    value <- as.character(rows[[nm]])
    if (anyNA(value) || any(!nzchar(trimws(value)))) {
      curation_abort(
        paste0("Review row {.field ", nm, "} values must be non-empty."),
        class = "planscanR_error_curation_repository"
      )
    }
    rows[[nm]] <- trimws(value)
  }
  if (!"deleted" %in% names(rows)) {
    rows$deleted <- rep(FALSE, nrow(rows))
  } else if (!is.logical(rows$deleted) || anyNA(rows$deleted)) {
    curation_abort(
      "Review {.field deleted} values must be non-missing logical values.",
      class = "planscanR_error_curation_repository"
    )
  }
  rows$decision <- as.character(rows$decision)
  bad_decision <- is.na(rows$decision) | !nzchar(trimws(rows$decision))
  if (any(bad_decision & !rows$deleted)) {
    curation_abort(
      "Review row decisions must be non-empty; use a deleted row for a tombstone.",
      class = "planscanR_error_curation_repository"
    )
  }
  if (!"note" %in% names(rows)) {
    rows$note <- rep(NA_character_, nrow(rows))
  } else {
    rows$note <- as.character(rows$note)
  }
  if (!"reviewed_at" %in% names(rows)) {
    rows$reviewed_at <- as.POSIXct(rep(NA_real_, nrow(rows)), origin = "1970-01-01", tz = "UTC")
  }
  if (!inherits(rows$reviewed_at, "POSIXct")) {
    parsed <- curation_parse_timestamp(rows$reviewed_at)
    bad <- is.na(parsed) & !is.na(rows$reviewed_at) & nzchar(as.character(rows$reviewed_at))
    if (any(bad)) {
      curation_abort(
        "Review {.field reviewed_at} values must be parseable timestamps.",
        class = "planscanR_error_curation_repository"
      )
    }
    rows$reviewed_at <- parsed
  }
  if (!"sidecar_path" %in% names(rows)) {
    rows$sidecar_path <- rep(NA_character_, nrow(rows))
  } else {
    rows$sidecar_path <- as.character(rows$sidecar_path)
  }
  if (!"revision" %in% names(rows)) {
    rows$revision <- rep(NA_integer_, nrow(rows))
  } else {
    rows$revision <- curation_revision_vector(rows$revision, nrow(rows), "revision")
  }
  canonical <- c(
    "country", "document_id", "reviewer_id", "decision", "source", "note",
    "reviewed_at", "sidecar_path", "revision", "deleted"
  )
  if (isTRUE(require_reviewer)) {
    canonical <- c(canonical, setdiff(names(rows), canonical))
  }
  rows[, canonical, drop = FALSE]
}

curation_parse_timestamp <- function(value) {
  if (inherits(value, "POSIXct")) {
    return(as.POSIXct(value, tz = "UTC"))
  }
  text <- as.character(value)
  parsed <- as.POSIXct(rep(NA_real_, length(text)), origin = "1970-01-01", tz = "UTC")
  pattern <- paste0(
    "^([0-9]{4})-([0-9]{2})-([0-9]{2})",
    "(?:T| )([0-9]{2}):([0-9]{2}):([0-9]{2}(?:\\.[0-9]+)?)",
    "(Z|[+-][0-9]{2}:?[0-9]{2})?$"
  )
  bad <- !is.na(text) & nzchar(text)
  for (i in which(bad)) {
    date_only <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", text[[i]])
    if (date_only) {
      date <- as.Date(text[[i]], format = "%Y-%m-%d")
      if (!is.na(date) && identical(format(date, "%Y-%m-%d"), text[[i]])) {
        parsed[[i]] <- as.POSIXct(date, tz = "UTC")
        next
      }
      curation_abort(
        "Review {.field reviewed_at} contains an invalid date.",
        class = "planscanR_error_curation_repository"
      )
    }
    match <- regexec(pattern, text[[i]], perl = TRUE)
    parts <- regmatches(text[[i]], match)[[1L]]
    if (length(parts) == 0L) {
      curation_abort(
        "Review {.field reviewed_at} must be a complete ISO-8601 timestamp.",
        class = "planscanR_error_curation_repository"
      )
    }
    year <- as.integer(parts[[2L]])
    month <- as.integer(parts[[3L]])
    day <- as.integer(parts[[4L]])
    hour <- as.integer(parts[[5L]])
    minute <- as.integer(parts[[6L]])
    second <- as.numeric(parts[[7L]])
    date_text <- sprintf("%04d-%02d-%02d", year, month, day)
    date <- as.Date(date_text, format = "%Y-%m-%d")
    if (
      is.na(date) ||
        !identical(format(date, "%Y-%m-%d"), date_text) ||
        hour > 23L ||
        minute > 59L ||
        !is.finite(second) ||
        second >= 60
    ) {
      curation_abort(
        "Review {.field reviewed_at} contains an invalid date or time.",
        class = "planscanR_error_curation_repository"
      )
    }
    base <- as.POSIXct(
      sprintf("%04d-%02d-%02d %02d:%02d:00", year, month, day, hour, minute),
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    )
    parsed[[i]] <- base + second
    zone <- if (length(parts) >= 8L) parts[[8L]] else NA_character_
    if (!is.na(zone) && nzchar(zone) && zone != "Z") {
      zone_parts <- regmatches(zone, regexec("^([+-])([0-9]{2}):?([0-9]{2})$", zone))[[1L]]
      zone_hour <- as.integer(zone_parts[[3L]])
      zone_minute <- as.integer(zone_parts[[4L]])
      if (zone_hour > 23L || zone_minute > 59L) {
        curation_abort(
          "Review {.field reviewed_at} contains an invalid timezone offset.",
          class = "planscanR_error_curation_repository"
        )
      }
      offset <- (zone_hour * 60L + zone_minute) * 60L
      if (zone_parts[[2L]] == "+") {
        parsed[[i]] <- parsed[[i]] - offset
      } else {
        parsed[[i]] <- parsed[[i]] + offset
      }
    }
  }
  parsed
}

curation_page_value <- function(value, arg, allow_null = FALSE) {
  if (isTRUE(allow_null) && is.null(value)) {
    return(NULL)
  }
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      !is.finite(value) ||
      value < 0 ||
      value > .Machine$integer.max ||
      value != trunc(value)
  ) {
    curation_abort(
      paste0("{.arg ", arg, "} must be NULL or a non-negative integer."),
      class = "planscanR_error_curation_repository"
    )
  }
  as.integer(value)
}

curation_check_scope <- function(scope, class = "planscanR_error_curation_repository") {
  if (!inherits(scope, "planscanR_curation_scope")) {
    curation_abort("{.arg scope} must be a planscanR_curation_scope.", class = class)
  }
  invisible(scope)
}

curation_check_actor <- function(actor) {
  if (!inherits(actor, "planscanR_curation_actor")) {
    curation_abort(
      "{.arg actor} must be a trusted planscanR_curation_actor.",
      class = "planscanR_error_curation_actor"
    )
  }
  invisible(actor)
}

curation_revision_vector <- function(expected_revision, n, arg = "expected_revision") {
  valid <- length(expected_revision) == n && is.numeric(expected_revision)
  if (isTRUE(valid) && n > 0L) {
    valid <- all(
      is.finite(expected_revision),
      expected_revision >= 0,
      expected_revision <= .Machine$integer.max,
      expected_revision == trunc(expected_revision)
    )
  }
  if (!isTRUE(valid)) {
    curation_abort(
      paste0(
        "{.arg ", arg, "} must contain exactly ", n,
        " non-negative integer revision value", if (n == 1L) "" else "s", "."
      ),
      class = "planscanR_error_curation_revision"
    )
  }
  as.integer(expected_revision)
}

curation_callback_contract <- function(fn, required, arg) {
  if (!curation_function_accepts(fn, required)) {
    curation_abort(
      paste0(
        "{.arg ", arg, "} must accept ",
        paste0("{.arg ", required, "}", collapse = ", "),
        "."
      ),
      class = "planscanR_error_curation_repository"
    )
  }
  fn
}

#' Build an injected review-repository contract.
#'
#' C2 supplies the SQLite callbacks. Each write callback receives explicit
#' actor and revision values. expected_revision = 0 means that the decision or
#' artifact is expected to be absent. A clear must leave a revisioned tombstone
#' in the implementation so an old client cannot recreate a decision after an
#' intervening clear (the ABA case).
#'
#' @param read_fn Function accepting scope, key, and actor.
#' @param list_fn Function accepting scope, filter, actor, limit, and offset.
#' @param upsert_fn Function accepting scope, decision, actor, and
#'   expected_revision.
#' @param clear_fn Function accepting scope, key, actor, and expected_revision.
#' @param bulk_upsert_fn Atomic function accepting scope, decisions, actor, and
#'   a per-row expected_revision vector.
#' @param import_fn Function accepting scope, rows, actor, and a per-row
#'   expected_revision vector. Import rows carry explicit `import_source` and
#'   `import_row_id` values; an optional `import_fingerprint` is preserved.
#' @param export_fn Function accepting scope, actor, and format.
#' @return A planscanR_curation_review_repository object.
#' @export
curation_review_repository <- function(
  read_fn,
  list_fn,
  upsert_fn,
  clear_fn,
  bulk_upsert_fn,
  import_fn,
  export_fn
) {
  callbacks <- list(
    read_fn = curation_callback_contract(read_fn, c("scope", "key", "actor"), "read_fn"),
    list_fn = curation_callback_contract(
      list_fn,
      c("scope", "filter", "actor", "limit", "offset"),
      "list_fn"
    ),
    upsert_fn = curation_callback_contract(
      upsert_fn,
      c("scope", "decision", "actor", "expected_revision"),
      "upsert_fn"
    ),
    clear_fn = curation_callback_contract(
      clear_fn,
      c("scope", "key", "actor", "expected_revision"),
      "clear_fn"
    ),
    bulk_upsert_fn = curation_callback_contract(
      bulk_upsert_fn,
      c("scope", "decisions", "actor", "expected_revision"),
      "bulk_upsert_fn"
    ),
    import_fn = curation_callback_contract(
      import_fn,
      c("scope", "rows", "actor", "expected_revision"),
      "import_fn"
    ),
    export_fn = curation_callback_contract(
      export_fn,
      c("scope", "actor", "format"),
      "export_fn"
    )
  )
  structure(callbacks, class = "planscanR_curation_review_repository")
}

curation_check_review_repository <- function(repo) {
  if (!inherits(repo, "planscanR_curation_review_repository")) {
    curation_abort(
      "{.arg repo} must be a planscanR_curation_review_repository.",
      class = "planscanR_error_curation_repository"
    )
  }
  invisible(repo)
}

#' Read one review decision through the injected repository.
#'
#' @param repo A planscanR_curation_review_repository.
#' @param scope A planscanR_curation_scope.
#' @param key Record/reviewer key supplied by the caller.
#' @param actor Trusted authenticated actor.
#' @return `NULL` for a never-seen key, or one canonical decision row including
#'   `revision` and a possible `deleted` tombstone.
#' @export
curation_review_read <- function(repo, scope, key, actor) {
  curation_check_review_repository(repo)
  curation_check_scope(scope)
  curation_check_actor(actor)
  key <- curation_validate_review_key(key)
  repo$read_fn(scope = scope, key = key, actor = actor)
}

#' List review decisions through the injected repository.
#'
#' @param repo A planscanR_curation_review_repository.
#' @param scope A planscanR_curation_scope.
#' @param filter Repository-defined filter list.
#' @param actor Trusted authenticated actor.
#' @param limit Optional page size.
#' @param offset Zero-based page offset.
#' @return A canonical decision data frame with country/document/reviewer keys,
#'   decision provenance, timestamp, sidecar path, revision, and deleted flag.
#' @export
curation_review_list <- function(
  repo,
  scope,
  filter = list(),
  actor,
  limit = NULL,
  offset = 0L
) {
  curation_check_review_repository(repo)
  curation_check_scope(scope)
  curation_check_actor(actor)
  if (!is.list(filter)) {
    curation_abort(
      "{.arg filter} must be a list.",
      class = "planscanR_error_curation_repository"
    )
  }
  limit <- curation_page_value(limit, "limit", allow_null = TRUE)
  offset <- curation_page_value(offset, "offset")
  repo$list_fn(
    scope = scope,
    filter = filter,
    actor = actor,
    limit = limit,
    offset = offset
  )
}

#' Upsert one review decision with optimistic concurrency.
#'
#' @param repo A planscanR_curation_review_repository.
#' @param scope A planscanR_curation_scope.
#' @param decision One-row decision data frame.
#' @param actor Trusted authenticated actor.
#' @param expected_revision Required current revision, or 0 for insertion.
#' @return A mutation result containing `ok`, the affected `key`, resulting
#'   `revision`, and `deleted`; conflicts use a classed error.
#' @export
curation_review_upsert <- function(repo, scope, decision, actor, expected_revision = NULL) {
  curation_check_review_repository(repo)
  curation_check_scope(scope)
  curation_check_actor(actor)
  if (!is.data.frame(decision) || nrow(decision) != 1L) {
    curation_abort(
      "{.arg decision} must be a one-row data frame.",
      class = "planscanR_error_curation_repository"
    )
  }
  decision <- curation_review_rows(decision, actor)
  revision <- curation_revision_vector(expected_revision, 1L)
  repo$upsert_fn(
    scope = scope,
    decision = decision,
    actor = actor,
    expected_revision = revision[[1L]]
  )
}

#' Clear one review decision while preserving its revisioned tombstone.
#'
#' @param repo A planscanR_curation_review_repository.
#' @param scope A planscanR_curation_scope.
#' @param key Record/reviewer key supplied by the caller.
#' @param actor Trusted authenticated actor.
#' @param expected_revision Required current revision.
#' @return An atomic mutation result containing `ok`, affected keys, resulting
#'   revisions, and `deleted` flags; conflicts use a classed error.
#' @export
curation_review_clear <- function(repo, scope, key, actor, expected_revision = NULL) {
  curation_check_review_repository(repo)
  curation_check_scope(scope)
  curation_check_actor(actor)
  key <- curation_validate_review_key(key)
  revision <- curation_revision_vector(expected_revision, 1L)
  repo$clear_fn(
    scope = scope,
    key = key,
    actor = actor,
    expected_revision = revision[[1L]]
  )
}

#' Atomically upsert multiple review decisions.
#'
#' @param repo A planscanR_curation_review_repository.
#' @param scope A planscanR_curation_scope.
#' @param decisions Data frame of decisions.
#' @param actor Trusted authenticated actor.
#' @param expected_revision Per-row current revisions; 0 means absent.
#' @return An atomic mutation result containing `ok`, affected keys, resulting
#'   revisions, and `deleted` flags; conflicts use a classed error.
#' @export
curation_review_bulk_upsert <- function(repo, scope, decisions, actor, expected_revision = NULL) {
  curation_check_review_repository(repo)
  curation_check_scope(scope)
  curation_check_actor(actor)
  if (!is.data.frame(decisions)) {
    curation_abort(
      "{.arg decisions} must be a data frame.",
      class = "planscanR_error_curation_repository"
    )
  }
  decisions <- curation_review_rows(decisions, actor)
  revisions <- curation_revision_vector(expected_revision, nrow(decisions))
  repo$bulk_upsert_fn(
    scope = scope,
    decisions = decisions,
    actor = actor,
    expected_revision = revisions
  )
}

#' Import legacy or exported review rows through the repository.
#'
#' @param repo A planscanR_curation_review_repository.
#' @param scope A planscanR_curation_scope.
#' @param rows Review rows to import.
#' @param actor Trusted authenticated actor performing the import.
#' @param expected_revision Per-row current revisions; 0 means absent.
#' @return An atomic mutation result containing `ok`, affected keys, resulting
#'   revisions, and `deleted` flags; conflicts use a classed error.
#' @export
curation_review_import <- function(repo, scope, rows, actor, expected_revision = NULL) {
  curation_check_review_repository(repo)
  curation_check_scope(scope)
  curation_check_actor(actor)
  if (!is.data.frame(rows)) {
    curation_abort(
      "{.arg rows} must be a data frame.",
      class = "planscanR_error_curation_repository"
    )
  }
  rows <- curation_review_rows(rows, actor, require_reviewer = TRUE)
  revisions <- curation_revision_vector(expected_revision, nrow(rows))
  repo$import_fn(
    scope = scope,
    rows = rows,
    actor = actor,
    expected_revision = revisions
  )
}

#' Export review rows through the repository.
#'
#' @param repo A planscanR_curation_review_repository.
#' @param scope A planscanR_curation_scope.
#' @param actor Trusted authenticated actor.
#' @param format Export format, such as csv or tibble.
#' @return A canonical decision data frame with provenance, revisions, and
#'   tombstones represented by `deleted` rows.
#' @export
curation_review_export <- function(repo, scope, actor, format = "csv") {
  curation_check_review_repository(repo)
  curation_check_scope(scope)
  curation_check_actor(actor)
  format <- curation_scalar_character(
    format,
    "format",
    class = "planscanR_error_curation_repository"
  )
  repo$export_fn(scope = scope, actor = actor, format = format)
}
