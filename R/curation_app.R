# Configuration-driven curation app.
#
# The app is deliberately kept behind runtime dependency checks.  The screen
# package remains usable without Shiny, bslib, reactable, or plotly; callers
# opt into this UI by supplying a complete curation context.

curation_app_ui_packages <- function() {
  c("shiny", "bslib", "reactable", "plotly", "htmltools")
}

curation_app_require_ui <- function() {
  missing <- curation_app_ui_packages()[!vapply(
    curation_app_ui_packages(),
    requireNamespace,
    logical(1),
    quietly = TRUE
  )]
  if (length(missing) > 0L) {
    curation_abort(
      paste0(
        "The curation app needs optional package",
        if (length(missing) == 1L) "" else "s",
        ": ", paste(missing, collapse = ", "), "."
      ),
      class = "planscanR_error_curation_missing_dependency"
    )
  }
  invisible(TRUE)
}

curation_app_check_actor_resolver <- function(resolver) {
  if (!inherits(resolver, "planscanR_curation_actor_resolver")) {
    curation_abort(
      "{.arg actor_resolver} must be a trusted curation actor resolver.",
      class = "planscanR_error_curation_actor"
    )
  }
  invisible(resolver)
}

curation_app_empty_records <- function() {
  tibble::tibble(
    country = character(0),
    document_id = character(0),
    title = character(0),
    summary = character(0),
    cosine_max = numeric(0),
    class_label = character(0),
    class_score = numeric(0),
    class_relevant = logical(0),
    kw_total = integer(0),
    n_attachments = integer(0),
    has_attachments = logical(0),
    n_downloaded = integer(0),
    has_downloaded = logical(0),
    selected = logical(0)
  )
}

curation_app_empty_reviews <- function() {
  data.frame(
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
    stringsAsFactors = FALSE
  )
}

curation_app_normalize_historical_links <- function(rows) {
  if (is.null(rows) || !is.data.frame(rows) || nrow(rows) == 0L) {
    return(data.frame(
      import_row_id = character(0), country = character(0), document_id = character(0),
      original_source = character(0), imported_source = character(0),
      historical_reviewer = character(0),
      historical_linkage = character(0), current_linkage = character(0),
      current_match_count = integer(0), linkage_status = character(0),
      mutation_allowed = logical(0), stringsAsFactors = FALSE
    ))
  }
  required <- c("country", "document_id")
  if (!all(required %in% names(rows))) {
    curation_abort(
      "Historical linkage rows must include country and document_id.",
      class = "planscanR_error_curation_repository"
    )
  }
  rows$country <- as.character(rows$country)
  rows$document_id <- as.character(rows$document_id)
  if (!"import_row_id" %in% names(rows)) rows$import_row_id <- NA_character_
  rows$import_row_id <- as.character(rows$import_row_id)
  source_column <- intersect(c("original_source", "historical_source", "source"), names(rows))
  rows$original_source <- if (length(source_column) == 0L) NA_character_ else as.character(rows[[source_column[[1L]]]])
  imported_column <- intersect(c("imported_source", "normalized_source", "source"), names(rows))
  rows$imported_source <- if (length(imported_column) == 0L) NA_character_ else as.character(rows[[imported_column[[1L]]]])
  reviewer_column <- intersect(c("historical_reviewer", "legacy_reviewer", "reviewer", "reviewer_id"), names(rows))
  rows$historical_reviewer <- if (length(reviewer_column) == 0L) NA_character_ else as.character(rows[[reviewer_column[[1L]]]])
  historical_column <- intersect(c("historical_linkage", "metadata_match"), names(rows))
  rows$historical_linkage <- if (length(historical_column) == 0L) "unverified" else as.character(rows[[historical_column[[1L]]]])
  rows$historical_linkage[is.na(rows$historical_linkage) | !nzchar(rows$historical_linkage)] <- "unverified"
  if (!"current_match_count" %in% names(rows)) {
    rows$current_match_count <- NA_integer_
  } else {
    rows$current_match_count <- suppressWarnings(as.integer(rows$current_match_count))
  }
  if (!"current_linkage" %in% names(rows)) {
    rows$current_linkage <- ifelse(
      rows$current_match_count == 1L, "current_unique",
      ifelse(rows$current_match_count == 0L, "historical_only", "unverified")
    )
  } else {
    rows$current_linkage <- as.character(rows$current_linkage)
  }
  rows$current_linkage[is.na(rows$current_linkage) | !nzchar(rows$current_linkage)] <- "unverified"
  if (!"linkage_status" %in% names(rows)) {
    rows$linkage_status <- ifelse(
      !is.na(rows$current_match_count), rows$current_linkage, rows$historical_linkage
    )
  } else {
    rows$linkage_status <- as.character(rows$linkage_status)
  }
  rows$linkage_status[is.na(rows$linkage_status) | !nzchar(rows$linkage_status)] <- "unverified"
  if (!"mutation_allowed" %in% names(rows)) {
    rows$mutation_allowed <- !is.na(rows$current_match_count) & rows$current_match_count == 1L
  } else {
    rows$mutation_allowed <- as.logical(rows$mutation_allowed)
    rows$mutation_allowed[is.na(rows$mutation_allowed)] <- FALSE
  }
  rows
}

curation_app_historical_review_rows <- function(reviews, historical_links = NULL) {
  reviews <- curation_app_normalize_reviews(reviews)
  if (!"import_row_id" %in% names(reviews)) reviews$import_row_id <- NA_character_
  historical_links <- curation_app_normalize_historical_links(historical_links)
  if (nrow(reviews) == 0L || nrow(historical_links) == 0L) {
    return(reviews)
  }
  review_key <- curation_app_key_string(reviews$country, reviews$document_id)
  link_key <- curation_app_key_string(historical_links$country, historical_links$document_id)
  link_row <- vapply(seq_len(nrow(reviews)), function(i) {
    if (!is.na(reviews$import_row_id[[i]]) && nzchar(reviews$import_row_id[[i]])) {
      hit <- which(historical_links$import_row_id == reviews$import_row_id[[i]])
      if (length(hit) == 1L) {
        return(hit)
      }
    }
    hit <- which(link_key == review_key[[i]])
    if (length(hit) == 1L) hit else if (length(hit) > 1L) -1L else 0L
  }, integer(1))
  fields <- c(
    "original_source", "imported_source", "historical_reviewer", "historical_linkage", "current_linkage",
    "current_match_count", "linkage_status", "mutation_allowed"
  )
  for (field in fields) {
    value <- rep(NA, nrow(reviews))
    if (field %in% c("current_match_count")) value <- rep(NA_integer_, nrow(reviews))
    if (field %in% c("mutation_allowed")) value <- rep(TRUE, nrow(reviews))
    valid <- link_row > 0L
    value[valid] <- historical_links[[field]][link_row[valid]]
    ambiguous <- link_row < 0L
    if (field %in% c("historical_linkage", "current_linkage", "linkage_status")) value[ambiguous] <- "ambiguous"
    if (field == "mutation_allowed") value[ambiguous] <- FALSE
    reviews[[field]] <- value
  }
  reviews$historical_mutation_allowed <- as.logical(reviews$mutation_allowed)
  reviews$historical_mutation_allowed[is.na(reviews$historical_mutation_allowed)] <- FALSE
  reviews
}

curation_app_historical_record_rows <- function(records, historical_links = NULL) {
  records <- as.data.frame(records, stringsAsFactors = FALSE)
  if (nrow(records) == 0L) {
    return(records)
  }
  links <- curation_app_normalize_historical_links(historical_links)
  records$historical_mutation_allowed <- TRUE
  records$historical_badge <- rep("", nrow(records))
  if (nrow(links) == 0L) {
    return(records)
  }
  record_key <- curation_app_key_string(records$country, records$document_id)
  link_key <- curation_app_key_string(links$country, links$document_id)
  link_row <- vapply(seq_len(nrow(records)), function(i) {
    hit <- which(link_key == record_key[[i]])
    if (length(hit) == 1L) hit else if (length(hit) > 1L) -1L else 0L
  }, integer(1))
  fields <- c(
    "original_source", "imported_source", "historical_reviewer", "historical_linkage", "current_linkage",
    "current_match_count", "linkage_status", "mutation_allowed"
  )
  for (field in fields) {
    value <- if (field == "current_match_count") rep(NA_integer_, nrow(records)) else rep(NA, nrow(records))
    if (field == "mutation_allowed") value <- rep(TRUE, nrow(records))
    valid <- link_row > 0L
    value[valid] <- links[[field]][link_row[valid]]
    ambiguous <- link_row < 0L
    if (field %in% c("historical_linkage", "current_linkage", "linkage_status")) value[ambiguous] <- "ambiguous"
    if (field == "mutation_allowed") value[ambiguous] <- FALSE
    records[[field]] <- value
  }
  records$historical_mutation_allowed <- as.logical(records$mutation_allowed)
  records$historical_mutation_allowed[is.na(records$historical_mutation_allowed)] <- FALSE
  records$historical_badge <- vapply(seq_len(nrow(records)), function(i) {
    badge <- curation_app_historical_badge(records[i, , drop = FALSE])
    if (is.null(badge)) "" else as.character(badge)
  }, character(1))
  records
}

curation_app_historical_only_records <- function(
  historical_links,
  current_records = NULL,
  reviews = NULL
) {
  links <- curation_app_normalize_historical_links(historical_links)
  if (nrow(links) == 0L) {
    return(curation_app_empty_records()[0, , drop = FALSE])
  }
  current <- if (is.null(current_records) || !is.data.frame(current_records)) {
    character(0)
  } else {
    curation_app_key_string(current_records$country, current_records$document_id)
  }
  link_key <- curation_app_key_string(links$country, links$document_id)
  is_historical_only <- !link_key %in% current | links$current_linkage == "historical_only" |
    links$current_match_count == 0L
  is_historical_only[is.na(is_historical_only)] <- FALSE
  if (!any(is_historical_only)) {
    return(curation_app_empty_records()[0, , drop = FALSE])
  }
  selected <- links[is_historical_only, , drop = FALSE]
  template <- curation_app_empty_records()
  template <- template[rep(NA_integer_, nrow(selected)), , drop = FALSE]
  template$country <- selected$country
  template$document_id <- selected$document_id
  template$import_row_id <- selected$import_row_id
  template$title <- "Historical review"
  template$summary <- "Historical label; current record linkage is unresolved."
  template$selected <- FALSE
  template$decision <- NA_character_
  template$note <- NA_character_
  template$reviewed_at <- NA_character_
  template$reviewer_id <- NA_character_
  template$source <- NA_character_
  review_rows <- curation_app_active_reviews(reviews)
  if (nrow(review_rows) > 0L) {
    review_rows$import_row_id <- if ("import_row_id" %in% names(review_rows)) {
      as.character(review_rows$import_row_id)
    } else {
      rep(NA_character_, nrow(review_rows))
    }
    collapse_review_field <- function(values, hit) {
      values <- as.character(values[hit])
      values <- unique(values[!is.na(values) & nzchar(values)])
      if (length(values) == 0L) NA_character_ else paste(values, collapse = " | ")
    }
    for (i in seq_len(nrow(selected))) {
      hit <- integer(0)
      if (!is.na(selected$import_row_id[[i]]) && nzchar(selected$import_row_id[[i]])) {
        hit <- which(review_rows$import_row_id == selected$import_row_id[[i]])
      }
      if (length(hit) == 0L) {
        hit <- which(
          review_rows$country == selected$country[[i]] &
            review_rows$document_id == selected$document_id[[i]]
        )
      }
      if (length(hit) == 0L) next
      template$decision[[i]] <- collapse_review_field(review_rows$decision, hit)
      template$note[[i]] <- collapse_review_field(review_rows$note, hit)
      template$reviewed_at[[i]] <- collapse_review_field(review_rows$reviewed_at, hit)
      template$reviewer_id[[i]] <- collapse_review_field(review_rows$reviewer_id, hit)
      template$source[[i]] <- collapse_review_field(review_rows$source, hit)
    }
  }
  template <- curation_app_historical_record_rows(template, selected)
  template
}

curation_app_historical_only_table <- function(
  historical_links,
  current_records = NULL,
  reviews = NULL
) {
  rows <- curation_app_historical_only_records(historical_links, current_records, reviews)
  if (nrow(rows) == 0L) {
    return(reactable::reactable(data.frame(Note = "No historical-only labels are available."), pagination = FALSE))
  }
  shown <- rows[, intersect(
    c(
      "country", "document_id", "historical_badge", "historical_reviewer",
      "historical_linkage", "current_linkage", "decision", "note", "reviewed_at",
      "reviewer_id", "source"
    ),
    names(rows)
  ), drop = FALSE]
  columns <- list(
    country = reactable::colDef(name = "Country", width = 90),
    document_id = reactable::colDef(name = "Document", width = 160),
    historical_badge = reactable::colDef(name = "Attribution / linkage", width = 300, html = TRUE),
    historical_reviewer = reactable::colDef(name = "Reviewer", width = 160),
    historical_linkage = reactable::colDef(name = "Historical status", width = 160),
    current_linkage = reactable::colDef(name = "Current status", width = 160),
    decision = reactable::colDef(name = "Repository decision", width = 150),
    note = reactable::colDef(name = "Review note", width = 220),
    reviewed_at = reactable::colDef(name = "Reviewed at", width = 180),
    reviewer_id = reactable::colDef(name = "Repository reviewer", width = 180),
    source = reactable::colDef(name = "Review source", width = 140)
  )
  reactable::reactable(
    shown,
    columns = columns[intersect(names(columns), names(shown))],
    searchable = TRUE,
    filterable = TRUE,
    pagination = TRUE,
    defaultPageSize = 25,
    compact = TRUE,
    highlight = TRUE
  )
}

curation_app_historical_mutation_allowed <- function(
  historical_links,
  country,
  document_id,
  import_row_id = NULL
) {
  links <- curation_app_normalize_historical_links(historical_links)
  if (nrow(links) == 0L) {
    return(TRUE)
  }
  hit <- integer(0)
  if (!is.null(import_row_id) && length(import_row_id) == 1L && !is.na(import_row_id) && nzchar(import_row_id)) {
    hit <- which(links$import_row_id == as.character(import_row_id))
  }
  if (length(hit) == 0L) {
    hit <- which(links$country == as.character(country) & links$document_id == as.character(document_id))
  }
  if (length(hit) != 1L) {
    return(length(hit) == 0L)
  }
  isTRUE(links$mutation_allowed[[hit]])
}

curation_app_historical_badge <- function(row) {
  if (is.data.frame(row)) {
    if (nrow(row) == 0L) {
      return(NULL)
    }
    row <- row[1L, , drop = FALSE]
    get <- function(name, default = NULL) if (name %in% names(row)) row[[name]][[1L]] else default
  } else if (is.list(row)) {
    get <- function(name, default = NULL) row[[name]] %||% default
  } else {
    return(NULL)
  }
  original <- get("original_source")
  imported <- get("imported_source")
  reviewer <- get("historical_reviewer")
  status <- get("linkage_status")
  allowed <- isTRUE(get("mutation_allowed", TRUE))
  if (all(vapply(list(original, imported, status), function(value) is.null(value) || is.na(value) || !nzchar(as.character(value)), logical(1)))) {
    return(NULL)
  }
  source_text <- if (!is.null(original) && !is.na(original) && !is.null(imported) && !is.na(imported) &&
    nzchar(as.character(original)) && nzchar(as.character(imported)) &&
    !identical(as.character(original), as.character(imported))) {
    paste0("original ", as.character(original), " · imported ", as.character(imported))
  } else if (!is.null(imported) && !is.na(imported) && nzchar(as.character(imported))) {
    paste0("source ", as.character(imported))
  } else {
    "historical"
  }
  status_text <- switch(as.character(status %||% "unverified"),
    current_unique = "current match",
    matched = "current match",
    historical_only = "historical only",
    ambiguous = "ambiguous",
    pending = "pending",
    unseen = "unseen",
    `not_seen_in_returned_metadata` = "unseen",
    unverified = "unverified",
    as.character(status %||% "unverified")
  )
  label <- paste("Historical", source_text, status_text, sep = " · ")
  if (!is.null(reviewer) && !is.na(reviewer) && nzchar(as.character(reviewer))) {
    label <- paste(label, paste0("by ", as.character(reviewer)), sep = " · ")
  }
  if (!allowed) label <- paste(label, "read-only", sep = " · ")
  htmltools::tags$span(
    class = paste("curation-historical-badge", if (allowed) "is-actionable" else "is-unresolved"),
    title = if (allowed) "Historical linkage is uniquely matched to the current record." else "Historical linkage is unresolved; current mutations are disabled.",
    label
  )
}

curation_app_normalize_reviews <- function(rows) {
  if (is.null(rows) || !is.data.frame(rows) || nrow(rows) == 0L) {
    return(curation_app_empty_reviews())
  }
  if (!"reviewer_id" %in% names(rows) && "reviewer" %in% names(rows)) {
    rows$reviewer_id <- as.character(rows$reviewer)
  }
  required <- c("country", "document_id", "reviewer_id", "decision", "source")
  missing <- setdiff(required, names(rows))
  if (length(missing) > 0L) {
    curation_abort(
      paste0("The review repository omitted required columns: ", paste(missing, collapse = ", "), "."),
      class = "planscanR_error_curation_repository"
    )
  }
  if (!"note" %in% names(rows)) rows$note <- NA_character_
  if (!"reviewed_at" %in% names(rows)) {
    rows$reviewed_at <- as.POSIXct(rep(NA_real_, nrow(rows)), origin = "1970-01-01", tz = "UTC")
  }
  if (!"sidecar_path" %in% names(rows)) rows$sidecar_path <- NA_character_
  if (!"revision" %in% names(rows)) rows$revision <- 0L
  if (!"deleted" %in% names(rows)) rows$deleted <- FALSE
  rows$country <- as.character(rows$country)
  rows$document_id <- as.character(rows$document_id)
  rows$reviewer_id <- as.character(rows$reviewer_id)
  rows$decision <- as.character(rows$decision)
  rows$source <- as.character(rows$source)
  rows$revision <- as.integer(rows$revision)
  rows$deleted <- as.logical(rows$deleted)
  rows[, c(
    "country", "document_id", "reviewer_id", "decision", "source", "note",
    "reviewed_at", "sidecar_path", "revision", "deleted",
    setdiff(names(rows), c(
      "country", "document_id", "reviewer_id", "decision", "source", "note",
      "reviewed_at", "sidecar_path", "revision", "deleted"
    ))
  ), drop = FALSE]
}

curation_app_active_reviews <- function(rows) {
  rows <- curation_app_normalize_reviews(rows)
  if (nrow(rows) == 0L) {
    return(rows)
  }
  rows[!rows$deleted & !is.na(rows$decision) & nzchar(rows$decision), , drop = FALSE]
}

curation_app_admin_roles <- function() {
  c("admin", "curation_admin", "review_admin")
}

curation_app_is_admin <- function(actor) {
  is.list(actor) && !is.null(actor$roles) &&
    any(as.character(actor$roles) %in% curation_app_admin_roles())
}

curation_app_key_string <- function(country, document_id, reviewer_id = NULL) {
  if (length(country) == 0L || length(document_id) == 0L) {
    return(character(0))
  }
  key <- paste0(as.character(country), "::", as.character(document_id))
  if (!is.null(reviewer_id)) {
    key <- paste0(key, "::", as.character(reviewer_id))
  }
  key
}

curation_app_current_revision <- function(rows, country, document_id, reviewer_id) {
  rows <- curation_app_normalize_reviews(rows)
  if (nrow(rows) == 0L) {
    return(0L)
  }
  hit <- rows$country == as.character(country) &
    rows$document_id == as.character(document_id) &
    rows$reviewer_id == as.character(reviewer_id)
  if (!any(hit)) {
    return(0L)
  }
  as.integer(rows$revision[which(hit)[1L]])
}

curation_app_review_revision_lookup <- function(rows, reviewer_id) {
  rows <- curation_app_normalize_reviews(rows)
  if (nrow(rows) == 0L) {
    return(stats::setNames(integer(0), character(0)))
  }
  rows <- rows[rows$reviewer_id == as.character(reviewer_id), , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(stats::setNames(integer(0), character(0)))
  }
  revisions <- as.integer(rows$revision)
  revisions[is.na(revisions) | revisions < 0L] <- 0L
  stats::setNames(
    revisions,
    curation_app_key_string(rows$country, rows$document_id, reviewer_id)
  )
}

curation_app_attach_review_revisions <- function(data, rows, reviewer_id) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  lookup <- curation_app_review_revision_lookup(rows, reviewer_id)
  keys <- curation_app_key_string(data$country, data$document_id, reviewer_id)
  data$review_revision <- as.integer(lookup[keys])
  data$review_revision[is.na(data$review_revision) | data$review_revision < 0L] <- 0L
  data
}

curation_app_review_display_rows <- function(data, rows, reviewer_id) {
  data <- curation_app_attach_review_revisions(data, rows, reviewer_id)
  if (nrow(data) == 0L) {
    return(list())
  }
  lapply(seq_len(nrow(data)), function(i) {
    list(
      index = as.integer(i - 1L),
      country = as.character(data$country[[i]]),
      document_id = as.character(data$document_id[[i]]),
      revision = as.integer(data$review_revision[[i]])
    )
  })
}

curation_app_review_display_token <- function(data, rows, reviewer_id) {
  curation_artifact_content_fingerprint(
    curation_app_review_display_rows(data, rows, reviewer_id)
  )
}

curation_app_validate_displayed_revision <- function(value) {
  if (is.null(value) || length(value) == 0L) {
    return(NULL)
  }
  value <- suppressWarnings(as.integer(value[[1L]]))
  if (length(value) != 1L || is.na(value) || value < 0L) {
    curation_abort(
      "The review event did not include a valid displayed revision.",
      class = "planscanR_error_curation_conflict"
    )
  }
  value
}

curation_app_align_review_row <- function(rows, row, previous = NULL) {
  missing <- setdiff(names(rows), names(row))
  for (name in missing) {
    value <- if (!is.null(previous) && name %in% names(previous)) {
      previous[[name]][[1L]]
    } else {
      rows[[name]][NA_integer_]
    }
    row[[name]] <- value
  }
  additional <- setdiff(names(row), names(rows))
  for (name in additional) {
    rows[[name]] <- row[[name]][rep(NA_integer_, nrow(rows))]
  }
  row[, names(rows), drop = FALSE]
}

curation_app_replace_review_result <- function(rows, decision, result, actor) {
  rows <- curation_app_normalize_reviews(rows)
  if (is.null(result) || is.null(result$key)) {
    return(rows)
  }
  key <- result$key
  reviewer_id <- key$reviewer_id %||% actor$id
  hit <- rows$country == key$country &
    rows$document_id == key$document_id &
    rows$reviewer_id == reviewer_id
  previous <- if (any(hit)) rows[which(hit)[[1L]], , drop = FALSE] else NULL
  if (any(hit)) rows <- rows[!hit, , drop = FALSE]
  if (isTRUE(result$deleted)) {
    previous_source <- if (!is.null(previous)) as.character(previous$source[[1L]]) else NA_character_
    if (is.na(previous_source) || !nzchar(previous_source)) previous_source <- "clear"
    tombstone <- data.frame(
      country = key$country,
      document_id = key$document_id,
      reviewer_id = reviewer_id,
      decision = NA_character_,
      source = if (is.null(decision)) previous_source else decision$source[[1L]],
      note = if (is.null(decision) && !is.null(previous)) previous$note[[1L]] else if (is.null(decision)) NA_character_ else decision$note[[1L]],
      reviewed_at = if (is.null(decision) && !is.null(previous)) {
        previous$reviewed_at[[1L]]
      } else if (is.null(decision)) {
        as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
      } else {
        decision$reviewed_at[[1L]]
      },
      sidecar_path = if (is.null(decision) && !is.null(previous)) previous$sidecar_path[[1L]] else if (is.null(decision)) NA_character_ else decision$sidecar_path[[1L]],
      revision = as.integer(result$revision),
      deleted = TRUE,
      stringsAsFactors = FALSE
    )
    tombstone <- curation_app_align_review_row(rows, tombstone, previous)
    return(rbind(rows, tombstone))
  }
  if (is.null(decision)) {
    return(rows)
  }
  decision$revision <- as.integer(result$revision)
  decision$deleted <- FALSE
  decision$reviewer_id <- reviewer_id
  decision <- curation_app_align_review_row(rows, decision, previous)
  curation_app_normalize_reviews(rbind(rows, decision))
}

# Merge one canonical repository row into a session-local review frame.  A
# refresh is deliberately a read-only operation: callers use it after a CAS
# conflict and must never retry the stale write with the newly read revision.
curation_app_merge_review_row <- function(rows, current) {
  rows <- curation_app_normalize_reviews(rows)
  if (is.null(current) || !is.data.frame(current) || nrow(current) == 0L) {
    return(rows)
  }
  current <- curation_app_normalize_reviews(current[1L, , drop = FALSE])
  hit <- rows$country == current$country[[1L]] &
    rows$document_id == current$document_id[[1L]] &
    rows$reviewer_id == current$reviewer_id[[1L]]
  previous <- if (any(hit)) rows[which(hit)[[1L]], , drop = FALSE] else NULL
  if (any(hit)) rows <- rows[!hit, , drop = FALSE]
  current <- curation_app_align_review_row(rows, current, previous)
  curation_app_normalize_reviews(rbind(rows, current))
}

curation_app_review_keys <- function(rows) {
  rows <- curation_app_normalize_reviews(rows)
  if (nrow(rows) == 0L) {
    return(character(0))
  }
  curation_app_key_string(rows$country, rows$document_id, rows$reviewer_id)
}

# Merge a repository poll without replacing a review that the browser is in
# the middle of saving.  The pending key remains at its last locally observed
# revision, so a later write still gets a CAS conflict instead of silently
# advancing over another actor's change.
curation_app_merge_reviews_preserving_pending <- function(
  local,
  incoming,
  pending_keys = character(0)
) {
  local <- curation_app_normalize_reviews(local)
  incoming <- curation_app_normalize_reviews(incoming)
  pending_keys <- unique(as.character(pending_keys %||% character(0)))
  if (nrow(incoming) == 0L) {
    return(local)
  }
  for (i in seq_len(nrow(incoming))) {
    row <- incoming[i, , drop = FALSE]
    key <- curation_app_key_string(row$country, row$document_id, row$reviewer_id)
    if (key %in% pending_keys) next
    existing <- curation_app_review_keys(local) == key
    if (any(existing)) local <- local[!existing, , drop = FALSE]
    local <- curation_app_normalize_reviews(rbind(local, row))
  }
  local
}

curation_app_review_refresh_updates <- function(
  local,
  incoming,
  pending_keys = character(0)
) {
  local <- curation_app_normalize_reviews(local)
  incoming <- curation_app_normalize_reviews(incoming)
  pending_keys <- unique(as.character(pending_keys %||% character(0)))
  if (nrow(incoming) == 0L) {
    return(list())
  }
  updates <- list()
  local_keys <- curation_app_review_keys(local)
  for (i in seq_len(nrow(incoming))) {
    row <- incoming[i, , drop = FALSE]
    key <- curation_app_key_string(row$country, row$document_id, row$reviewer_id)
    if (key %in% pending_keys) next
    hit <- which(local_keys == key)
    old_decision <- if (length(hit) == 0L) {
      NULL
    } else if (isTRUE(local$deleted[[hit[[1L]]]]) || is.na(local$decision[[hit[[1L]]]])) {
      NULL
    } else {
      as.character(local$decision[[hit[[1L]]]])
    }
    new_decision <- if (isTRUE(row$deleted[[1L]]) || is.na(row$decision[[1L]])) {
      NULL
    } else {
      as.character(row$decision[[1L]])
    }
    old_revision <- if (length(hit) == 0L) 0L else as.integer(local$revision[[hit[[1L]]]])
    new_revision <- as.integer(row$revision[[1L]])
    if (is.na(old_revision)) old_revision <- 0L
    if (is.na(new_revision)) new_revision <- 0L
    if (!identical(old_decision, new_decision) || !identical(old_revision, new_revision)) {
      updates[[length(updates) + 1L]] <- list(
        key = key,
        decision = new_decision,
        revision = new_revision
      )
    }
  }
  updates
}

curation_app_canonical_review_update <- function(rows, key) {
  rows <- curation_app_normalize_reviews(rows)
  hit <- rows$country == as.character(key$country) &
    rows$document_id == as.character(key$document_id) &
    rows$reviewer_id == as.character(key$reviewer_id)
  if (!any(hit)) {
    return(list(
      key = curation_app_key_string(key$country, key$document_id, key$reviewer_id),
      decision = NULL,
      revision = 0L
    ))
  }
  row <- rows[which(hit)[[1L]], , drop = FALSE]
  list(
    key = curation_app_key_string(row$country[[1L]], row$document_id[[1L]], row$reviewer_id[[1L]]),
    decision = if (isTRUE(row$deleted[[1L]]) || is.na(row$decision[[1L]])) NULL else as.character(row$decision[[1L]]),
    revision = if (is.na(row$revision[[1L]])) 0L else as.integer(row$revision[[1L]])
  )
}

curation_app_refresh_review <- function(rows, repository, scope, key, actor) {
  current <- curation_review_read(repository, scope, key, actor)
  if (is.null(current)) {
    rows <- curation_app_normalize_reviews(rows)
    hit <- rows$country == key$country &
      rows$document_id == key$document_id &
      rows$reviewer_id == key$reviewer_id
    return(rows[!hit, , drop = FALSE])
  }
  curation_app_merge_review_row(rows, current)
}

curation_app_record_key <- function(row) {
  list(country = as.character(row$country[[1L]]), document_id = as.character(row$document_id[[1L]]))
}

curation_app_request_for_session <- function(request, session) {
  if (is.function(request)) {
    return(request(session = session))
  }
  if (is.null(request)) {
    return(list(session = session))
  }
  request
}

curation_app_resolve_current_actor <- function(
  context,
  session,
  initial_actor,
  resolver = context$actor_resolver
) {
  curation_app_check_actor_resolver(resolver)
  current <- curation_resolve_actor(
    resolver,
    curation_app_request_for_session(context$request, session)
  )
  if (!identical(current$id, initial_actor$id)) {
    curation_abort(
      "The authenticated actor changed; reload the curation session.",
      class = "planscanR_error_curation_actor"
    )
  }
  current
}

curation_app_translate <- function(config, row) {
  key <- curation_app_record_key(row)
  result <- curation_translate(
    provider = config$translator,
    record = row,
    key = key,
    write = FALSE
  )
  list(
    title_en = result$title_en %||% NA_character_,
    summary_en = result$summary_en %||% NA_character_,
    status = result$status %||% "disabled"
  )
}

curation_app_has_text <- function(value) {
  !is.null(value) && length(value) == 1L && !is.na(value) && nzchar(trimws(as.character(value)))
}

curation_app_column_value <- function(row, column, default = NULL) {
  if (!is.data.frame(row) || nrow(row) == 0L || !column %in% names(row)) {
    return(default)
  }
  value <- row[[column]][[1L]]
  if (length(value) != 1L || is.na(value)) default else value
}

curation_app_record_detail <- function(row, config) {
  has <- curation_app_has_text
  tr <- curation_app_translate(config, row)
  text_or <- function(value, fallback) if (has(value)) as.character(value) else fallback
  country <- as.character(curation_app_column_value(row, "country", ""))
  document_id <- as.character(curation_app_column_value(row, "document_id", ""))
  title <- curation_app_column_value(row, "title")
  summary <- curation_app_column_value(row, "summary")
  url <- curation_app_column_value(row, "url")
  htmltools::div(
    style = "padding:12px 16px;background:#fafafa;",
    htmltools::div(
      style = "color:#777;font-size:12px;margin-bottom:8px;",
      sprintf("ID: %s \u00b7 %s", document_id, toupper(country))
    ),
    htmltools::div(
      style = "display:flex;gap:20px;flex-wrap:wrap;",
      htmltools::div(
        style = "flex:1;min-width:280px;",
        htmltools::tags$b("Original"),
        htmltools::tags$h5(text_or(title, "(no title)")),
        htmltools::tags$p(text_or(summary, "(no summary)"), style = "white-space:pre-wrap;")
      ),
      htmltools::div(
        style = "flex:1;min-width:280px;border-left:1px solid #ddd;padding-left:20px;",
        htmltools::tags$b("English"),
        if (tr$status == "disabled") {
          htmltools::tags$p("Translation disabled; original text shown.", style = "color:#888;")
        } else {
          htmltools::tagList(
            htmltools::tags$h5(text_or(tr$title_en, "(no title)")),
            htmltools::tags$p(text_or(tr$summary_en, "(no summary)"), style = "white-space:pre-wrap;")
          )
        }
      )
    ),
    if (has(url)) {
      htmltools::tags$a(href = url, target = "_blank", "Open source page \u2197")
    }
  )
}

curation_app_decision_cell <- function(source, reviewer_id, render_id = NULL) {
  js_literal <- function(value) {
    value <- gsub("\\\\", "\\\\\\\\", as.character(value))
    value <- gsub("'", "\\'", value, fixed = TRUE)
    value <- gsub("\r", "\\r", value, fixed = TRUE)
    value <- gsub("\n", "\\n", value, fixed = TRUE)
    value <- gsub("\u2028", "\\u2028", value, fixed = TRUE)
    gsub("\u2029", "\\u2029", value, fixed = TRUE)
  }
  render_attr <- if (is.null(render_id)) "" else paste0(" data-render-id=\"", js_literal(render_id), "\"")
  reactable::JS(sprintf(
    "function(cellInfo) {
      var src = '%s';
      var id = cellInfo.row['document_id'];
      var country = cellInfo.row['country'];
      var rid = '%s';
      var key = String(country) + '::' + String(id) + '::' + rid;
      var revision = cellInfo.row['review_revision'];
      var val = cellInfo.value;
      var mutationAllowed = cellInfo.row['historical_mutation_allowed'];
      var mutationBlocked = mutationAllowed === false || String(mutationAllowed).toLowerCase() === 'false' || mutationAllowed === 0;
      if (mutationBlocked) return '<span class=\"curation-historical-readonly\" data-mutation-allowed=\"false\" title=\"Historical linkage is unresolved; current mutations are disabled.\">Historical linkage is unresolved; current mutations are disabled.</span>';
      if (window.__curationReviewState === undefined) window.__curationReviewState = {};
      if (window.__curationReviewRevision === undefined) window.__curationReviewRevision = {};
      if (!(key in window.__curationReviewState)) {
        window.__curationReviewState[key] = (val === null || val === undefined) ? null : val;
      }
      if (!(key in window.__curationReviewRevision) || window.__curationReviewRevision[key] === undefined) {
        window.__curationReviewRevision[key] = Number(revision) || 0;
      }
      var cur = window.__curationReviewState[key];
      function attr(s) {
        return String(s)
          .replace(/&/g, '&amp;')
          .replace(/\\r/g, '&#13;')
          .replace(/\\n/g, '&#10;')
          .replace(/\\\"/g, '&quot;')
          .replace(/</g, '&lt;')
          .replace(/>/g, '&gt;');
      }
      function btn(dec, label, color) {
        var active = (cur === dec) ? ' active' : '';
        return '<button type=\"button\" class=\"curation-decision' + active + '\"' +
          ' data-decision=\"' + attr(dec) + '\" style=\"--curation-color:' + attr(color) + '\"' +
          '>' + label + '</button>';
      }
      return '<div class=\"curation-decision-group\" data-review-id=\"' + attr(key) +
        '\" data-record-id=\"' + attr(id) + '\" data-country=\"' + attr(country) +
        '\" data-source=\"' + attr(src) + '\" data-reviewer=\"' + attr(rid) +
        '\" data-revision=\"' + attr(revision) + '\" data-mutation-allowed=\"' + attr(mutationAllowed) + '\"%s>' +
        btn('keep','Keep','#1a9850') + btn('drop','Drop','#d73027') + btn('unsure','?','#fc8d59') + '</div>';
    }",
    js_literal(source),
    js_literal(reviewer_id),
    render_attr
  ))
}

curation_app_admin_decision_cell <- function() {
  reactable::JS(
    "function(cellInfo) {
      var id = cellInfo.row['document_id'];
      var country = cellInfo.row['country'];
      var rid = cellInfo.row['reviewer_id'];
      var revision = cellInfo.row['review_revision'];
      var key = String(country) + '::' + String(id) + '::' + String(rid);
      var val = cellInfo.value;
      var mutationAllowed = cellInfo.row['historical_mutation_allowed'];
      var mutationBlocked = mutationAllowed === false || String(mutationAllowed).toLowerCase() === 'false' || mutationAllowed === 0;
      if (mutationBlocked) return '<span class=\"curation-historical-readonly\" data-mutation-allowed=\"false\" title=\"Historical linkage is unresolved; current mutations are disabled.\">Historical linkage is unresolved; current mutations are disabled.</span>';
      if (window.__curationReviewState === undefined) window.__curationReviewState = {};
      if (!(key in window.__curationReviewState)) {
        window.__curationReviewState[key] = (val === null || val === undefined) ? null : val;
      }
      function attr(s) {
        return String(s)
          .replace(/&/g, '&amp;')
          .replace(/\\r/g, '&#13;')
          .replace(/\\n/g, '&#10;')
          .replace(/\\\"/g, '&quot;')
          .replace(/</g, '&lt;')
          .replace(/>/g, '&gt;');
      }
      function btn(dec, label, color) {
        var active = (val === dec) ? ' active' : '';
        return '<button type=\"button\" class=\"curation-decision' + active + '\"' +
          ' data-decision=\"' + attr(dec) + '\" style=\"--curation-color:' + attr(color) + '\"' +
          '>' + label + '</button>';
      }
      return '<div class=\"curation-decision-group\" data-admin=\"true\" data-review-id=\"' + attr(key) +
        '\" data-record-id=\"' + attr(id) + '\" data-country=\"' + attr(country) +
        '\" data-source=\"admin\" data-reviewer=\"' + attr(rid) +
        '\" data-revision=\"' + attr(revision) + '\" data-mutation-allowed=\"' + attr(mutationAllowed) + '\">' +
        btn('keep','Keep','#1a9850') + btn('drop','Drop','#d73027') + btn('unsure','?','#fc8d59') + '</div>';
    }"
  )
}

curation_app_admin_table <- function(rows, historical_links = NULL) {
  rows <- curation_app_active_reviews(rows)
  if (nrow(rows) == 0L) {
    return(reactable::reactable(data.frame(Note = "No active reviews are available."), pagination = FALSE))
  }
  rows <- curation_app_historical_review_rows(rows, historical_links)
  rows$review_revision <- as.integer(rows$revision)
  rows$decision <- as.character(rows$decision)
  rows$historical_badge <- vapply(seq_len(nrow(rows)), function(i) {
    badge <- curation_app_historical_badge(rows[i, , drop = FALSE])
    if (is.null(badge)) "" else as.character(badge)
  }, character(1))
  columns <- list(
    decision = reactable::colDef(
      name = "Review", sticky = "left", width = 160, sortable = FALSE,
      filterable = FALSE, html = TRUE, cell = curation_app_admin_decision_cell()
    ),
    country = reactable::colDef(name = "Country", width = 90, align = "center"),
    document_id = reactable::colDef(name = "Document", width = 150),
    reviewer_id = reactable::colDef(name = "Target reviewer", width = 160),
    source = reactable::colDef(name = "Source", width = 100),
    revision = reactable::colDef(name = "Revision", width = 90, align = "center"),
    historical_badge = reactable::colDef(name = "Attribution / linkage", width = 300, html = TRUE)
  )
  show <- c("decision", "country", "document_id", "reviewer_id", "source", "revision", "historical_badge")
  for (name in setdiff(names(rows), show)) columns[[name]] <- reactable::colDef(show = FALSE)
  reactable::reactable(
    rows[, unique(c(show, setdiff(names(rows), show))), drop = FALSE],
    columns = columns,
    searchable = TRUE,
    filterable = TRUE,
    resizable = TRUE,
    pagination = TRUE,
    defaultPageSize = 25,
    compact = TRUE,
    highlight = TRUE,
    defaultColDef = reactable::colDef(headerStyle = list(whiteSpace = "normal"))
  )
}

curation_app_table <- function(data, reviews, config, source = "browse", blind = FALSE, reviewer_id, render_id = NULL, historical_links = NULL) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (nrow(data) == 0L) {
    return(reactable::reactable(data.frame(Note = "No records match this view."), pagination = FALSE))
  }
  data <- curation_app_historical_record_rows(data, historical_links)
  all_reviews <- curation_app_normalize_reviews(reviews)
  reviews <- curation_app_active_reviews(all_reviews)
  lookup <- stats::setNames(
    reviews$decision[reviews$reviewer_id == reviewer_id],
    curation_app_key_string(
      reviews$country[reviews$reviewer_id == reviewer_id],
      reviews$document_id[reviews$reviewer_id == reviewer_id],
      reviewer_id
    )
  )
  data$decision <- unname(lookup[curation_app_key_string(data$country, data$document_id, reviewer_id)])
  data <- curation_app_attach_review_revisions(data, all_reviews, reviewer_id)
  has_model <- "select_prob" %in% names(data) && any(!is.na(data$select_prob))
  trunc_cell <- function(value) {
    htmltools::div(
      title = if (is.na(value)) "" else as.character(value),
      style = "white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
      if (is.na(value)) "" else as.character(value)
    )
  }
  cols <- list(
    decision = reactable::colDef(
      name = "Review", sticky = "left", width = 160, sortable = FALSE,
      filterable = FALSE, html = TRUE, cell = curation_app_decision_cell(source, reviewer_id, render_id)
    ),
    country = reactable::colDef(name = "Country", width = 90, align = "center"),
    document_id = reactable::colDef(name = "Document", width = 130),
    title = reactable::colDef(name = "Title", minWidth = 220, cell = trunc_cell),
    cosine_max = reactable::colDef(name = "Topic match", width = 120, format = reactable::colFormat(digits = 3)),
    class_label = reactable::colDef(name = "Category", width = 140),
    class_score = reactable::colDef(name = "Category confidence", width = 160, format = reactable::colFormat(digits = 2)),
    kw_total = reactable::colDef(name = "Keywords", width = 90, align = "center"),
    selected = reactable::colDef(name = "Pre-selected", width = 120),
    selected_model = reactable::colDef(name = "Model", width = 100),
    n_attachments = reactable::colDef(name = "Attachments", width = 100, align = "center"),
    historical_badge = reactable::colDef(name = "Attribution / linkage", width = 300, html = TRUE)
  )
  if (!has_model) cols$selected_model <- NULL
  if (isTRUE(blind)) {
    for (column in c("cosine_max", "class_label", "class_score", "kw_total", "selected", "selected_model")) {
      cols[[column]] <- NULL
    }
  }
  present <- intersect(names(cols), names(data))
  show <- c("decision", present[present != "decision"])
  for (column in setdiff(names(data), show)) cols[[column]] <- reactable::colDef(show = FALSE)
  cols <- cols[intersect(names(cols), names(data))]
  reactable::reactable(
    data,
    columns = cols,
    selection = "multiple",
    onClick = "expand",
    details = function(index) curation_app_record_detail(data[index, , drop = FALSE], config),
    searchable = TRUE,
    filterable = TRUE,
    resizable = TRUE,
    defaultPageSize = 25,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(25, 50, 100, 250),
    compact = TRUE,
    highlight = TRUE,
    defaultColDef = reactable::colDef(headerStyle = list(whiteSpace = "normal"))
  )
}

curation_app_single_record_ui <- function(row, config) {
  if (is.null(row) || nrow(row) == 0L) {
    return(shiny::helpText("Draw a sample to begin."))
  }
  tr <- curation_app_translate(config, row)
  has <- curation_app_has_text
  val <- function(x) if (length(x) == 1L && !is.na(x) && nzchar(trimws(as.character(x)))) as.character(x) else NULL
  country <- as.character(curation_app_column_value(row, "country", ""))
  document_id <- as.character(curation_app_column_value(row, "document_id", ""))
  title <- curation_app_column_value(row, "title")
  summary <- curation_app_column_value(row, "summary")
  url <- curation_app_column_value(row, "url")
  meta <- list(
    Date = val(curation_app_column_value(row, "date_decision") %||% curation_app_column_value(row, "year")),
    Category = val(curation_app_column_value(row, "native_type")),
    Status = val(curation_app_column_value(row, "status")),
    Authority = val(curation_app_column_value(row, "competent_authority")),
    Proponent = val(curation_app_column_value(row, "proponent")),
    Jurisdiction = val(curation_app_column_value(row, "jurisdiction"))
  )
  meta <- meta[!vapply(meta, is.null, logical(1))]
  meta_ui <- if (length(meta) == 0L) {
    NULL
  } else {
    htmltools::div(
      style = "display:flex;gap:8px;flex-wrap:wrap;margin-bottom:12px;",
      lapply(names(meta), function(name) {
        htmltools::span(
          paste0(name, ": ", meta[[name]]),
          style = "background:#eef2f5;border-radius:10px;padding:3px 10px;font-size:12px;"
        )
      })
    )
  }
  text_or <- function(value, fallback) if (has(value)) as.character(value) else fallback
  htmltools::div(
    style = "font-size:15px;line-height:1.5;",
    htmltools::div(
      style = "color:#777;font-size:12px;margin-bottom:8px;",
      sprintf("ID: %s \u00b7 %s", document_id, toupper(country))
    ),
    meta_ui,
    htmltools::div(
      style = "display:flex;gap:24px;flex-wrap:wrap;",
      htmltools::div(
        style = "flex:1;min-width:320px;",
        htmltools::tags$b("Original"),
        htmltools::tags$h4(text_or(title, "(no title)")),
        htmltools::tags$p(text_or(summary, "(no summary)"), style = "white-space:pre-wrap;")
      ),
      htmltools::div(
        style = "flex:1;min-width:320px;",
        htmltools::tags$b("English"),
        if (tr$status == "disabled") {
          htmltools::tags$p("Translation disabled; original text shown.", style = "color:#888;")
        } else {
          htmltools::tagList(
            htmltools::tags$h4(text_or(tr$title_en, "(no title)")),
            htmltools::tags$p(text_or(tr$summary_en, "(no summary)"), style = "white-space:pre-wrap;")
          )
        }
      )
    ),
    if (has(url)) htmltools::tags$a(href = url, target = "_blank", "Open source page \u2197")
  )
}

curation_app_validate_logout_path <- function(logout_path) {
  if (is.null(logout_path)) {
    return(NULL)
  }
  if (!is.character(logout_path) || length(logout_path) != 1L ||
    is.na(logout_path) || !grepl("^/[A-Za-z0-9._/-]+$", logout_path)) {
    curation_abort(
      "{.arg logout_path} must be an absolute local path containing only URL path characters.",
      class = "planscanR_error_curation_config"
    )
  }
  logout_path
}

curation_app_acquisition_service <- function(service) {
  if (is.null(service)) {
    return(NULL)
  }
  if (inherits(service, "planscanR_acquisition_catalog")) {
    return(list(catalog = service, available = TRUE))
  }
  if (!is.list(service)) {
    return(list(available = FALSE, reason = "The acquisition service is unavailable."))
  }
  available <- if (is.null(service$available)) {
    inherits(service$catalog, "planscanR_acquisition_catalog")
  } else {
    isTRUE(service$available)
  }
  if (available && !inherits(service$catalog, "planscanR_acquisition_catalog")) {
    available <- FALSE
  }
  service$available <- available
  service$reason <- as.character(service$reason %||% "The acquisition catalog is unavailable.")[[1L]]
  service
}

curation_app_acquisition_nav <- function(acquisition) {
  if (is.null(acquisition)) {
    return(NULL)
  }
  if (isTRUE(acquisition$available)) {
    return(bslib::nav_panel(
      "Acquisition",
      acquisition_dashboard_ui(
        "acquisition",
        preview = acquisition$preview %||% if (isTRUE(acquisition$available)) list(dynamic = TRUE) else NULL
      )
    ))
  }
  bslib::nav_panel(
    "Acquisition",
    shiny::tags$div(
      id = "acquisition-unavailable",
      shiny::h3("Acquisition unavailable"),
      shiny::p(acquisition$reason %||% "The acquisition catalog is unavailable.")
    )
  )
}

curation_app_ui <- function(config, logout_path = NULL, acquisition = NULL) {
  curation_app_require_ui()
  logout_path <- curation_app_validate_logout_path(logout_path)
  acquisition <- curation_app_acquisition_service(acquisition)
  selection_defaults <- config$selection_controls
  default_value <- function(name, fallback) {
    value <- selection_defaults[[name]]
    if (is.null(value) || length(value) != 1L || is.na(value)) fallback else value
  }
  default_labels <- as.character(selection_defaults$nonrenewable %||% character(0))
  learner_choices <- tryCatch(
    curation_selection_learner_choices(),
    error = function(e) c("Logistic regression" = "logistic")
  )
  if (length(learner_choices) == 0L) {
    learner_choices <- c("Logistic regression" = "logistic")
  }
  bslib <- asNamespace("bslib")
  shiny <- asNamespace("shiny")
  bslib$page_navbar(
    title = config$branding$title,
    id = "nav",
    theme = bslib$bs_theme(version = 5, primary = config$branding$primary),
    header = htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(
        ".curation-decision{border:1px solid #ccc;background:#fff;border-radius:4px;",
        "font-size:11px;padding:2px 7px;cursor:pointer;line-height:1.5;}",
        ".curation-decision.active{background:var(--curation-color);color:#fff;}",
        ".curation-decision-group{display:flex;gap:3px;}",
        ".curation-stat{font-variant-numeric:tabular-nums;font-size:1.35rem;font-weight:600;}",
        ".curation-comparison{width:100%;border-collapse:collapse;}",
        ".curation-comparison td,.curation-comparison th{padding:7px;border-bottom:1px solid #eee;}"
      )),
      htmltools::tags$script(htmltools::HTML(
        "window.__curationReviewState = window.__curationReviewState || {};",
        "window.__curationReviewRevision = window.__curationReviewRevision || {};",
        "window.__curationDisplayedRows = window.__curationDisplayedRows || {};",
        "window.__curationDisplayedPending = window.__curationDisplayedPending || {};",
        "window.curationReviewSet = function(id, country, decision, source, reviewer, revision) {",
        "  var key = String(country) + '::' + String(id) + '::' + String(reviewer || window.__curationReviewerId || '');",
        "  var current = window.__curationReviewState[key];",
        "  var next = current === decision ? null : decision;",
        "  var displayedRevision = Number(revision);",
        "  if (revision === null || revision === undefined || !isFinite(displayedRevision) || displayedRevision < 0) return;",
        "  window.__curationReviewState[key] = next;",
        "  if (window.Shiny) window.Shiny.setInputValue('row_decision', {id:id, country:country, decision:next, source:source, revision:displayedRevision, nonce:Date.now() + Math.random()}, {priority:'event'});",
        "};",
        "window.__curationCaptureBulkRevision = function(tableId) {",
        "  if (!window.Shiny || !window.Reactable || !window.Reactable.getState) return;",
        "  var container = document.getElementById(tableId);",
        "  var state = window.Reactable.getState(tableId) || {};",
        "  var displayed = window.__curationDisplayedRows[tableId] || {};",
        "  if (!container || !displayed.render_id) return;",
        "  var rows = [];",
        "  var seen = {};",
        "  (state.selected || []).forEach(function(index) {",
        "    var row = displayed.rows && displayed.rows[String(index)];",
        "    if (!row) return;",
        "    var key = String(row.country) + '::' + String(row.document_id);",
        "    if (seen[key]) return;",
        "    seen[key] = true;",
        "    rows.push({country:row.country, document_id:row.document_id, revision:Number(row.revision)});",
        "  });",
        "  window.Shiny.setInputValue('review_bulk_display', {table_id:tableId, render_id:displayed.render_id, rows:rows, nonce:Date.now() + Math.random()}, {priority:'event'});",
        "};",
        "window.__curationCommitDisplayed = function(tableId) {",
        "  var pending = window.__curationDisplayedPending[tableId];",
        "  var container = document.getElementById(tableId);",
        "  if (!pending || !container) return;",
        "  var groups = container.querySelectorAll('.curation-decision-group');",
        "  if (groups.length === 0 && (pending.rows || []).length > 0) return;",
        "  var rows = {};",
        "  var expected = {};",
        "  (pending.rows || []).forEach(function(row) {",
        "    if (!row) return;",
        "    rows[String(row.index)] = row;",
        "    expected[String(row.country) + '::' + String(row.document_id)] = String(row.revision);",
        "  });",
        "  var matched = 0;",
        "  var seen = {};",
        "  var valid = true;",
        "  Array.prototype.forEach.call(groups, function(group) {",
        "    if (String(group.getAttribute('data-render-id')) !== String(pending.render_id)) { valid = false; return; }",
        "    var key = String(group.getAttribute('data-country')) + '::' + String(group.getAttribute('data-record-id'));",
        "    if (seen[key] || expected[key] === undefined || expected[key] !== String(group.getAttribute('data-revision'))) { valid = false; return; }",
        "    seen[key] = true; matched += 1;",
        "  });",
        "  if (valid && matched === groups.length) window.__curationDisplayedRows[tableId] = {render_id:pending.render_id, rows:rows};",
        "};",
        "window.__curationObserveDisplayed = function(tableId) {",
        "  var container = document.getElementById(tableId);",
        "  if (!container || container.__curationObserved) return;",
        "  container.__curationObserved = true;",
        "  new MutationObserver(function() { window.__curationCommitDisplayed(tableId); }).observe(container, {childList:true, subtree:true});",
        "  setTimeout(function() { window.__curationCommitDisplayed(tableId); }, 100);",
        "};",
        "window.__curationRefreshDecisionButtons = function() {",
        "  document.querySelectorAll('[data-review-id]').forEach(function(group) {",
        "    var value = window.__curationReviewState[group.getAttribute('data-review-id')];",
        "    group.querySelectorAll('button[data-decision]').forEach(function(button) {",
        "      button.classList.toggle('active', button.getAttribute('data-decision') === value);",
        "    });",
        "  });",
        "};",
        "if (window.Shiny) {",
        "  window.Shiny.addCustomMessageHandler('curationSetReviewer', function(message) { window.__curationReviewerId = message.id || ''; });",
        "  window.Shiny.addCustomMessageHandler('curationBulkDecision', function(message) {",
        "    (message.keys || []).forEach(function(key) { window.__curationReviewState[key] = message.decision || null; });",
        "    (message.updates || []).forEach(function(update) {",
        "      if (!update || !update.key) return;",
        "      window.__curationReviewState[update.key] = update.decision == null ? null : String(update.decision);",
        "      if (update.revision != null) window.__curationReviewRevision[update.key] = Number(update.revision) || 0;",
        "    });",
        "    window.__curationRefreshDecisionButtons();",
        "  });",
        "  window.Shiny.addCustomMessageHandler('curationReviewDisplayed', function(message) {",
        "    window.__curationDisplayedPending[message.table_id] = {render_id:message.render_id, rows:message.rows || []};",
        "    window.__curationObserveDisplayed(message.table_id);",
        "    setTimeout(function() { window.__curationCommitDisplayed(message.table_id); }, 100);",
        "    setTimeout(function() { window.__curationObserveDisplayed(message.table_id); window.__curationCommitDisplayed(message.table_id); }, 300);",
        "  });",
        "  window.Shiny.addCustomMessageHandler('curationReviewRefresh', function(message) {",
        "    (message.updates || []).forEach(function(update) {",
        "      if (!update || !update.key) return;",
        "      window.__curationReviewState[update.key] = update.decision == null ? null : String(update.decision);",
        "      if (update.revision != null) window.__curationReviewRevision[update.key] = Number(update.revision) || 0;",
        "    });",
        "    window.__curationRefreshDecisionButtons();",
        "  });",
        "}",
        "document.addEventListener('click', function(event) {",
        "  var action = event.target && event.target.closest ? event.target.closest('button') : null;",
        "  if (!action || !window.Shiny) return;",
        "  var decision = {sr_keep:'keep', sr_drop:'drop', sr_unsure:'unsure'}[action.id];",
        "  if (!decision) return;",
        "  var countryNode = document.getElementById('sr_displayed_country');",
        "  var documentNode = document.getElementById('sr_displayed_document_id');",
        "  var reviewerNode = document.getElementById('sr_displayed_reviewer');",
        "  var revisionNode = document.getElementById('sr_displayed_revision');",
        "  var country = countryNode && countryNode.value;",
        "  var documentId = documentNode && documentNode.value;",
        "  var reviewer = reviewerNode && reviewerNode.value;",
        "  var revision = revisionNode && Number(revisionNode.value);",
        "  event.preventDefault();",
        "  event.stopImmediatePropagation();",
        "  if (!country || !documentId || !reviewer || !isFinite(revision) || revision < 0) return;",
        "  window.Shiny.setInputValue('sr_decision', {country:country, document_id:documentId, reviewer:reviewer, decision:decision, revision:revision, nonce:Date.now() + Math.random()}, {priority:'event'});",
        "}, true);",
        "document.addEventListener('click', function(event) {",
        "  var action = event.target && event.target.closest ? event.target.closest('button') : null;",
        "  if (action && window.__curationCaptureBulkRevision) {",
        "    if (['mark_keep','mark_drop','mark_unsure','mark_clear'].indexOf(action.id) >= 0) window.__curationCaptureBulkRevision('review_tbl');",
        "    if (['rnd_keep','rnd_drop','rnd_unsure','rnd_clear'].indexOf(action.id) >= 0) window.__curationCaptureBulkRevision('random_tbl');",
        "  }",
        "  var button = event.target && event.target.closest ? event.target.closest('button.curation-decision') : null;",
        "  if (!button) return;",
        "  var group = button.closest('.curation-decision-group');",
        "  if (!group) return;",
        "  event.stopPropagation();",
        "  if (group.getAttribute('data-admin') === 'true') {",
        "    var adminRevision = Number(group.getAttribute('data-revision'));",
        "    if (!isFinite(adminRevision) || adminRevision < 0 || !window.Shiny) return;",
        "    window.Shiny.setInputValue('admin_row_decision', {country:group.getAttribute('data-country'), document_id:group.getAttribute('data-record-id'), reviewer:group.getAttribute('data-reviewer'), decision:button.getAttribute('data-decision'), revision:adminRevision, nonce:Date.now() + Math.random()}, {priority:'event'});",
        "    return;",
        "  }",
        "  window.curationReviewSet(group.getAttribute('data-record-id'), group.getAttribute('data-country'), button.getAttribute('data-decision'), group.getAttribute('data-source'), group.getAttribute('data-reviewer'), group.getAttribute('data-revision'));",
        "}, true);",
        "document.addEventListener('keydown', function(event) {",
        "  var tag = (event.target && event.target.tagName || '').toLowerCase();",
        "  if (tag === 'input' || tag === 'textarea' || tag === 'select' || event.altKey || event.ctrlKey || event.metaKey) return;",
        "  if (!window.Shiny || !document.querySelector('input[name=rnd_view][value=single]:checked')) return;",
        "  if (event.key === 'ArrowRight') { event.preventDefault(); window.Shiny.setInputValue('sr_next', Date.now() + Math.random(), {priority:'event'}); }",
        "  if (event.key === 'ArrowLeft') { event.preventDefault(); window.Shiny.setInputValue('sr_prev', Date.now() + Math.random(), {priority:'event'}); }",
        "});"
      ))
    ),
    sidebar = bslib$sidebar(
      width = 300,
      shiny$uiOutput("actor_info"),
      if (!is.null(logout_path)) {
        htmltools::tags$form(
          action = logout_path,
          method = "post",
          style = "margin:0 0 12px;",
          htmltools::tags$button(
            type = "submit",
            class = "btn btn-outline-secondary btn-sm",
            "Log out"
          )
        )
      },
      shiny$selectInput("countries", "Countries", choices = character(0), multiple = TRUE),
      shiny$uiOutput("country_labels_summary"),
      shiny$actionButton("show_label_counts", "Label counts by country", class = "btn-outline-secondary btn-sm"),
      shiny$conditionalPanel(
        condition = "input.nav == 'Overview'",
        bslib$accordion(
          open = FALSE,
          bslib$accordion_panel(
            "Heuristic knobs",
            shiny$sliderInput("threshold", "Cosine threshold", 0, 1, as.numeric(default_value("relevance_threshold", 0.5)), 0.01),
            shiny$sliderInput("kw_min", "Min keyword hits", 0, 6, as.numeric(default_value("kw_min", 2)), 1),
            shiny$checkboxInput("use_classifier", "Use classifier arm", isTRUE(default_value("use_classifier", TRUE))),
            shiny$checkboxInput("use_trim", "Trim confident non-renewables", isTRUE(default_value("use_trim", TRUE))),
            shiny$conditionalPanel(
              "input.use_trim == true",
              shiny$selectizeInput("nonrenewable", "Excluded labels", choices = names(config$labels), selected = default_labels, multiple = TRUE),
              shiny$sliderInput("nonrenewable_score", "Excluded confidence", 0, 1, as.numeric(default_value("nonrenewable_score", 0.5)), 0.01)
            )
          )
        )
      ),
      htmltools::tags$hr(),
      shiny$selectInput("learner", "Selection model", choices = learner_choices, selected = unname(learner_choices[[1L]])),
      shiny$actionButton("train_model", "Train selection model", class = "btn-outline-primary btn-sm"),
      shiny$uiOutput("model_info"),
      shiny$sliderInput("model_threshold", "Model decision threshold", 0, 1, 0.5, 0.01),
      shiny$actionButton("compute_lc", "Compute learning curve", class = "btn-primary btn-sm"),
      plotly::plotlyOutput("learning_curve_plot", height = "260px"),
      htmltools::tags$hr(),
      shiny$actionButton("rebuild", "Rebuild snapshot", class = "btn-outline-secondary btn-sm"),
      shiny$uiOutput("snapshot_info")
    ),
    bslib$nav_panel(
      "Overview",
      bslib$layout_columns(
        bslib$card(htmltools::div(class = "curation-stat", shiny$textOutput("vb_total", inline = TRUE)), bslib$card_header("Indexed")),
        bslib$card(htmltools::div(class = "curation-stat", shiny$textOutput("vb_selected", inline = TRUE)), bslib$card_header("Selected (heuristic)")),
        bslib$card(htmltools::div(class = "curation-stat", shiny$textOutput("vb_selected_model", inline = TRUE)), bslib$card_header("Selected (model)")),
        bslib$card(htmltools::div(class = "curation-stat", shiny$textOutput("vb_reviewed", inline = TRUE)), bslib$card_header("Reviewed")),
        col_widths = c(3, 3, 3, 3)
      ),
      bslib$layout_columns(
        bslib$card(bslib$card_header("Pipeline funnel"), plotly::plotlyOutput("funnel_plot", height = "360px")),
        bslib$card(bslib$card_header("Per-country breakdown"), reactable::reactableOutput("funnel_table")),
        col_widths = c(7, 5)
      ),
      bslib$card(
        bslib$card_header("Historical imported labels"),
        shiny$helpText("Historical labels retain source and linkage attribution. Unresolved rows are read-only."),
        reactable::reactableOutput("historical_overview_tbl")
      ),
      bslib$card(
        bslib$card_header(
          htmltools::div(
            class = "d-flex justify-content-between align-items-center",
            htmltools::span("Automated selection vs. human review"),
            shiny$actionButton("show_perf_country", "Performance by country", class = "btn-outline-secondary btn-sm")
          )
        ),
        shiny$radioButtons("eval_on", NULL, inline = TRUE, choices = c("All reviews" = "all", "Random sample only (unbiased)" = "random"), selected = "all"),
        shiny$uiOutput("agreement_ui")
      )
    ),
    bslib$nav_panel(
      "Review",
      bslib$card(
        bslib$card_header("Triage"),
        htmltools::div(
          class = "d-flex align-items-end flex-wrap gap-2",
          shiny$selectInput(
            "stage_filter", "Show",
            choices = c("All" = "all", "Heuristic-selected" = "selected", "Heuristic not selected" = "not_selected", "Model-selected" = "model_selected", "Model not selected" = "model_not_selected", "Heuristic vs Model differ" = "model_vs_heuristic", "Reviewed" = "reviewed", "Unreviewed" = "unreviewed", "Disagreements (auto vs human)" = "disagree"),
            selected = "model_selected", width = "300px"
          ),
          shiny$actionButton("mark_keep", "Keep", class = "btn-success btn-sm"),
          shiny$actionButton("mark_drop", "Drop", class = "btn-danger btn-sm"),
          shiny$actionButton("mark_unsure", "Unsure", class = "btn-warning btn-sm"),
          shiny$actionButton("mark_clear", "Clear", class = "btn-outline-secondary btn-sm"),
          shiny$textOutput("review_info", inline = TRUE)
        ),
        shiny$helpText("Select rows for bulk actions. Click a row to expand the original and optional translated text."),
        reactable::reactableOutput("review_tbl"),
        min_height = "520px",
        class = "curation-review-card"
      ),
      bslib$card(
        bslib$card_header("Historical imported labels"),
        shiny$helpText("Historical labels retain source and linkage attribution. Unresolved rows are read-only."),
        reactable::reactableOutput("historical_review_tbl")
      ),
      shiny$uiOutput("admin_review_ui")
    ),
    bslib$nav_panel(
      "Random review",
      bslib$card(
        bslib$card_header("Random sample \u2014 unbiased review"),
        htmltools::div(
          class = "d-flex align-items-end flex-wrap gap-3",
          shiny$numericInput("rnd_n", "Records / country", 50, min = 1, step = 10),
          shiny$numericInput("rnd_seed", "Seed (optional)", NA, min = 0),
          shiny$actionButton("rnd_draw", "Build queue", class = "btn-primary"),
          shiny$checkboxInput("rnd_blind", "Blind (hide pipeline signals)", TRUE),
          shiny$radioButtons("rnd_view", NULL, choices = c("One at a time" = "single", "Table" = "table"), selected = "single", inline = TRUE),
          shiny$textOutput("rnd_info", inline = TRUE),
          shiny$uiOutput("warm_status")
        ),
        shiny$helpText("Other reviewers' records are queued first; fresh records are then sampled by country. Bulk and stepper decisions retain random provenance."),
        shiny$conditionalPanel(
          "input.rnd_view == 'table'",
          htmltools::div(
            shiny$actionButton("rnd_keep", "Keep", class = "btn-success btn-sm"),
            shiny$actionButton("rnd_drop", "Drop", class = "btn-danger btn-sm"),
            shiny$actionButton("rnd_unsure", "Unsure", class = "btn-warning btn-sm"),
            shiny$actionButton("rnd_clear", "Clear", class = "btn-outline-secondary btn-sm")
          ),
          reactable::reactableOutput("random_tbl")
        ),
        shiny$conditionalPanel(
          "input.rnd_view == 'single'",
          htmltools::div(
            shiny$actionButton("sr_prev", "\u2190 Previous", class = "btn-outline-secondary btn-sm"),
            shiny$actionButton("sr_next", "Next \u2192", class = "btn-outline-secondary btn-sm"),
            shiny$checkboxInput("sr_skip", "Skip already-classified", TRUE),
            shiny$textOutput("sr_pos", inline = TRUE)
          ),
          shiny$uiOutput("sr_record"),
          shiny$uiOutput("sr_decision")
        )
      )
    ),
    curation_app_acquisition_nav(acquisition)
  )
}

curation_app_read_reviews <- function(repository, scope, actor) {
  rows <- curation_review_list(
    repository,
    scope,
    filter = list(),
    actor = actor
  )
  curation_app_normalize_reviews(rows)
}

curation_app_model_snapshot_identity <- function(provenance, model) {
  input <- if (is.list(provenance)) provenance$input %||% list() else list()
  training <- if (is.list(model)) model$training_provenance %||% list() else list()
  training_input <- if (is.list(training)) training$input %||% list() else list()
  ids <- c(
    input$snapshot_id,
    input$version,
    if (is.list(provenance)) provenance$corpus_snapshot$snapshot_id else NULL,
    training_input$snapshot_id,
    training_input$version
  )
  versions <- c(
    input$snapshot_version,
    input$version,
    if (is.list(provenance)) provenance$snapshot_version else NULL,
    if (is.list(provenance)) provenance$corpus_snapshot$version else NULL,
    training_input$snapshot_version,
    training_input$version
  )
  fingerprint <- c(
    input$fingerprint,
    input$record_fingerprint,
    if (is.list(provenance)) provenance$record_fingerprint else NULL,
    training_input$fingerprint,
    training_input$record_fingerprint
  )
  first <- function(values) {
    values <- values[!vapply(values, is.null, logical(1))]
    if (length(values) == 0L) NULL else as.character(values[[1L]])
  }
  list(
    id = first(ids),
    version = first(versions),
    fingerprint = first(fingerprint)
  )
}

curation_app_model_snapshot_id <- function(provenance, model) {
  curation_app_model_snapshot_identity(provenance, model)$id
}

curation_app_read_model_reference <- function(store, scope, actor, config = NULL) {
  active <- tryCatch(
    curation_artifact_active_model(store, scope, actor),
    error = function(e) NULL
  )
  if (is.null(active)) {
    return(NULL)
  }
  active_provenance <- if (is.list(active)) active$provenance %||% list() else list()
  active_model_id <- if (is.list(active)) {
    active$active_model_id %||% active$model_id %||% NULL
  } else {
    NULL
  }
  if (inherits(active, "planscanR_selection_model")) {
    identity <- curation_app_model_snapshot_identity(active_provenance, active)
    return(list(
      model = active,
      model_id = active_model_id,
      input_snapshot_id = identity$id,
      input_snapshot_version = identity$version,
      input_snapshot_fingerprint = identity$fingerprint,
      artifact_path = NULL,
      provenance = active_provenance,
      config_id = scope$config_id,
      created_by = NULL
    ))
  }
  payload <- if (is.list(active) && "model" %in% names(active)) {
    active[["model"]]
  } else if (is.list(active) && "payload" %in% names(active)) {
    active[["payload"]]
  } else {
    NULL
  }
  if (inherits(payload, "planscanR_selection_model")) {
    identity <- curation_app_model_snapshot_identity(active_provenance, payload)
    return(list(
      model = payload,
      model_id = active_model_id,
      input_snapshot_id = identity$id,
      input_snapshot_version = identity$version,
      input_snapshot_fingerprint = identity$fingerprint,
      artifact_path = NULL,
      provenance = active_provenance,
      config_id = scope$config_id,
      created_by = NULL
    ))
  }
  if (is.null(active_model_id)) {
    return(NULL)
  }
  loaded <- tryCatch(
    curation_artifact_read_model(store, scope, active_model_id, actor),
    error = function(e) NULL
  )
  if (is.null(loaded)) {
    return(NULL)
  }
  loaded_payload <- if (is.list(loaded) && "model" %in% names(loaded)) {
    loaded[["model"]]
  } else if (is.list(loaded) && "payload" %in% names(loaded)) {
    loaded[["payload"]]
  } else {
    NULL
  }
  if (!inherits(loaded_payload, "planscanR_selection_model")) {
    return(NULL)
  }
  loaded_provenance <- if (is.list(loaded)) loaded$provenance %||% active_provenance else active_provenance
  identity <- curation_app_model_snapshot_identity(loaded_provenance, loaded_payload)
  list(
    model = loaded_payload,
    model_id = as.character(active_model_id),
    input_snapshot_id = identity$id,
    input_snapshot_version = identity$version,
    input_snapshot_fingerprint = identity$fingerprint,
    artifact_path = if (is.list(loaded)) loaded$path %||% NULL else NULL,
    provenance = loaded_provenance,
    config_id = scope$config_id,
    created_by = if (is.list(loaded_provenance)) loaded_provenance$creator %||% NULL else NULL
  )
}

curation_app_read_model <- function(store, scope, actor, config) {
  reference <- curation_app_read_model_reference(store, scope, actor, config)
  if (is.null(reference)) NULL else reference$model
}

curation_app_read_sample <- function(store, scope, actor) {
  listed <- tryCatch(
    curation_artifact_list_samples(store, scope, reviewer_id = actor$id, actor = actor),
    error = function(e) NULL
  )
  if (is.null(listed) || !is.data.frame(listed) || nrow(listed) == 0L) {
    return(list(id = NULL, revision = 0L, queue = tibble::tibble(document_id = character(0), country = character(0))))
  }
  id_column <- intersect(c("sample_id", "id"), names(listed))
  if (length(id_column) == 0L) {
    return(list(id = NULL, revision = 0L, queue = tibble::tibble(document_id = character(0), country = character(0))))
  }
  # Samples are immutable.  The newest row is the session's restart point;
  # choosing the first row would silently restore an older queue after a
  # reviewer drew a second sample.
  latest <- nrow(listed)
  id <- as.character(listed[[id_column[[1L]]]][[latest]])
  revision <- if ("revision" %in% names(listed)) as.integer(listed$revision[[latest]]) else 0L
  read <- tryCatch(curation_artifact_read_sample(store, scope, id, actor), error = function(e) NULL)
  payload <- if (is.null(read)) NULL else read$payload %||% read$sample %||% read
  queue <- if (is.list(payload) && is.data.frame(payload$queue)) payload$queue else if (is.data.frame(payload)) payload else NULL
  if (is.null(queue) || !all(c("country", "document_id") %in% names(queue))) {
    queue <- tibble::tibble(document_id = character(0), country = character(0))
  }
  list(id = id, revision = revision, queue = queue)
}

curation_app_read_snapshot <- function(store, scope, actor, version = NULL) {
  snapshot <- tryCatch(
    curation_artifact_read_snapshot(store, scope, actor, version = version),
    error = function(e) NULL
  )
  if (is.null(snapshot)) {
    return(list(records = NULL, id = NULL, version = NULL, revision = 0L))
  }
  payload <- snapshot$payload %||% snapshot$snapshot %||% snapshot
  payload_is_list <- is.list(payload) && !is.data.frame(payload)
  records <- if (payload_is_list && is.data.frame(payload$records)) payload$records else if (is.data.frame(payload)) payload else NULL
  list(
    records = records,
    id = snapshot$snapshot_id %||% snapshot$id %||% if (payload_is_list) payload$snapshot_id else NULL,
    version = snapshot$version %||% if (payload_is_list) payload$version else NULL,
    revision = as.integer(snapshot$revision %||% if (payload_is_list) payload$revision else 0L),
    metadata = snapshot$metadata %||% if (payload_is_list) {
      payload$metadata
    } else {
      {
        attr(payload, "curation_snapshot_metadata", exact = TRUE)
      } %||% list()
    }
  )
}

curation_app_snapshot_config_matches <- function(snapshot_info, config) {
  metadata <- snapshot_info$metadata
  if (!is.list(metadata) || is.null(metadata$config_fingerprint)) {
    return(FALSE)
  }
  expected <- tryCatch(curation_config_fingerprint(config), error = function(e) NULL)
  identical(as.character(metadata$config_fingerprint), as.character(expected))
}

curation_app_snapshot_compatible <- function(snapshot_info, scope, config) {
  if (!is.list(snapshot_info) || !is.data.frame(snapshot_info$records)) {
    return(FALSE)
  }
  metadata <- snapshot_info$metadata
  if (!is.list(metadata)) {
    return(FALSE)
  }
  schema_version <- metadata$schema_version
  if (!is.character(schema_version) || length(schema_version) != 1L ||
    is.na(schema_version) || !identical(schema_version, "curation-snapshot.v1")) {
    return(FALSE)
  }
  if (!is.character(metadata$enrichment_schema) ||
    length(metadata$enrichment_schema) != 1L ||
    is.na(metadata$enrichment_schema) ||
    !identical(metadata$enrichment_schema, curation_artifact_enrichment_schema)) {
    return(FALSE)
  }
  valid_records <- tryCatch(
    {
      curation_validate_record_frame(snapshot_info$records)
      TRUE
    },
    error = function(error) FALSE
  )
  if (!isTRUE(valid_records) || !curation_app_snapshot_config_matches(snapshot_info, config)) {
    return(FALSE)
  }
  expected <- tryCatch(
    curation_snapshot_version(snapshot_info$records, scope, config = config),
    error = function(error) NULL
  )
  is.character(snapshot_info$version) && length(snapshot_info$version) == 1L &&
    !is.na(snapshot_info$version) && !is.null(expected) &&
    identical(as.character(snapshot_info$version), as.character(expected))
}

curation_app_model_provenance <- function(
  scope,
  config,
  records,
  reviews,
  actor,
  snapshot_version = NULL,
  snapshot_id = NULL,
  learner_key,
  eval_source,
  threshold,
  input_provenance = NULL,
  cohort_receipt = NULL,
  training_audit = NULL
) {
  curation_check_scope(scope, class = "planscanR_error_curation_artifact")
  curation_check_config(config)
  curation_check_actor(actor)
  version <- snapshot_version %||% curation_snapshot_version(records, scope, config = config)
  if (!is.character(version) || length(version) != 1L || is.na(version) || !nzchar(version)) {
    curation_abort(
      "Model publication needs a content-derived corpus snapshot version.",
      class = "planscanR_error_curation_artifact"
    )
  }
  input <- utils::modifyList(
    list(
      snapshot_id = snapshot_id,
      snapshot_version = version,
      fingerprint = curation_artifact_content_fingerprint(records)
    ),
    input_provenance %||% list()
  )
  list(
    review_fingerprint = curation_artifact_content_fingerprint(reviews),
    snapshot_version = version,
    config_fingerprint = curation_config_fingerprint(config),
    corpus_snapshot = list(
      corpus_id = scope$corpus_id,
      version = version,
      snapshot_id = snapshot_id
    ),
    review = list(
      fingerprint = curation_artifact_content_fingerprint(reviews),
      rows = nrow(reviews)
    ),
    config = list(
      project_id = scope$project_id,
      corpus_id = scope$corpus_id,
      config_id = scope$config_id,
      fingerprint = curation_config_fingerprint(config)
    ),
    input = input,
    cohort_receipt = cohort_receipt %||% list(),
    training_audit = training_audit %||% list(),
    training_parameters = list(
      learner = as.character(learner_key %||% "default"),
      eval_source = eval_source,
      threshold = as.numeric(threshold),
      v = 5L,
      repeats = 1L,
      seed = NULL
    ),
    creator = actor$id
  )
}

curation_app_training_input_provenance <- function(
  base = NULL,
  records,
  snapshot_id = NULL,
  snapshot_version = NULL
) {
  if (!is.data.frame(records)) {
    curation_abort(
      "Model training input provenance needs a scored record data frame.",
      class = "planscanR_error_curation_artifact"
    )
  }
  result <- base %||% list()
  if (!is.list(result)) {
    curation_abort(
      "Model input provenance must be a named list.",
      class = "planscanR_error_curation_artifact"
    )
  }
  if (!is.null(snapshot_id) && is.null(result$snapshot_id)) result$snapshot_id <- as.character(snapshot_id)
  if (!is.null(snapshot_version) && is.null(result$snapshot_version)) result$snapshot_version <- as.character(snapshot_version)
  result$rows <- nrow(records)
  result$countries <- sort(unique(as.character(records$country)))
  result$fingerprint <- curation_artifact_content_fingerprint(records)
  if (!is.null(result$feature_names)) {
    result$prepared_feature_fingerprint <- curation_scored_feature_fingerprint(
      records,
      result$feature_names
    )
  }
  result
}

curation_app_training_input_matches <- function(records, provenance = NULL, metadata = NULL) {
  expected <- if (is.list(provenance)) provenance$record_fingerprint else NULL
  if (is.null(expected)) {
    return(TRUE)
  }
  if (!is.character(expected) || length(expected) != 1L || is.na(expected) || !nzchar(expected)) {
    return(FALSE)
  }
  expected_features <- if (is.list(provenance)) provenance$scored_feature_fingerprint else NULL
  if (isTRUE(provenance$preserve_scored_features) &&
    (is.null(expected_features) || is.null(provenance$feature_names))) {
    return(FALSE)
  }
  if (!is.null(expected_features)) {
    actual_features <- curation_scored_feature_fingerprint(
      records,
      provenance$feature_names
    )
    if (!identical(as.character(expected_features), as.character(actual_features))) {
      return(FALSE)
    }
    metadata_features <- if (is.list(metadata)) metadata$scored_feature_fingerprint else NULL
    if (!is.null(metadata_features) &&
      !identical(as.character(metadata_features), as.character(actual_features))) {
      return(FALSE)
    }
  }
  actual <- curation_artifact_content_fingerprint(records)
  metadata_fingerprint <- if (is.list(metadata)) metadata$corpus_fingerprint else NULL
  if (is.null(metadata_fingerprint)) {
    record_metadata <- attr(records, "curation_snapshot_metadata", exact = TRUE)
    metadata_fingerprint <- if (is.list(record_metadata)) record_metadata$corpus_fingerprint else NULL
  }
  identical(as.character(expected), as.character(actual)) ||
    identical(as.character(expected), as.character(metadata_fingerprint))
}

curation_app_sample_nonce <- local({
  sequence <- 0L
  function() {
    sequence <<- sequence + 1L
    paste(Sys.getpid(), sequence, sep = "-")
  }
})

curation_app_new_sample_id <- function(scope, reviewer_id, queue, seed = NULL) {
  paste0(
    "sample-",
    curation_artifact_hash(list(
      scope = scope,
      reviewer_id = reviewer_id,
      queue = queue,
      seed = seed,
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC"),
      nonce = curation_app_sample_nonce()
    ))
  )
}

curation_app_funnel <- function(records, config, context = list(), by_country = FALSE) {
  one <- function(data) {
    result <- curation_compute_funnel(config, data, context = context)
    result$country <- if (nrow(data) == 0L) "all" else as.character(data$country[[1L]])
    result
  }
  if (!isTRUE(by_country) || nrow(records) == 0L) {
    result <- curation_compute_funnel(config, records, context = context)
    result$country <- "all"
    return(result)
  }
  dplyr::bind_rows(lapply(split(records, records$country), one))
}

curation_app_funnel_plot <- function(funnel) {
  if (is.null(funnel) || nrow(funnel) == 0L) {
    return(plotly::plot_ly(type = "bar") |> plotly::layout(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(text = "No data", showarrow = FALSE, x = 0.5, y = 0.5, xref = "paper", yref = "paper")
    ) |> plotly::config(displayModeBar = FALSE))
  }
  d <- funnel[funnel$country == "all", , drop = FALSE]
  if (nrow(d) == 0L) d <- funnel
  d$stage <- factor(d$stage, levels = rev(unique(d$stage[order(d$order)])))
  d$label <- sprintf("%s (%.1f%%)", format(d$n, big.mark = ",", trim = TRUE), d$pct)
  d$hover <- sprintf("<b>%s</b><br>Count: %s<br>Percent: %.1f%%", d$stage, format(d$n, big.mark = ",", trim = TRUE), d$pct)
  plotly::plot_ly(
    d,
    x = ~n, y = ~stage, type = "bar", orientation = "h",
    marker = list(color = "#0e3c62"), text = ~label, textposition = "outside",
    cliponaxis = FALSE, hovertext = ~hover, hoverinfo = "text"
  ) |>
    plotly::layout(
      xaxis = list(title = "records", zeroline = FALSE),
      yaxis = list(title = "", automargin = TRUE),
      margin = list(l = 10, r = 60, t = 10, b = 10), showlegend = FALSE, bargap = 0.3
    ) |>
    plotly::config(displayModeBar = FALSE)
}

curation_app_learning_plot <- function(curve) {
  if (is.null(curve) || nrow(curve) == 0L) {
    return(plotly::plot_ly(type = "scatter", mode = "lines") |> plotly::layout(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(text = "Click Compute learning curve", showarrow = FALSE, x = 0.5, y = 0.5, xref = "paper", yref = "paper")
    ) |> plotly::config(displayModeBar = FALSE))
  }
  summary <- tryCatch(learning_curve_summary(curve), error = function(e) NULL)
  if (is.null(summary) || nrow(summary) == 0L) {
    return(curation_app_learning_plot(NULL))
  }
  summary <- summary[order(summary$n_train_used), , drop = FALSE]
  has_country <- "country" %in% names(summary)
  all_rows <- if (has_country) summary[summary$country == "all", , drop = FALSE] else summary
  plot <- plotly::plot_ly()
  if (nrow(all_rows) > 0L) {
    sd <- if ("f1_sd" %in% names(all_rows)) ifelse(is.na(all_rows$f1_sd), 0, all_rows$f1_sd) else rep(0, nrow(all_rows))
    plot <- plotly::add_trace(
      plot,
      x = all_rows$n_train_used, y = pmin(1, all_rows$f1_mean + sd),
      type = "scatter", mode = "lines", line = list(width = 0),
      showlegend = FALSE, hoverinfo = "skip"
    )
    plot <- plotly::add_trace(
      plot,
      x = all_rows$n_train_used, y = pmax(0, all_rows$f1_mean - sd),
      type = "scatter", mode = "lines", line = list(width = 0),
      fill = "tonexty", fillcolor = "rgba(14,60,98,0.16)",
      showlegend = FALSE, hoverinfo = "skip"
    )
    plot <- plotly::add_trace(
      plot,
      x = all_rows$n_train_used, y = all_rows$f1_mean,
      type = "scatter", mode = "lines+markers", name = "All",
      line = list(color = "#0e3c62", width = 2.5), marker = list(color = "#0e3c62")
    )
  }
  if (has_country) {
    countries <- setdiff(unique(as.character(summary$country)), "all")
    palette <- grDevices::hcl.colors(max(1L, length(countries)), "Dark 3")
    for (i in seq_along(countries)) {
      d <- summary[summary$country == countries[[i]], , drop = FALSE]
      plot <- plotly::add_trace(
        plot,
        x = d$n_train_used, y = d$f1_mean, type = "scatter",
        mode = "lines+markers", name = countries[[i]],
        line = list(color = palette[[i]], width = 1.2), marker = list(size = 4)
      )
    }
  }
  plotly::layout(
    plot,
    xaxis = list(title = "Number of training labels"),
    yaxis = list(title = "F1 (held-out)", range = c(0, 1)),
    legend = list(orientation = "h")
  ) |>
    plotly::config(displayModeBar = FALSE)
}

curation_app_server <- function(input, output, session, context) {
  config <- context$config
  provider <- context$records_provider
  repository <- context$review_repository
  artifact_store <- context$artifact_store
  historical_links <- context$historical_reviews %||% NULL
  model_input_provenance <- context$input_provenance %||% list()
  cohort_receipt <- context$cohort_receipt %||% list()
  preserve_scored_features <- isTRUE(model_input_provenance$preserve_scored_features)
  curation_app_check_actor_resolver(context$actor_resolver)
  actor <- curation_resolve_actor(
    context$actor_resolver,
    curation_app_request_for_session(context$request, session)
  )
  current_actor <- function() {
    curation_app_resolve_current_actor(context, session, actor)
  }
  scope <- config$identity
  reviewer_id <- actor$id
  session$sendCustomMessage("curationSetReviewer", list(id = reviewer_id))

  acquisition <- curation_app_acquisition_service(
    context$acquisition %||% context$acquisition_catalog
  )

  # A session may outlive an account disablement or role change.  Recheck the
  # resolver periodically and close the socket when its credential is no
  # longer valid; every mutation below also calls current_actor immediately
  # before accessing a repository or artifact store.
  actor_probe <- context$actor_probe
  if (!is.null(actor_probe)) {
    curation_app_check_actor_resolver(actor_probe)
    auth_timer <- shiny::reactiveTimer(60000, session)
    shiny::observeEvent(auth_timer(),
      {
        valid <- tryCatch(
          curation_app_resolve_current_actor(
            context, session, actor,
            resolver = actor_probe
          ),
          error = function(error) FALSE
        )
        if (isFALSE(valid) && is.function(session$close)) {
          session$close()
        }
      },
      ignoreInit = TRUE
    )
  }

  app_state <- context$app_state
  if (!is.environment(app_state)) {
    app_state <- new.env(parent = emptyenv())
  }
  read_records <- function(refresh = FALSE) {
    curation_records_snapshot(
      provider,
      config,
      refresh = refresh,
      preserve_scored_features = preserve_scored_features
    )
  }
  snapshot_info <- if (exists("snapshot", envir = app_state, inherits = FALSE)) {
    app_state$snapshot
  } else {
    curation_app_read_snapshot(artifact_store, scope, actor)
  }
  if (curation_app_snapshot_compatible(snapshot_info, scope, config) &&
    curation_app_training_input_matches(snapshot_info$records, model_input_provenance, snapshot_info$metadata)) {
    initial_records <- snapshot_info$records
    app_state$snapshot <- snapshot_info
  } else {
    if (exists("snapshot", envir = app_state, inherits = FALSE)) {
      rm("snapshot", envir = app_state)
    }
    live_records <- tryCatch(read_records(FALSE), error = identity)
    if (inherits(live_records, "error")) {
      shiny::showNotification(
        paste0("Unable to load records: ", conditionMessage(live_records)),
        type = "error", duration = 8
      )
      initial_records <- curation_app_empty_records()
      snapshot_info <- list(records = NULL, id = NULL, version = NULL, revision = 0L, metadata = list())
    } else if (!curation_app_training_input_matches(live_records, model_input_provenance)) {
      shiny::showNotification(
        "The selected scored input does not match the records provider; training is blocked until the explicit input is selected.",
        type = "error", duration = 10
      )
      initial_records <- curation_app_empty_records()
      snapshot_info <- list(records = NULL, id = NULL, version = NULL, revision = 0L, metadata = list())
    } else {
      initial_records <- live_records
      next_version <- tryCatch(
        curation_snapshot_version(live_records, scope, config = config),
        error = function(error) error
      )
      written <- if (inherits(next_version, "error")) {
        next_version
      } else {
        tryCatch(
          curation_artifact_write_snapshot(
            artifact_store,
            scope,
            snapshot = live_records,
            actor = actor,
            expected_revision = 0L
          ),
          error = identity
        )
      }
      if (inherits(written, "error") ||
        (is.list(written) && "ok" %in% names(written) && isFALSE(written$ok))) {
        message <- if (inherits(written, "error")) {
          conditionMessage(written)
        } else {
          "the artifact store rejected the write"
        }
        shiny::showNotification(
          paste0("Initial snapshot was not persisted: ", message),
          type = "warning", duration = 8
        )
        snapshot_info <- list(records = NULL, id = NULL, version = NULL, revision = 0L, metadata = list())
      } else {
        metadata <- attr(live_records, "curation_snapshot_metadata", exact = TRUE) %||% list()
        metadata$schema_version <- metadata$schema_version %||% "curation-snapshot.v1"
        snapshot_info <- list(
          records = live_records,
          id = written$snapshot_id %||% NULL,
          version = written$version %||% next_version,
          revision = as.integer(written$revision %||% 1L),
          metadata = metadata
        )
        app_state$snapshot <- snapshot_info
      }
    }
  }
  records <- shiny::reactiveVal(initial_records)
  snapshot_id <- shiny::reactiveVal(snapshot_info$id)
  snapshot_revision <- shiny::reactiveVal(snapshot_info$revision)
  snapshot_version <- shiny::reactiveVal(snapshot_info$version)
  reviews <- shiny::reactiveVal(tryCatch(
    curation_app_read_reviews(repository, scope, actor),
    error = function(e) {
      shiny::showNotification(paste0("Unable to load reviews: ", conditionMessage(e)), type = "error", duration = 8)
      curation_app_empty_reviews()
    }
  ))
  pending_review_keys <- shiny::reactiveVal(character(0))
  review_display_tokens <- new.env(parent = emptyenv())
  # Keep an already-open review view current for other actors' writes.  The
  # non-touching probe is checked independently from the existing socket
  # expiry timer; a revoked session is closed before its repository read.
  if (!is.null(actor_probe)) {
    review_timer <- shiny::reactiveTimer(60000, session)
    shiny::observeEvent(review_timer(),
      {
        refresh_actor <- tryCatch(
          curation_app_resolve_current_actor(
            context, session, actor,
            resolver = actor_probe
          ),
          error = identity
        )
        if (inherits(refresh_actor, "error")) {
          if (is.function(session$close)) session$close()
          return()
        }
        incoming <- tryCatch(
          curation_app_read_reviews(repository, scope, refresh_actor),
          error = identity
        )
        if (inherits(incoming, "error")) {
          shiny::showNotification(
            paste0("Review refresh failed: ", conditionMessage(incoming)),
            type = "warning", duration = 8
          )
          return()
        }
        local <- reviews()
        pending <- pending_review_keys()
        updates <- curation_app_review_refresh_updates(local, incoming, pending)
        merged <- curation_app_merge_reviews_preserving_pending(local, incoming, pending)
        if (!identical(local, merged)) reviews(merged)
        if (length(updates) > 0L) {
          try(
            session$sendCustomMessage("curationReviewRefresh", list(updates = updates)),
            silent = TRUE
          )
        }
      },
      ignoreInit = TRUE
    )
  }
  initial_model_reference <- curation_app_read_model_reference(
    artifact_store, scope, actor, config
  )
  model_reference <- shiny::reactiveVal(initial_model_reference)
  model <- shiny::reactiveVal(
    if (is.null(initial_model_reference)) NULL else initial_model_reference$model
  )
  sample_info <- curation_app_read_sample(artifact_store, scope, actor)
  sample <- shiny::reactiveVal(sample_info$queue)
  sample_id <- shiny::reactiveVal(sample_info$id)
  sample_revision <- shiny::reactiveVal(sample_info$revision)
  sample_index <- shiny::reactiveVal(1L)
  learning_curve <- shiny::reactiveVal(NULL)
  random_df <- shiny::reactive({
    queue <- sample()
    data <- selected_records()
    if (nrow(queue) == 0L || nrow(data) == 0L) {
      return(data[0, , drop = FALSE])
    }
    key <- curation_app_key_string(data$country, data$document_id)
    wanted <- curation_app_key_string(queue$country, queue$document_id)
    result <- data[match(wanted, key), , drop = FALSE]
    result[!is.na(result$document_id), , drop = FALSE]
  })

  output$actor_info <- shiny::renderUI({
    shiny::tagList(
      shiny::tags$div("Authenticated reviewer", style = "font-size:0.75rem;font-weight:600;text-transform:uppercase;color:#666;"),
      shiny::tags$div(actor$display_name %||% actor$id, style = "margin-bottom:6px;"),
      shiny::tags$small(paste0("Reviewer ID: ", actor$id), style = "color:#888;")
    )
  })

  countries <- shiny::reactive({
    sort(unique(as.character(records()$country)))
  })
  shiny::observe({
    cc <- countries()
    shiny::updateSelectInput(session, "countries", choices = cc, selected = cc)
  })
  selected_countries <- shiny::reactive({
    value <- input$countries
    if (is.null(value) || length(value) == 0L) countries() else as.character(value)
  })

  shiny::observeEvent(input$rebuild, {
    mutation_actor <- tryCatch(current_actor(), error = function(e) e)
    if (inherits(mutation_actor, "error")) {
      shiny::showNotification(
        paste0("Snapshot refresh failed: ", conditionMessage(mutation_actor)),
        type = "error", duration = 8
      )
      return()
    }
    fresh <- tryCatch(read_records(TRUE), error = function(e) e)
    if (inherits(fresh, "error")) {
      shiny::showNotification(paste0("Snapshot refresh failed: ", conditionMessage(fresh)), type = "error", duration = 8)
      return()
    }
    if (!curation_app_training_input_matches(fresh, model_input_provenance)) {
      shiny::showNotification(
        "Snapshot refresh returned records outside the explicitly selected scored input; the refresh was not persisted.",
        type = "error", duration = 10
      )
      return()
    }
    next_version <- tryCatch(
      curation_snapshot_version(fresh, scope, config = config),
      error = function(e) e
    )
    if (inherits(next_version, "error")) {
      shiny::showNotification(
        paste0("Snapshot refresh failed: ", conditionMessage(next_version)),
        type = "error", duration = 8
      )
      return()
    }
    if (identical(snapshot_version(), next_version)) {
      records(fresh)
      metadata <- attr(fresh, "curation_snapshot_metadata", exact = TRUE) %||% list()
      metadata$schema_version <- metadata$schema_version %||% "curation-snapshot.v1"
      app_state$snapshot <- list(
        records = fresh,
        id = snapshot_id(),
        version = next_version,
        revision = snapshot_revision(),
        metadata = metadata
      )
      shiny::showNotification("The current snapshot is already persisted.", type = "message")
      return()
    }
    # Another session may already have published this exact immutable version
    # while this session was open.  Reuse it instead of trying to overwrite an
    # existing artifact path.
    existing <- curation_app_read_snapshot(
      artifact_store, scope, mutation_actor,
      version = next_version
    )
    if (curation_app_snapshot_compatible(existing, scope, config)) {
      records(fresh)
      snapshot_id(existing$id)
      snapshot_version(existing$version)
      snapshot_revision(existing$revision)
      app_state$snapshot <- existing
      shiny::showNotification("Snapshot refreshed from the persisted artifact.", type = "message")
      return()
    }
    written <- tryCatch(
      curation_artifact_write_snapshot(
        artifact_store,
        scope,
        snapshot = fresh,
        actor = mutation_actor,
        expected_revision = 0L
      ),
      error = function(e) e
    )
    if (inherits(written, "error") || (is.list(written) && "ok" %in% names(written) && isFALSE(written$ok))) {
      message <- if (inherits(written, "error")) conditionMessage(written) else "the artifact store rejected the write"
      shiny::showNotification(paste0("Snapshot refresh was not persisted: ", message), type = "error", duration = 8)
      return()
    }
    records(fresh)
    snapshot_id(written$snapshot_id %||% snapshot_id())
    snapshot_version(written$version %||% next_version)
    if (!is.null(written$revision)) snapshot_revision(as.integer(written$revision))
    metadata <- attr(fresh, "curation_snapshot_metadata", exact = TRUE) %||% list()
    metadata$schema_version <- metadata$schema_version %||% "curation-snapshot.v1"
    app_state$snapshot <- list(
      records = fresh,
      id = written$snapshot_id %||% snapshot_id(),
      version = written$version %||% next_version,
      revision = as.integer(written$revision %||% snapshot_revision()),
      metadata = metadata
    )
    shiny::showNotification("Snapshot refreshed through the injected artifact store.", type = "message")
  })

  nonrenewable_input_seen <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(input$nonrenewable,
    {
      nonrenewable_input_seen(TRUE)
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )
  selection_controls <- shiny::reactive({
    controls <- config$selection_controls
    replace_if_configured <- function(name, value) {
      has_value <- !is.null(value) && length(value) > 0L && !all(is.na(value))
      is_empty_label_set <- identical(name, "nonrenewable") && is.character(value) && length(value) == 0L
      if (name %in% names(controls) && (has_value || is_empty_label_set)) {
        controls[[name]] <<- value
      }
    }
    replace_if_configured("relevance_threshold", input$threshold)
    replace_if_configured("kw_min", as.integer(input$kw_min))
    replace_if_configured("use_classifier", if (is.null(input$use_classifier)) NULL else isTRUE(input$use_classifier))
    selected_exclusions <- if (is.null(input$use_trim)) {
      NULL
    } else if (!isTRUE(input$use_trim)) {
      character(0)
    } else if (!nonrenewable_input_seen()) {
      NULL
    } else {
      if (is.null(input$nonrenewable)) character(0) else as.character(input$nonrenewable)
    }
    replace_if_configured("use_trim", if (is.null(input$use_trim)) NULL else isTRUE(input$use_trim))
    replace_if_configured("nonrenewable", selected_exclusions)
    replace_if_configured("nonrenewable_score", input$nonrenewable_score)
    controls
  })
  model_scored <- shiny::reactive({
    data <- records()
    fitted <- model()
    if (is.null(fitted)) {
      data$select_prob <- rep(NA_real_, nrow(data))
      data$selected_model <- rep(FALSE, nrow(data))
      return(data)
    }
    tryCatch(
      curation_predict_model(fitted, data, config, threshold = input$model_threshold %||% 0.5),
      error = function(e) {
        shiny::showNotification(paste0("Model prediction failed: ", conditionMessage(e)), type = "warning")
        data$select_prob <- rep(NA_real_, nrow(data))
        data$selected_model <- rep(FALSE, nrow(data))
        data
      }
    )
  })
  selected_records <- shiny::reactive({
    data <- curation_apply_selection(config, model_scored(), selection_controls())
    if (!"selected_model" %in% names(data)) data$selected_model <- rep(FALSE, nrow(data))
    data
  })
  acquisition_preview <- if (!is.null(acquisition) && isTRUE(acquisition$available)) {
    shiny::reactive({
      reference <- model_reference()
      policy <- acquisition$policy %||% list()
      policy_version <- acquisition$policy_version %||% policy$policy_version %||% NULL
      priority_rules <- acquisition$priority_rules %||% policy$priority_rules %||% NULL
      if (is.null(reference) || is.null(policy_version) || is.null(priority_rules)) {
        return(NULL)
      }
      acquisition_campaign_preview_from_curation(
        catalog = acquisition$catalog,
        model_reference = reference,
        records = selected_records(),
        threshold = suppressWarnings(as.numeric(input$model_threshold %||% 0.5)),
        policy_version = policy_version,
        priority_rules = priority_rules,
        records_snapshot_id = snapshot_id(),
        records_snapshot_version = snapshot_version(),
        records_snapshot_fingerprint = curation_artifact_content_fingerprint(records())
      )
    })
  } else {
    NULL
  }
  if (!is.null(acquisition) && isTRUE(acquisition$available)) {
    acquisition_actor_resolver <- acquisition$actor_resolver %||% context$actor_resolver
    acquisition_actor_probe <- acquisition$actor_probe %||% context$actor_probe
    tryCatch(
      acquisition_dashboard_server(
        id = "acquisition",
        catalog = acquisition$catalog,
        actor_resolver = acquisition_actor_resolver,
        request = acquisition$request %||% context$request,
        actor_probe = acquisition_actor_probe,
        preview = acquisition$preview %||% acquisition_preview %||% NULL,
        submit_fn = acquisition$campaign_submit_fn %||% acquisition_campaign_submit_authenticated,
        refresh_ms = acquisition$refresh_ms %||% 2000L,
        page_size = acquisition$page_size %||% 50L
      ),
      error = function(error) {
        shiny::showNotification(
          paste0("Acquisition dashboard unavailable: ", conditionMessage(error)),
          type = "warning", duration = 8
        )
      }
    )
  }
  countries_debounced <- shiny::debounce(shiny::reactive(selected_countries()), 500)
  filtered <- shiny::reactive({
    data <- selected_records()
    data[data$country %in% countries_debounced(), , drop = FALSE]
  })

  active_reviews <- shiny::reactive({
    curation_app_active_reviews(reviews())
  })
  initial_admin <- curation_app_is_admin(actor)
  output$admin_review_ui <- shiny::renderUI({
    if (!isTRUE(initial_admin)) {
      return(NULL)
    }
    bslib <- asNamespace("bslib")
    bslib$card(
      bslib$card_header("Admin review management"),
      shiny::helpText(
        "Administrators can edit an existing review for any target reviewer. The authenticated administrator remains the actor; the target reviewer is shown per row."
      ),
      reactable::reactableOutput("admin_review_tbl", height = "420px"),
      max_height = "560px",
      class = "curation-admin-card"
    )
  })
  output$admin_review_tbl <- reactable::renderReactable({
    if (!isTRUE(initial_admin)) {
      return(reactable::reactable(data.frame(Note = "Administrator access is required."), pagination = FALSE))
    }
    curation_app_admin_table(reviews(), historical_links = historical_links)
  })
  current_reviewed_keys <- shiny::reactive({
    rv <- active_reviews()
    rv <- rv[rv$reviewer_id == reviewer_id, , drop = FALSE]
    if (nrow(rv) == 0L) character(0) else unique(curation_app_key_string(rv$country, rv$document_id))
  })

  output$country_labels_summary <- shiny::renderUI({
    counts <- curation_country_label_counts(
      records(), reviews(), countries(),
      reviewer_id = reviewer_id, scope = scope
    )
    htmltools::tags$table(
      style = "width:100%;font-size:12px;border-collapse:collapse;margin:6px 0;",
      htmltools::tags$tbody(lapply(seq_len(nrow(counts)), function(i) {
        htmltools::tags$tr(
          htmltools::tags$td(toupper(counts$country[[i]])),
          htmltools::tags$td(sprintf("%s / %s", counts$labelled[[i]], counts$records[[i]]), style = "text-align:right;")
        )
      }))
    )
  })
  output$label_counts_tbl <- reactable::renderReactable({
    counts <- curation_country_label_counts(records(), reviews(), countries(), reviewer_id = reviewer_id, scope = scope)
    reactable::reactable(counts, compact = TRUE, pagination = FALSE)
  })
  shiny::observeEvent(input$show_label_counts, {
    shiny::showModal(shiny::modalDialog(
      title = "Label counts by country", size = "l", reactable::reactableOutput("label_counts_tbl"),
      footer = shiny::modalButton("Close")
    ))
  })

  output$model_info <- shiny::renderUI({
    fitted <- model()
    if (is.null(fitted)) {
      return(shiny::helpText("No model published yet."))
    }
    audit <- fitted$training_audit %||% list()
    eligible <- audit$eligible %||% list()
    exclusions <- audit$exclusions %||% list()
    countries <- fitted$cv_by_country$country %||% character(0)
    countries <- setdiff(as.character(countries), "all")
    receipt <- fitted$training_provenance$cohort_receipt %||% list()
    receipt_id <- receipt$id %||% receipt$receipt_id %||% NULL
    input <- fitted$training_provenance$input %||% list()
    input_id <- input$snapshot_id %||% input$version %||% NULL
    exclusion_text <- paste0(
      "unsure rows ", exclusions$unsure %||% 0L,
      "; disagreement projects ", exclusions$disagreement %||% 0L,
      "; unresolved projects ", exclusions$unresolved %||% 0L,
      "; unmatched labels ", exclusions$unmatched %||% 0L,
      "; incompatible score rows ", exclusions$incompatible %||% 0L
    )
    details <- c(
      sprintf("Model: %s - %d eligible labels", fitted$learner_name %||% "selection", fitted$n_train %||% NA_integer_),
      sprintf("Eligible keep/drop: %d/%d scored rows", eligible$keep %||% fitted$n_keep %||% 0L, eligible$drop %||% (fitted$n_train - (fitted$n_keep %||% 0L)) %||% 0L),
      paste0("Excluded audit: ", exclusion_text),
      sprintf("OOF random cohort: %s", if (length(countries) == 0L) "unavailable" else paste(countries, collapse = ", "))
    )
    if (!is.null(input_id)) details <- c(details, paste0("Input snapshot: ", input_id))
    if (!is.null(receipt_id)) details <- c(details, paste0("Cohort receipt: ", receipt_id))
    shiny::helpText(shiny::tagList(lapply(details, shiny::tags$div)))
  })
  output$learning_curve_plot <- plotly::renderPlotly({
    curation_app_learning_plot(learning_curve())
  })

  shiny::observeEvent(input$train_model, {
    trigger <- input$train_model
    if (is.null(trigger) || !is.numeric(trigger) || length(trigger) != 1L || is.na(trigger) || trigger <= 0) {
      return()
    }
    training_actor <- tryCatch(current_actor(), error = function(e) e)
    if (inherits(training_actor, "error")) {
      shiny::showNotification(
        paste0("Training failed: ", conditionMessage(training_actor)),
        type = "error", duration = 8
      )
      return()
    }
    model_threshold <- as.numeric(input$model_threshold %||% 0.5)
    active_before <- tryCatch(
      curation_artifact_active_model(artifact_store, scope, training_actor),
      error = function(e) NULL
    )
    expected_active_revision <- if (is.null(active_before)) {
      0L
    } else {
      as.integer(active_before$active_revision %||% active_before$revision %||% 0L)
    }
    provenance <- tryCatch(
      {
        training_reviews <- curation_app_historical_review_rows(
          reviews(),
          historical_links = historical_links
        )
        input_identity <- curation_app_training_input_provenance(
          base = model_input_provenance,
          records = records(),
          snapshot_id = snapshot_id(),
          snapshot_version = snapshot_version()
        )
        curation_app_model_provenance(
          scope = scope,
          config = config,
          records = records(),
          reviews = training_reviews,
          actor = training_actor,
          snapshot_version = snapshot_version(),
          snapshot_id = snapshot_id(),
          learner_key = input$learner %||% "logistic",
          eval_source = "random",
          threshold = model_threshold,
          input_provenance = input_identity,
          cohort_receipt = cohort_receipt
        )
      },
      error = function(e) e
    )
    if (inherits(provenance, "error")) {
      shiny::showNotification(paste0("Training failed: ", conditionMessage(provenance)), type = "error", duration = 8)
      return()
    }
    fitted <- tryCatch(
      shiny::withProgress(message = "Training selection model (cross-validating)...", value = 0.5, {
        training_reviews <- curation_app_historical_review_rows(
          reviews(),
          historical_links = historical_links
        )
        curation_train_model(
          records(), training_reviews, config,
          learner_key = input$learner %||% "logistic",
          eval_source = "random",
          threshold = model_threshold,
          input_provenance = curation_app_training_input_provenance(
            base = model_input_provenance,
            records = records(),
            snapshot_id = snapshot_id(),
            snapshot_version = snapshot_version()
          ),
          cohort_receipt = cohort_receipt
        )
      }),
      error = function(e) e
    )
    if (inherits(fitted, "error")) {
      shiny::showNotification(paste0("Training failed: ", conditionMessage(fitted)), type = "error", duration = 8)
      return()
    }
    provenance$training_audit <- fitted$training_audit %||% list()
    provenance$metrics <- fitted$cv_by_country %||% selection_cv_metrics(fitted, by_country = TRUE)
    publication_actor <- tryCatch(current_actor(), error = function(e) e)
    if (inherits(publication_actor, "error")) {
      shiny::showNotification(
        paste0("Model publication failed: ", conditionMessage(publication_actor)),
        type = "warning", duration = 8
      )
      return()
    }
    # Model publication is the only artifact write here.  The app never writes
    # an RDS path or a session-wide review frame.
    published <- tryCatch(
      {
        curation_publish_model(
          artifact_store, scope, fitted,
          provenance = provenance,
          # Every model version is immutable.  A non-zero model-list revision
          # is not a valid CAS value for a newly-created version.
          expected_revision = 0L,
          expected_active_revision = expected_active_revision,
          config = config,
          actor = publication_actor
        )
      },
      error = identity
    )
    if (inherits(published, "error")) {
      # Keep displaying the previously active model.  A trained object is not
      # part of the session state until its immutable write and active CAS
      # publication both succeed.
      shiny::showNotification(
        paste0("Model trained but could not be published: ", conditionMessage(published)),
        type = "warning", duration = 8
      )
      return()
    }
    model(fitted)
    model_reference(
      curation_app_read_model_reference(
        artifact_store,
        scope,
        publication_actor,
        config
      ) %||% list(
        model = fitted,
        model_id = published$active_model_id %||% published$model_id %||% NULL,
        input_snapshot_id = snapshot_id(),
        input_snapshot_version = snapshot_version(),
        input_snapshot_fingerprint = curation_artifact_content_fingerprint(records()),
        artifact_path = NULL,
        provenance = provenance,
        config_id = scope$config_id,
        created_by = publication_actor$id
      )
    )
    shiny::showNotification("Selection model trained.", type = "message")
  })
  shiny::observeEvent(input$compute_lc, {
    result <- tryCatch(
      shiny::withProgress(message = "Computing learning curve...", value = 0.5, {
        curation_learning_curve(records(), reviews(), config, learner_key = input$learner %||% "logistic", by_country = TRUE)
      }),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      shiny::showNotification(paste0("Learning curve failed: ", conditionMessage(result)), type = "error", duration = 8)
    } else {
      learning_curve(result)
    }
  })

  output$vb_total <- shiny::renderText(format(nrow(filtered()), big.mark = ","))
  output$vb_selected <- shiny::renderText({
    data <- filtered()
    n <- sum(data$selected %in% TRUE)
    if (nrow(data) == 0L) "\u2014" else sprintf("%s (%.1f%%)", format(n, big.mark = ","), 100 * n / nrow(data))
  })
  output$vb_selected_model <- shiny::renderText({
    data <- filtered()
    if (nrow(data) == 0L || !any(!is.na(data$select_prob))) {
      return("\u2014")
    }
    n <- sum(data$selected_model %in% TRUE)
    sprintf("%s (%.1f%%)", format(n, big.mark = ","), 100 * n / nrow(data))
  })
  output$vb_reviewed <- shiny::renderText({
    data <- filtered()
    if (nrow(data) == 0L) {
      return("\u2014")
    }
    n <- sum(curation_app_key_string(data$country, data$document_id) %in% current_reviewed_keys())
    sprintf("%s (%.1f%%)", format(n, big.mark = ","), 100 * n / nrow(data))
  })

  funnel_context <- shiny::reactive(list(
    model = model(),
    has_model = any(!is.na(filtered()$select_prob)),
    reviews = active_reviews(),
    reviewer_id = reviewer_id,
    scope = scope
  ))
  output$funnel_plot <- plotly::renderPlotly({
    curation_app_funnel_plot(curation_app_funnel(filtered(), config, context = funnel_context(), by_country = FALSE))
  })
  output$funnel_table <- reactable::renderReactable({
    data <- curation_app_funnel(filtered(), config, context = funnel_context(), by_country = TRUE)
    reactable::reactable(data[, intersect(c("country", "stage", "n", "pct"), names(data)), drop = FALSE], groupBy = "country", compact = TRUE, pagination = FALSE)
  })
  output$historical_overview_tbl <- reactable::renderReactable({
    curation_app_historical_only_table(
      historical_links,
      current_records = records(), reviews = reviews()
    )
  })

  display_df <- shiny::reactive({
    data <- filtered()
    rv <- active_reviews()
    keys <- curation_app_key_string(data$country, data$document_id)
    f <- input$stage_filter %||% "all"
    data <- switch(f,
      selected = data[data$selected %in% TRUE, , drop = FALSE],
      not_selected = data[!(data$selected %in% TRUE), , drop = FALSE],
      model_selected = data[data$selected_model %in% TRUE, , drop = FALSE],
      model_not_selected = data[!(data$selected_model %in% TRUE), , drop = FALSE],
      model_vs_heuristic = data[(data$selected %in% TRUE) != (data$selected_model %in% TRUE), , drop = FALSE],
      reviewed = data[keys %in% current_reviewed_keys(), , drop = FALSE],
      unreviewed = data[!keys %in% current_reviewed_keys(), , drop = FALSE],
      disagree = {
        agreed <- tryCatch(curation_selection_vs_human(data, rv, config = config, eval_source = NULL), error = function(e) NULL)
        if (is.null(agreed)) {
          data[0, , drop = FALSE]
        } else {
          labels <- stats::setNames(rv$decision, curation_app_key_string(rv$country, rv$document_id))
          human <- labels[curation_app_key_string(data$country, data$document_id)]
          auto <- data$selected %in% TRUE
          reviewed <- !is.na(human)
          data[reviewed & ((auto & human == "drop") | (!auto & human == "keep")), , drop = FALSE]
        }
      },
      data
    )
    cosine <- data$cosine_max
    cosine[is.na(cosine)] <- -1
    data[order(-(data$selected %in% TRUE), -cosine), , drop = FALSE]
  })
  display_capped <- shiny::reactive({
    data <- display_df()
    if (nrow(data) > 5000L) {
      attr(data, "truncated") <- nrow(data)
      data <- data[seq_len(5000L), , drop = FALSE]
    }
    data
  })
  output$review_info <- shiny::renderText({
    data <- display_capped()
    total <- attr(data, "truncated")
    if (is.null(total)) sprintf("%d rows", nrow(data)) else sprintf("showing %d of %d (capped)", nrow(data), total)
  })
  output$review_tbl <- reactable::renderReactable({
    data <- display_capped()
    display_rows <- curation_app_review_display_rows(data, reviews(), reviewer_id)
    render_id <- curation_artifact_content_fingerprint(display_rows)
    review_display_tokens$review_tbl <- render_id
    session$sendCustomMessage(
      "curationReviewDisplayed",
      list(
        table_id = "review_tbl",
        render_id = render_id,
        rows = display_rows
      )
    )
    curation_app_table(
      data, reviews(), config,
      source = "browse", reviewer_id = reviewer_id,
      render_id = render_id, historical_links = historical_links
    )
  })
  output$historical_review_tbl <- reactable::renderReactable({
    curation_app_historical_only_table(
      historical_links,
      current_records = records(), reviews = reviews()
    )
  })

  write_one <- function(country, document_id, decision, source, expected_revision = NULL) {
    write_actor <- current_actor()
    available <- records()
    valid_record <- nrow(available) > 0L && any(
      as.character(available$country) == as.character(country) &
        as.character(available$document_id) == as.character(document_id)
    )
    if (!valid_record) {
      curation_abort(
        "The review event did not identify a record in the current snapshot.",
        class = "planscanR_error_curation_repository"
      )
    }
    if (!curation_app_historical_mutation_allowed(historical_links, country, document_id)) {
      curation_abort(
        "Historical linkage is unresolved; current mutations are disabled.",
        class = "planscanR_error_curation_repository"
      )
    }
    if (is.null(expected_revision) || length(expected_revision) == 0L) {
      curation_abort(
        "The review event did not include the displayed revision.",
        class = "planscanR_error_curation_conflict"
      )
    }
    expected <- curation_app_validate_displayed_revision(expected_revision)
    key <- curation_review_key(country, document_id, reviewer_id)
    if (is.null(decision)) {
      result <- curation_review_clear(repository, scope, key, write_actor, expected)
      decision_row <- NULL
    } else {
      decision_row <- data.frame(
        country = as.character(country), document_id = as.character(document_id), reviewer_id = reviewer_id,
        decision = as.character(decision), source = as.character(source), note = NA_character_,
        reviewed_at = as.POSIXct(Sys.time(), tz = "UTC"), sidecar_path = NA_character_,
        stringsAsFactors = FALSE
      )
      result <- curation_review_upsert(repository, scope, decision_row, write_actor, expected)
    }
    reviews(curation_app_replace_review_result(reviews(), decision_row, result, write_actor))
    canonical <- curation_app_canonical_review_update(reviews(), key)
    try(
      session$sendCustomMessage("curationReviewRefresh", list(updates = list(canonical))),
      silent = TRUE
    )
    result
  }
  refresh_conflict <- function(error, country = NULL, document_id = NULL) {
    if (!inherits(error, "planscanR_error_curation_conflict")) {
      return(invisible(FALSE))
    }
    key <- error$key
    if (!is.list(key) || is.null(key$country) || is.null(key$document_id)) {
      if (is.null(country) || is.null(document_id)) {
        return(invisible(FALSE))
      }
      key <- curation_review_key(country, document_id, reviewer_id)
    }
    refreshed <- tryCatch(
      {
        refresh_actor <- current_actor()
        curation_app_refresh_review(reviews(), repository, scope, key, refresh_actor)
      },
      error = function(refresh_error) refresh_error
    )
    if (inherits(refreshed, "error")) {
      shiny::showNotification(
        paste0("The review changed in another session and could not be refreshed: ", conditionMessage(refreshed)),
        type = "warning", duration = 8
      )
      return(invisible(FALSE))
    }
    reviews(refreshed)
    canonical <- curation_app_canonical_review_update(refreshed, key)
    try(
      session$sendCustomMessage("curationReviewRefresh", list(updates = list(canonical))),
      silent = TRUE
    )
    shiny::showNotification(
      "This review changed in another session; the current decision was reloaded. Save again if needed.",
      type = "warning", duration = 8
    )
    invisible(TRUE)
  }
  safe_write_one <- function(country, document_id, decision, source, expected_revision = NULL) {
    key <- curation_app_key_string(country, document_id, reviewer_id)
    pending_before <- pending_review_keys()
    pending_review_keys(unique(c(pending_before, key)))
    on.exit(pending_review_keys(pending_before), add = TRUE)
    tryCatch(
      write_one(country, document_id, decision, source, expected_revision = expected_revision),
      error = function(e) {
        refresh_conflict(e, country, document_id)
        shiny::showNotification(paste0("Review write failed: ", conditionMessage(e)), type = "error", duration = 8)
        NULL
      }
    )
  }
  shiny::observeEvent(input$admin_row_decision,
    {
      info <- input$admin_row_decision
      required <- c("country", "document_id", "reviewer", "decision", "revision")
      if (!is.list(info) || any(vapply(required, function(name) is.null(info[[name]]) || length(info[[name]]) == 0L, logical(1)))) {
        shiny::showNotification(
          "Administrator write stopped because the target row or displayed revision was missing.",
          type = "warning", duration = 8
        )
        return()
      }
      admin_actor <- tryCatch(current_actor(), error = function(error) error)
      if (inherits(admin_actor, "error")) {
        shiny::showNotification(paste0("Administrator write failed: ", conditionMessage(admin_actor)), type = "error", duration = 8)
        return()
      }
      if (!curation_app_is_admin(admin_actor)) {
        shiny::showNotification("Administrator access is required to edit another reviewer's row.", type = "warning", duration = 8)
        return()
      }
      displayed_revision <- tryCatch(
        curation_app_validate_displayed_revision(info$revision),
        error = function(error) error
      )
      if (inherits(displayed_revision, "error")) {
        shiny::showNotification("Administrator write stopped because the displayed revision was invalid.", type = "warning", duration = 8)
        return()
      }
      country <- as.character(info$country[[1L]])
      document_id <- as.character(info$document_id[[1L]])
      target_reviewer <- as.character(info$reviewer[[1L]])
      decision <- as.character(info$decision[[1L]])
      if (!decision %in% c("keep", "drop", "unsure")) {
        shiny::showNotification("Administrator write stopped because the decision was invalid.", type = "warning", duration = 8)
        return()
      }
      if (!curation_app_historical_mutation_allowed(historical_links, country, document_id)) {
        shiny::showNotification("Administrator write stopped because historical linkage is unresolved.", type = "warning", duration = 8)
        return()
      }
      key <- curation_review_key(country, document_id, target_reviewer)
      current <- tryCatch(curation_review_read(repository, scope, key, admin_actor), error = function(error) error)
      if (inherits(current, "error")) {
        shiny::showNotification(paste0("Administrator write failed: ", conditionMessage(current)), type = "error", duration = 8)
        return()
      }
      if (is.null(current) || nrow(current) == 0L || isTRUE(as.logical(current$deleted[[1L]])) || is.na(current$decision[[1L]]) || !nzchar(current$decision[[1L]])) {
        shiny::showNotification("Administrator write stopped because the target review no longer exists.", type = "warning", duration = 8)
        return()
      }
      pending_before <- pending_review_keys()
      pending_key <- curation_app_key_string(country, document_id, target_reviewer)
      pending_review_keys(unique(c(pending_before, pending_key)))
      on.exit(pending_review_keys(pending_before), add = TRUE)
      decision_row <- data.frame(
        country = country,
        document_id = document_id,
        reviewer_id = target_reviewer,
        decision = decision,
        source = as.character(current$source[[1L]]),
        note = current$note[[1L]],
        reviewed_at = as.POSIXct(Sys.time(), tz = "UTC"),
        sidecar_path = current$sidecar_path[[1L]],
        stringsAsFactors = FALSE
      )
      result <- tryCatch(
        curation_review_upsert(repository, scope, decision_row, admin_actor, displayed_revision),
        error = function(error) error
      )
      if (inherits(result, "error")) {
        refresh_conflict(result, country, document_id)
        shiny::showNotification(paste0("Administrator write failed: ", conditionMessage(result)), type = "error", duration = 8)
        return()
      }
      reviews(curation_app_replace_review_result(reviews(), decision_row, result, admin_actor))
      canonical <- curation_app_canonical_review_update(reviews(), key)
      try(
        session$sendCustomMessage("curationReviewRefresh", list(updates = list(canonical))),
        silent = TRUE
      )
      shiny::showNotification(
        sprintf("Updated %s/%s for reviewer %s.", country, document_id, target_reviewer),
        type = "message", duration = 6
      )
    },
    ignoreInit = TRUE
  )
  shiny::observeEvent(input$row_decision,
    {
      info <- input$row_decision
      if (is.null(info) || is.null(info$id) || is.null(info$country)) {
        return()
      }
      if (is.null(info$revision) || length(info$revision) == 0L) {
        shiny::showNotification(
          "Review write stopped because the displayed revision was missing; refresh and try again.",
          type = "warning", duration = 8
        )
        return()
      }
      decision <- if (is.null(info$decision)) NULL else as.character(info$decision)
      source <- if (is.null(info$source)) "browse" else as.character(info$source)
      displayed_revision <- curation_app_validate_displayed_revision(info$revision)
      safe_write_one(info$country, info$id, decision, source, expected_revision = displayed_revision)
    },
    ignoreInit = TRUE
  )

  apply_bulk <- function(decision, output_id, data, source) {
    displayed <- input$review_bulk_display
    if (!is.list(displayed) || is.null(displayed$table_id) ||
      !identical(as.character(displayed$table_id), output_id) ||
      is.null(displayed$render_id) || length(displayed$render_id) != 1L ||
      !identical(as.character(displayed$render_id), review_display_tokens[[output_id]])) {
      shiny::showNotification(
        "Bulk action stopped because the selected rows are no longer from the displayed table; refresh and try again.",
        type = "warning"
      )
      return(invisible(NULL))
    }
    displayed_rows <- displayed$rows %||% list()
    if (!is.list(displayed_rows) || length(displayed_rows) == 0L) {
      shiny::showNotification("No rows ticked.", type = "warning")
      return(invisible(NULL))
    }
    displayed_revisions <- list()
    displayed_keys <- character(0)
    for (entry in displayed_rows) {
      if (!is.list(entry) || is.null(entry$country) || is.null(entry$document_id) || is.null(entry$revision)) {
        shiny::showNotification(
          "Bulk action stopped because a selected row has no displayed revision; refresh the table and try again.",
          type = "warning"
        )
        return(invisible(NULL))
      }
      revision <- tryCatch(
        curation_app_validate_displayed_revision(entry$revision),
        error = function(error) NA_integer_
      )
      if (length(revision) != 1L || is.na(revision)) {
        shiny::showNotification(
          "Bulk action stopped because a selected row has no valid displayed revision; refresh the table and try again.",
          type = "warning"
        )
        return(invisible(NULL))
      }
      key <- curation_app_key_string(entry$country, entry$document_id, reviewer_id)
      if (key %in% displayed_keys) {
        shiny::showNotification("Bulk action stopped because the rendered selection contains a duplicate row.", type = "warning")
        return(invisible(NULL))
      }
      displayed_keys <- c(displayed_keys, key)
      displayed_revisions[[key]] <- revision
    }
    current_keys <- curation_app_key_string(data$country, data$document_id, reviewer_id)
    index <- match(displayed_keys, current_keys)
    if (anyNA(index)) {
      shiny::showNotification(
        "Bulk action stopped because a rendered row is no longer in the current table; refresh and try again.",
        type = "warning"
      )
      return(invisible(NULL))
    }
    selected <- data[index, c("country", "document_id"), drop = FALSE]
    allowed <- vapply(seq_len(nrow(selected)), function(i) {
      curation_app_historical_mutation_allowed(
        historical_links, selected$country[[i]], selected$document_id[[i]]
      )
    }, logical(1))
    if (any(!allowed)) {
      shiny::showNotification(
        "Bulk action stopped because one or more selected rows have unresolved historical linkage.",
        type = "warning"
      )
      return(invisible(NULL))
    }
    expected <- vapply(
      displayed_keys,
      function(key) as.integer(displayed_revisions[[key]]),
      integer(1)
    )
    if (anyNA(expected)) {
      shiny::showNotification(
        "Bulk action stopped because a selected row has no displayed revision; refresh the table and try again.",
        type = "warning"
      )
      return(invisible(NULL))
    }
    selected_keys <- curation_app_key_string(
      selected$country, selected$document_id, reviewer_id
    )
    if (!identical(selected_keys, displayed_keys)) {
      shiny::showNotification(
        "Bulk action stopped because the rendered rows changed before the action was submitted; refresh and try again.",
        type = "warning"
      )
      return(invisible(NULL))
    }
    pending_before <- pending_review_keys()
    pending_review_keys(unique(c(pending_before, selected_keys)))
    on.exit(pending_review_keys(pending_before), add = TRUE)
    if (is.null(decision)) {
      result <- tryCatch(
        {
          out <- vector("list", nrow(selected))
          for (i in seq_len(nrow(selected))) {
            out[[i]] <- write_one(
              selected$country[[i]], selected$document_id[[i]], NULL, source,
              expected_revision = expected[[i]]
            )
          }
          out
        },
        error = function(e) e
      )
      if (inherits(result, "error")) {
        refresh_conflict(result)
        shiny::showNotification(paste0("Bulk clear failed: ", conditionMessage(result)), type = "error", duration = 8)
        return(invisible(NULL))
      }
      keys <- curation_app_key_string(selected$country, selected$document_id, reviewer_id)
    } else {
      decisions <- data.frame(
        country = as.character(selected$country), document_id = as.character(selected$document_id), reviewer_id = reviewer_id,
        decision = rep(as.character(decision), nrow(selected)), source = rep(as.character(source), nrow(selected)),
        note = NA_character_, reviewed_at = rep(as.POSIXct(Sys.time(), tz = "UTC"), nrow(selected)),
        sidecar_path = NA_character_, stringsAsFactors = FALSE
      )
      write_actor <- tryCatch(current_actor(), error = function(e) e)
      if (inherits(write_actor, "error")) {
        shiny::showNotification(paste0("Bulk write failed: ", conditionMessage(write_actor)), type = "error", duration = 8)
        return(invisible(NULL))
      }
      result <- tryCatch(curation_review_bulk_upsert(repository, scope, decisions, write_actor, expected), error = function(e) e)
      if (inherits(result, "error")) {
        refresh_conflict(result)
        shiny::showNotification(paste0("Bulk write failed: ", conditionMessage(result)), type = "error", duration = 8)
        return(invisible(NULL))
      }
      result_rows <- result$results
      for (i in seq_len(nrow(decisions))) {
        one_result <- list(
          key = curation_review_key(decisions$country[[i]], decisions$document_id[[i]], reviewer_id),
          revision = result_rows$revision[[i]], deleted = result_rows$deleted[[i]]
        )
        reviews(curation_app_replace_review_result(reviews(), decisions[i, , drop = FALSE], one_result, write_actor))
      }
      keys <- curation_app_key_string(decisions$country, decisions$document_id, reviewer_id)
    }
    updates <- lapply(seq_len(nrow(selected)), function(i) {
      canonical <- curation_app_canonical_review_update(
        reviews(),
        curation_review_key(selected$country[[i]], selected$document_id[[i]], reviewer_id)
      )
      canonical
    })
    session$sendCustomMessage(
      "curationBulkDecision",
      list(keys = as.list(keys), decision = decision, updates = updates)
    )
    try(reactable::updateReactable(output_id, selected = NA), silent = TRUE)
    shiny::showNotification(sprintf("%d record(s) updated.", length(index)), type = "message")
    invisible(NULL)
  }
  shiny::observeEvent(input$mark_keep, apply_bulk("keep", "review_tbl", display_capped(), "browse"))
  shiny::observeEvent(input$mark_drop, apply_bulk("drop", "review_tbl", display_capped(), "browse"))
  shiny::observeEvent(input$mark_unsure, apply_bulk("unsure", "review_tbl", display_capped(), "browse"))
  shiny::observeEvent(input$mark_clear, apply_bulk(NULL, "review_tbl", display_capped(), "browse"))

  output$agreement_ui <- shiny::renderUI({
    agreement <- tryCatch(curation_inter_reviewer_summary(reviews(), config), error = function(e) NULL)
    agreement_ui <- if (is.null(agreement)) NULL else shiny::helpText(sprintf("Cross-reviewer agreement: %.0f%% \u2014 %d of %d records.", 100 * agreement$agreement_rate, agreement$n_agree, agreement$n_multi))
    eval_source <- if (identical(input$eval_on, "random")) "random" else NULL
    heuristic <- tryCatch(curation_selection_vs_human(filtered(), reviews(), config, eval_source = eval_source), error = function(e) NULL)
    fitted <- model()
    model_metrics <- if (is.null(fitted)) NULL else tryCatch(curation_model_metrics(fitted, config, input$model_threshold), error = function(e) NULL)
    if (is.null(heuristic)) {
      return(shiny::tagList(agreement_ui, shiny::helpText("No agreed labels for this view yet.")))
    }
    fmt <- function(value) if (is.null(value) || is.na(value)) "\u2014" else sprintf("%.2f", value)
    shiny::tagList(
      agreement_ui,
      htmltools::tags$table(
        class = "curation-comparison",
        htmltools::tags$thead(htmltools::tags$tr(htmltools::tags$th("Method"), htmltools::tags$th("Reviewed"), htmltools::tags$th("Precision \u00b7 Recall \u00b7 F1"), htmltools::tags$th("TP / FP / FN / TN"))),
        htmltools::tags$tbody(
          htmltools::tags$tr(htmltools::tags$td("Heuristic"), htmltools::tags$td(heuristic$n_reviewed), htmltools::tags$td(sprintf("%s \u00b7 %s \u00b7 %s", fmt(heuristic$precision), fmt(heuristic$recall), fmt(heuristic$f1))), htmltools::tags$td(sprintf("%d / %d / %d / %d", heuristic$tp, heuristic$fp, heuristic$fn, heuristic$tn))),
          htmltools::tags$tr(htmltools::tags$td("Model"), htmltools::tags$td(if (is.null(model_metrics)) "\u2014" else model_metrics$n_reviewed), htmltools::tags$td(if (is.null(model_metrics)) "\u2014" else sprintf("%s \u00b7 %s \u00b7 %s", fmt(model_metrics$precision), fmt(model_metrics$recall), fmt(model_metrics$f1))), htmltools::tags$td(if (is.null(model_metrics)) "\u2014" else sprintf("%d / %d / %d / %d", model_metrics$tp, model_metrics$fp, model_metrics$fn, model_metrics$tn)))
        )
      )
    )
  })
  output$perf_by_country <- reactable::renderReactable({
    heur <- tryCatch(curation_selection_vs_human(filtered(), reviews(), config, eval_source = "random", by_country = TRUE), error = function(e) NULL)
    if (is.null(heur)) {
      return(reactable::reactable(data.frame(Note = "No random-sample labels yet."), pagination = FALSE))
    }
    heur$method <- "Heuristic"
    fitted <- model()
    model_metrics <- if (is.null(fitted)) {
      NULL
    } else {
      tryCatch(
        curation_model_metrics(fitted, config, input$model_threshold, by_country = TRUE),
        error = function(e) NULL
      )
    }
    if (!is.null(model_metrics)) model_metrics$method <- "Model"
    columns <- c("method", "country", setdiff(names(heur), c("method", "country")))
    combined <- dplyr::bind_rows(heur, model_metrics)
    combined <- combined[, intersect(columns, names(combined)), drop = FALSE]
    reactable::reactable(combined, compact = TRUE, pagination = FALSE)
  })
  shiny::observeEvent(input$show_perf_country, {
    shiny::showModal(shiny::modalDialog(title = "Performance by country", size = "l", reactable::reactableOutput("perf_by_country"), footer = shiny::modalButton("Close")))
  })

  output$random_tbl <- reactable::renderReactable({
    data <- random_df()
    display_rows <- curation_app_review_display_rows(data, reviews(), reviewer_id)
    render_id <- curation_artifact_content_fingerprint(display_rows)
    review_display_tokens$random_tbl <- render_id
    session$sendCustomMessage(
      "curationReviewDisplayed",
      list(
        table_id = "random_tbl",
        render_id = render_id,
        rows = display_rows
      )
    )
    curation_app_table(
      data, reviews(), config,
      source = "random", blind = isTRUE(input$rnd_blind),
      reviewer_id = reviewer_id, render_id = render_id, historical_links = historical_links
    )
  })
  output$warm_status <- shiny::renderUI({
    if (is.null(config$translator)) shiny::helpText("Translation provider disabled.") else shiny::helpText("Translations are supplied by the injected provider.")
  })
  output$rnd_info <- shiny::renderText({
    queue <- sample()
    if (nrow(queue) == 0L) {
      return("No queue yet \u2014 click Build queue.")
    }
    mine <- curation_app_active_reviews(reviews())
    mine <- mine[mine$reviewer_id == reviewer_id, , drop = FALSE]
    done <- sum(curation_app_key_string(queue$country, queue$document_id) %in% curation_app_key_string(mine$country, mine$document_id))
    sprintf("%d in queue \u00b7 %d done by you \u00b7 %d remaining", nrow(queue), done, nrow(queue) - done)
  })

  shiny::observeEvent(input$rnd_draw, {
    sample_actor <- tryCatch(current_actor(), error = function(e) e)
    if (inherits(sample_actor, "error")) {
      shiny::showNotification(
        paste0("Queue build failed: ", conditionMessage(sample_actor)),
        type = "error", duration = 8
      )
      return()
    }
    countries_now <- selected_countries()
    queue <- tryCatch(
      curation_build_review_queue(
        records(), reviews(),
        reviewer_id = reviewer_id, countries = countries_now,
        n_per_country = input$rnd_n %||% 50L, seed = input$rnd_seed, scope = scope
      ),
      error = function(e) e
    )
    if (inherits(queue, "error")) {
      shiny::showNotification(paste0("Queue build failed: ", conditionMessage(queue)), type = "error", duration = 8)
      return()
    }
    # Each draw is a new immutable artifact.  Reusing the previous reviewer
    # sample ID would turn an ordinary queue rebuild into an overwrite attempt
    # and would make restart recovery select the wrong queue.
    new_sample_id <- curation_app_new_sample_id(
      scope, reviewer_id, queue,
      seed = input$rnd_seed
    )
    sample_payload <- list(
      sample_id = new_sample_id,
      reviewer_id = reviewer_id,
      queue = queue,
      mode = attr(queue, "mode") %||% "empty",
      seed = input$rnd_seed,
      created_at = Sys.time()
    )
    attr(sample_payload, "curation_sample_metadata") <- list(sample_id = new_sample_id)
    written <- tryCatch(
      curation_artifact_write_sample(
        artifact_store, scope,
        sample = sample_payload,
        reviewer_id = reviewer_id, actor = sample_actor, expected_revision = 0L
      ),
      error = function(e) e
    )
    if (inherits(written, "error") || (is.list(written) && "ok" %in% names(written) && isFALSE(written$ok))) {
      message <- if (inherits(written, "error")) conditionMessage(written) else "the artifact store rejected the write"
      shiny::showNotification(paste0("Queue built but sample artifact was not saved: ", message), type = "warning", duration = 8)
    } else {
      sample(sample_payload$queue)
      sample_id(written$sample_id %||% new_sample_id)
      sample_index(1L)
      if (!is.null(written$revision)) sample_revision(as.integer(written$revision))
      shiny::showNotification(sprintf("Queued %d records (%s).", nrow(queue), attr(queue, "mode") %||% "empty"), type = "message")
    }
  })
  shiny::observeEvent(input$rnd_keep, apply_bulk("keep", "random_tbl", random_df(), "random"))
  shiny::observeEvent(input$rnd_drop, apply_bulk("drop", "random_tbl", random_df(), "random"))
  shiny::observeEvent(input$rnd_unsure, apply_bulk("unsure", "random_tbl", random_df(), "random"))
  shiny::observeEvent(input$rnd_clear, apply_bulk(NULL, "random_tbl", random_df(), "random"))

  sr_current <- shiny::reactive({
    data <- random_df()
    if (nrow(data) == 0L) {
      return(NULL)
    }
    data[max(1L, min(as.integer(sample_index()), nrow(data))), , drop = FALSE]
  })
  sr_decision <- function(row) {
    if (is.null(row)) {
      return(NA_character_)
    }
    rv <- active_reviews()
    hit <- rv$country == row$country[[1L]] & rv$document_id == row$document_id[[1L]] & rv$reviewer_id == reviewer_id
    if (!any(hit)) NA_character_ else as.character(rv$decision[which(hit)[1L]])
  }
  sr_set <- function(info) {
    if (!is.list(info) || is.null(info$decision) || is.null(info$country) ||
      is.null(info$document_id) || is.null(info$reviewer) || is.null(info$revision)) {
      shiny::showNotification(
        "Stepper write stopped because the displayed row and revision were not captured.",
        type = "warning", duration = 8
      )
      return(invisible(NULL))
    }
    decision <- as.character(info$decision[[1L]])
    if (!decision %in% c("keep", "drop", "unsure")) {
      shiny::showNotification("Stepper write stopped because the decision was invalid.", type = "warning", duration = 8)
      return(invisible(NULL))
    }
    if (!identical(as.character(info$reviewer[[1L]]), as.character(reviewer_id))) {
      shiny::showNotification("Stepper write stopped because the displayed reviewer changed.", type = "warning", duration = 8)
      return(invisible(NULL))
    }
    row <- sr_current()
    if (is.null(row)) {
      return(invisible(NULL))
    }
    if (!identical(as.character(info$country[[1L]]), as.character(row$country[[1L]])) ||
      !identical(as.character(info$document_id[[1L]]), as.character(row$document_id[[1L]]))) {
      shiny::showNotification(
        "Stepper write stopped because the displayed row changed; review the current row and try again.",
        type = "warning", duration = 8
      )
      return(invisible(NULL))
    }
    displayed_revision <- tryCatch(
      curation_app_validate_displayed_revision(info$revision),
      error = function(error) error
    )
    if (inherits(displayed_revision, "error")) {
      shiny::showNotification(
        "Stepper write stopped because the displayed revision was invalid; review the current row and try again.",
        type = "warning", duration = 8
      )
      return(invisible(NULL))
    }
    result <- safe_write_one(
      info$country[[1L]], info$document_id[[1L]], decision, "random",
      expected_revision = displayed_revision
    )
    if (is.null(result)) {
      return(invisible(NULL))
    }
    data <- random_df()
    if (nrow(data) == 0L) {
      return(invisible(NULL))
    }
    done <- curation_app_key_string(data$country, data$document_id) %in% curation_app_key_string(active_reviews()$country[active_reviews()$reviewer_id == reviewer_id], active_reviews()$document_id[active_reviews()$reviewer_id == reviewer_id])
    next_index <- which(!done & seq_len(nrow(data)) > sample_index())
    if (length(next_index)) sample_index(next_index[[1L]]) else if (any(!done)) sample_index(which(!done)[[1L]])
    invisible(result)
  }
  shiny::observeEvent(input$sr_decision, sr_set(input$sr_decision), ignoreInit = TRUE)
  shiny::observeEvent(input$sr_next, {
    data <- random_df()
    if (nrow(data) == 0L) {
      return()
    }
    index <- as.integer(sample_index())
    if (isTRUE(input$sr_skip)) {
      done <- curation_app_key_string(data$country, data$document_id) %in% curation_app_key_string(active_reviews()$country[active_reviews()$reviewer_id == reviewer_id], active_reviews()$document_id[active_reviews()$reviewer_id == reviewer_id])
      candidates <- which(!done & seq_len(nrow(data)) > index)
      sample_index(if (length(candidates)) candidates[[1L]] else min(nrow(data), index + 1L))
    } else {
      sample_index(min(nrow(data), index + 1L))
    }
  })
  shiny::observeEvent(input$sr_prev, {
    data <- random_df()
    if (nrow(data) == 0L) {
      return()
    }
    index <- as.integer(sample_index())
    if (isTRUE(input$sr_skip)) {
      done <- curation_app_key_string(data$country, data$document_id) %in% curation_app_key_string(active_reviews()$country[active_reviews()$reviewer_id == reviewer_id], active_reviews()$document_id[active_reviews()$reviewer_id == reviewer_id])
      candidates <- which(!done & seq_len(nrow(data)) < index)
      sample_index(if (length(candidates)) candidates[[length(candidates)]] else max(1L, index - 1L))
    } else {
      sample_index(max(1L, index - 1L))
    }
  })
  shiny::observeEvent(input$sr_skip, {
    if (!isTRUE(input$sr_skip)) {
      return()
    }
    data <- random_df()
    if (nrow(data) == 0L) {
      return()
    }
    done <- curation_app_key_string(data$country, data$document_id) %in% curation_app_key_string(active_reviews()$country[active_reviews()$reviewer_id == reviewer_id], active_reviews()$document_id[active_reviews()$reviewer_id == reviewer_id])
    if (any(!done)) sample_index(which(!done)[[1L]])
  })
  output$sr_pos <- shiny::renderText({
    row <- sr_current()
    if (is.null(row)) {
      return("no sample")
    }
    sprintf("Record %d of %d \u00b7 %s%s", sample_index(), nrow(random_df()), toupper(row$country[[1L]]), if (!is.na(sr_decision(row))) paste0(" \u00b7 ", toupper(sr_decision(row))) else "")
  })
  output$sr_record <- shiny::renderUI({
    curation_app_single_record_ui(sr_current(), config)
  })
  output$sr_decision <- shiny::renderUI({
    row <- sr_current()
    if (is.null(row)) {
      return(NULL)
    }
    current <- sr_decision(row)
    displayed_revision <- curation_app_current_revision(
      reviews(), row$country[[1L]], row$document_id[[1L]], reviewer_id
    )
    shiny::tagList(
      shiny::tags$input(
        id = "sr_displayed_country",
        type = "hidden",
        value = as.character(row$country[[1L]])
      ),
      shiny::tags$input(
        id = "sr_displayed_document_id",
        type = "hidden",
        value = as.character(row$document_id[[1L]])
      ),
      shiny::tags$input(
        id = "sr_displayed_reviewer",
        type = "hidden",
        value = as.character(reviewer_id)
      ),
      shiny::tags$input(
        id = "sr_displayed_revision",
        type = "hidden",
        value = as.character(displayed_revision)
      ),
      shiny::actionButton("sr_keep", "Keep", class = paste("btn-lg btn-success", if (identical(current, "keep")) "active" else "")),
      shiny::actionButton("sr_drop", "Drop", class = paste("btn-lg btn-danger", if (identical(current, "drop")) "active" else "")),
      shiny::actionButton("sr_unsure", "Unsure", class = paste("btn-lg btn-warning", if (identical(current, "unsure")) "active" else ""))
    )
  })

  output$snapshot_info <- shiny::renderUI({
    origin <- if (!is.null(snapshot_version())) {
      sprintf("artifact snapshot v%s (revision %s)", snapshot_version(), snapshot_revision())
    } else {
      "injected record provider"
    }
    shiny::helpText(sprintf("%s records loaded from the %s.", format(nrow(records()), big.mark = ","), origin))
  })
  invisible(NULL)
}

#' Create a configurable curation Shiny application.
#'
#' All corpus, review, artifact, identity, and translation boundaries are
#' supplied by the caller.  The app resolves its trusted actor at session start,
#' rechecks it before mutations, and uses an optional non-touching probe to
#' close an expired socket; display names are informational and cannot select
#' a reviewer.
#'
#' @param config A `planscanR_curation_config`.
#' @param records_provider A `planscanR_curation_records_provider`.
#' @param review_repository A `planscanR_curation_review_repository`.
#' @param artifact_store A `planscanR_curation_artifact_store` used for sample,
#'   snapshot, and model seams.
#' @param actor_resolver A trusted `planscanR_curation_actor_resolver`.
#' @param request Optional deployment request value or function accepting
#'   `session`; it is passed to the actor resolver.
#' @param actor_probe Optional non-touching actor resolver used by the periodic
#'   socket-expiry check. Mutation paths always use `actor_resolver`.
#' @param logout_path Optional local POST path rendered as a visible Log out
#'   control, for example `/app/logout`. It is absent by default.
#' @param acquisition Optional injected acquisition service. A catalog can be
#'   supplied directly, or a list with `catalog`, `available`, and optional
#'   module settings. It is absent by default.
#' @param historical_reviews Optional data frame of imported historical review
#'   linkage and attribution rows. Unresolved rows are shown read-only.
#' @param input_provenance Optional BIOGAIN or caller-supplied identity for the
#'   compatible scored input. The app adds the loaded snapshot identity and
#'   content fingerprint before training.
#' @param cohort_receipt Optional immutable historical-cohort receipt carried
#'   into the model and publication provenance.
#' @return A `shiny.appobj`.
#' @export
curation_app <- function(
  config,
  records_provider,
  review_repository,
  artifact_store,
  actor_resolver,
  request = NULL,
  actor_probe = NULL,
  logout_path = NULL,
  acquisition = NULL,
  historical_reviews = NULL,
  input_provenance = NULL,
  cohort_receipt = NULL
) {
  curation_app_require_ui()
  curation_check_config(config)
  curation_check_records_provider(records_provider)
  curation_check_review_repository(review_repository)
  curation_check_artifact_store(artifact_store)
  curation_app_check_actor_resolver(actor_resolver)
  if (!is.null(actor_probe)) curation_app_check_actor_resolver(actor_probe)
  logout_path <- curation_app_validate_logout_path(logout_path)
  context <- list(
    config = config,
    records_provider = records_provider,
    review_repository = review_repository,
    artifact_store = artifact_store,
    actor_resolver = actor_resolver,
    actor_probe = actor_probe,
    logout_path = logout_path,
    historical_reviews = historical_reviews,
    input_provenance = input_provenance,
    cohort_receipt = cohort_receipt,
    request = request,
    acquisition = curation_app_acquisition_service(acquisition),
    app_state = new.env(parent = emptyenv())
  )
  shiny::shinyApp(
    ui = curation_app_ui(config, logout_path = logout_path, acquisition = acquisition),
    server = function(input, output, session) curation_app_server(input, output, session, context)
  )
}

#' Run a configurable curation Shiny application.
#'
#' @inheritParams curation_app
#' @param launch.browser Passed to [shiny::runApp()].
#' @param ... Further arguments passed to [shiny::runApp()], such as `host` or
#'   `port`.
#' @return Invisibly, the result of [shiny::runApp()].
#' @export
run_curation <- function(
  config,
  records_provider,
  review_repository,
  artifact_store,
  actor_resolver,
  request = NULL,
  launch.browser = interactive(),
  actor_probe = NULL,
  logout_path = NULL,
  acquisition = NULL,
  historical_reviews = NULL,
  input_provenance = NULL,
  cohort_receipt = NULL,
  ...
) {
  app <- curation_app(
    config = config,
    records_provider = records_provider,
    review_repository = review_repository,
    artifact_store = artifact_store,
    actor_resolver = actor_resolver,
    actor_probe = actor_probe,
    logout_path = logout_path,
    acquisition = acquisition,
    historical_reviews = historical_reviews,
    input_provenance = input_provenance,
    cohort_receipt = cohort_receipt,
    request = request
  )
  shiny::runApp(app, launch.browser = launch.browser, ...)
}
