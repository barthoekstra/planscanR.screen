# Record-provider contracts. Providers are injected into a curation context so
# record access is explicit and testable. The cache adapter uses only exported
# planscanR I/O and never resolves a cache root from process state.

curation_validate_record_frame <- function(records) {
  if (!is.data.frame(records)) {
    curation_abort("A record provider must return a data frame.", "planscanR_error_bad_input")
  }
  required <- c("country", "document_id")
  if (!all(required %in% names(records))) {
    curation_abort(
      "Record providers must return {.field country} and {.field document_id} columns.",
      class = "planscanR_error_bad_input"
    )
  }
  for (nm in required) {
    if (anyNA(records[[nm]]) || any(!nzchar(as.character(records[[nm]])))) {
      curation_abort(
        paste0("Record key column {.field ", nm, "} cannot contain missing or empty values."),
        class = "planscanR_error_bad_input"
      )
    }
  }
  records
}

curation_cosine_max <- function(records, topics) {
  columns <- paste0("relevance_score_", names(topics))
  columns <- columns[columns %in% names(records)]
  if (length(columns) == 0L) {
    return(rep(NA_real_, nrow(records)))
  }
  values <- lapply(columns, function(column) {
    value <- suppressWarnings(as.numeric(records[[column]]))
    value[is.na(value)] <- -Inf
    value
  })
  maximum <- do.call(pmax, values)
  maximum[!is.finite(maximum)] <- NA_real_
  maximum
}

curation_count_list_values <- function(values, kind = c("attachments", "downloads")) {
  kind <- match.arg(kind)
  if (kind == "attachments") {
    return(vapply(values, function(value) {
      if (is.null(value)) {
        return(0L)
      }
      value <- as.character(value)
      as.integer(sum(!is.na(value) & nzchar(value)))
    }, integer(1)))
  }
  vapply(values, function(value) {
    if (is.null(value) || !is.data.frame(value) || nrow(value) == 0L) {
      return(0L)
    }
    if (!"status" %in% names(value)) {
      return(0L)
    }
    as.integer(sum(as.character(value$status) %in% c("downloaded", "cached")))
  }, integer(1))
}

curation_existing_count <- function(records, column, fallback) {
  if (!column %in% names(records)) {
    return(fallback)
  }
  value <- suppressWarnings(as.numeric(records[[column]]))
  value[!is.finite(value) | value < 0] <- 0
  as.integer(value)
}

curation_drop_derived_columns <- function(records, prefix, keep) {
  old <- grep(prefix, names(records), value = TRUE)
  drop <- setdiff(old, keep)
  if (length(drop) > 0L) {
    records[drop] <- NULL
  }
  records
}

#' Enrich a corpus frame with the scalar curation signals.
#'
#' `config` is the only source of topic and keyword policy. Existing derived
#' keyword columns are removed before scoring, so re-enriching a frame with a
#' new configuration cannot retain a previous lexicon's totals. Raw sidecar
#' scores for the configured topic names are reused; this function does not
#' invoke an embedding or classifier backend.
#'
#' @param records A data frame with non-empty `country` and `document_id`
#'   columns. It may contain planscanR list columns such as `attachment_urls`
#'   and `download_status`.
#' @param config A `planscanR_curation_config` supplying `topics` and `lexicon`.
#' @param drop_list_columns If `TRUE`, remove UI-unfriendly attachment,
#'   download, discovery, and local-path list columns after deriving counts.
#' @return The enriched input frame with `cosine_max`, keyword scores,
#'   attachment/download counts, and classifier fallback columns.
#' @export
curation_enrich_records <- function(records, config, drop_list_columns = TRUE) {
  curation_check_config(config)
  records <- curation_validate_record_frame(records)
  if (!is.logical(drop_list_columns) || length(drop_list_columns) != 1L || is.na(drop_list_columns)) {
    curation_abort(
      "{.arg drop_list_columns} must be one non-missing logical value.",
      class = "planscanR_error_curation_records"
    )
  }

  # Only configured relevance columns participate in the derived maximum. A
  # stale column from a previous configuration is not retained in the output.
  keep_relevance <- paste0("relevance_score_", names(config$topics))
  records <- curation_drop_derived_columns(
    records,
    "^relevance_score_",
    keep_relevance
  )
  records$cosine_max <- curation_cosine_max(records, config$topics)

  # Keyword values are always recomputed from the explicit configuration.
  records <- curation_drop_derived_columns(records, "^kw_", character(0L))
  records <- score_keywords(records, lexicon = config$lexicon)

  attachment_count <- if ("attachment_urls" %in% names(records)) {
    curation_count_list_values(records$attachment_urls, "attachments")
  } else {
    curation_existing_count(records, "n_attachments", rep(0L, nrow(records)))
  }
  download_count <- if ("download_status" %in% names(records)) {
    curation_count_list_values(records$download_status, "downloads")
  } else {
    curation_existing_count(records, "n_downloaded", rep(0L, nrow(records)))
  }
  records$n_attachments <- attachment_count
  records$has_attachments <- attachment_count > 0L
  records$n_downloaded <- download_count
  records$has_downloaded <- download_count > 0L

  if (!"class_relevant" %in% names(records)) {
    records$class_relevant <- rep(NA, nrow(records))
  }
  if (!"class_label" %in% names(records)) {
    records$class_label <- rep(NA_character_, nrow(records))
  }
  if (!"class_score" %in% names(records)) {
    records$class_score <- rep(NA_real_, nrow(records))
  }

  if (isTRUE(drop_list_columns)) {
    list_columns <- names(records)[vapply(records, is.list, logical(1))]
    drop <- intersect(
      list_columns,
      c("attachment_urls", "local_path", "download_status", "discovery_log")
    )
    drop <- union(drop, grep("^(attachment_urls_|local_path_)", names(records), value = TRUE))
    if (length(drop) > 0L) {
      records[drop] <- NULL
    }
  }
  records
}

#' Build an injected curation record provider.
#'
#' @param read_fn Function accepting `corpus_id` and optional `countries`, and
#'   returning a record data frame.
#' @param refresh_fn Optional function with the same signature. It can rebuild a
#'   derived snapshot; persistence remains owned by a later artifact service.
#' @return A `planscanR_curation_records_provider` object.
#' @export
curation_records_provider <- function(read_fn, refresh_fn = NULL) {
  if (!curation_function_accepts(read_fn, c("corpus_id", "countries"))) {
    curation_abort(
      "{.arg read_fn} must accept {.arg corpus_id} and {.arg countries}.",
      class = "planscanR_error_curation_records"
    )
  }
  if (!is.null(refresh_fn) && !curation_function_accepts(refresh_fn, c("corpus_id", "countries"))) {
    curation_abort(
      "{.arg refresh_fn} must accept {.arg corpus_id} and {.arg countries}.",
      class = "planscanR_error_curation_records"
    )
  }
  structure(
    list(read_fn = read_fn, refresh_fn = refresh_fn),
    class = "planscanR_curation_records_provider"
  )
}

#' Build a planscanR cache-backed record provider.
#'
#' @param cache_dir Explicit planscanR cache root.
#' @param index_fn Injectable equivalent of [planscanR::index_cache()] for
#'   offline fixtures. It must accept `cache_dir` and `country`.
#' @return A `planscanR_curation_records_provider` object.
#' @export
curation_records_provider_cache <- function(cache_dir, index_fn = planscanR::index_cache) {
  cache_dir <- curation_scalar_character(
    cache_dir,
    "cache_dir",
    class = "planscanR_error_curation_records"
  )
  if (!curation_function_accepts(index_fn, c("cache_dir", "country"))) {
    curation_abort(
      "{.arg index_fn} must accept {.arg cache_dir} and {.arg country}.",
      class = "planscanR_error_curation_records"
    )
  }
  read_fn <- function(corpus_id, countries = NULL) {
    curation_scalar_character(
      corpus_id,
      "corpus_id",
      class = "planscanR_error_curation_records"
    )
    if (is.null(countries)) {
      return(index_fn(cache_dir = cache_dir, country = NULL))
    }
    if (!is.character(countries) || anyNA(countries) || any(!nzchar(countries))) {
      curation_abort(
        "{.arg countries} must be NULL or a non-empty character vector.",
        class = "planscanR_error_curation_records"
      )
    }
    parts <- lapply(countries, function(country) {
      index_fn(cache_dir = cache_dir, country = country)
    })
    parts <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, parts)
    if (length(parts) == 0L) {
      return(tibble::tibble(
        country = character(0),
        document_id = character(0)
      ))
    }
    dplyr::bind_rows(parts)
  }
  curation_records_provider(read_fn = read_fn, refresh_fn = read_fn)
}

curation_check_records_provider <- function(provider) {
  if (!inherits(provider, "planscanR_curation_records_provider")) {
    curation_abort(
      "{.arg provider} must be a planscanR_curation_records_provider.",
      class = "planscanR_error_curation_records"
    )
  }
  invisible(provider)
}

#' Read records from an injected provider.
#'
#' @param provider A `planscanR_curation_records_provider`.
#' @param corpus_id Explicit corpus identity.
#' @param countries Optional country filter.
#' @return A validated record data frame.
#' @export
curation_records_read <- function(provider, corpus_id, countries = NULL) {
  curation_check_records_provider(provider)
  corpus_id <- curation_scalar_character(
    corpus_id,
    "corpus_id",
    class = "planscanR_error_curation_records"
  )
  out <- provider$read_fn(corpus_id = corpus_id, countries = countries)
  curation_validate_record_frame(out)
}

#' Refresh records through an injected provider.
#'
#' @param provider A `planscanR_curation_records_provider`.
#' @param corpus_id Explicit corpus identity.
#' @param countries Optional country filter.
#' @return A validated record data frame.
#' @export
curation_records_refresh <- function(provider, corpus_id, countries = NULL) {
  curation_check_records_provider(provider)
  if (is.null(provider$refresh_fn)) {
    curation_abort(
      "This record provider does not support refresh.",
      class = "planscanR_error_curation_records"
    )
  }
  corpus_id <- curation_scalar_character(
    corpus_id,
    "corpus_id",
    class = "planscanR_error_curation_records"
  )
  out <- provider$refresh_fn(corpus_id = corpus_id, countries = countries)
  curation_validate_record_frame(out)
}

#' Read and enrich a corpus snapshot through an injected provider.
#'
#' This is a pure service boundary: it performs no RDS or cache writes. C3 can
#' persist the returned value through [curation_artifact_store()] or a test
#' double, while a changed configuration always rebuilds the derived columns.
#'
#' @param provider A `planscanR_curation_records_provider`.
#' @param config A `planscanR_curation_config` whose corpus identity is used.
#' @param countries Optional country filter.
#' @param refresh If `TRUE`, call the provider's refresh callback.
#' @return An enriched corpus data frame.
#' @export
curation_records_snapshot <- function(provider, config, countries = NULL, refresh = FALSE) {
  curation_check_records_provider(provider)
  curation_check_config(config)
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
    curation_abort(
      "{.arg refresh} must be one non-missing logical value.",
      class = "planscanR_error_curation_records"
    )
  }
  records <- if (isTRUE(refresh)) {
    curation_records_refresh(provider, config$corpus_id, countries = countries)
  } else {
    curation_records_read(provider, config$corpus_id, countries = countries)
  }
  curation_enrich_records(records, config)
}

# Compatibility name for callers that describe the operation as building a
# snapshot. The operation remains pure and configuration driven.
#' @rdname curation_records_snapshot
#' @export
curation_build_snapshot <- curation_records_snapshot
