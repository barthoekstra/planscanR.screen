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

curation_scored_feature_fingerprint <- function(records, feature_names) {
  feature_names <- unique(as.character(feature_names %||% character(0)))
  required <- c("country", "document_id", feature_names)
  if (!is.data.frame(records) || !all(required %in% names(records))) {
    return(NULL)
  }
  value <- as.data.frame(
    records[c("country", "document_id", feature_names)],
    stringsAsFactors = FALSE
  )
  attributes(value) <- list(
    names = names(value),
    row.names = seq_len(nrow(value)),
    class = "data.frame"
  )
  curation_artifact_content_fingerprint(value)
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
#' @param preserve_scored_features If `TRUE`, require the configured scored
#'   feature columns before enrichment and restore their exact values,
#'   including `NA`, after enrichment. This is the opt-in path for a verified
#'   scored-input context; the default retains ordinary keyword recomputation.
#' @return The enriched input frame with `cosine_max`, keyword scores,
#'   attachment/download counts, and classifier fallback columns.
#' @export
curation_enrich_records <- function(
  records,
  config,
  drop_list_columns = TRUE,
  preserve_scored_features = FALSE
) {
  curation_check_config(config)
  records <- curation_validate_record_frame(records)
  if (!is.logical(drop_list_columns) || length(drop_list_columns) != 1L || is.na(drop_list_columns)) {
    curation_abort(
      "{.arg drop_list_columns} must be one non-missing logical value.",
      class = "planscanR_error_curation_records"
    )
  }
  if (!is.logical(preserve_scored_features) ||
    length(preserve_scored_features) != 1L || is.na(preserve_scored_features)) {
    curation_abort(
      "{.arg preserve_scored_features} must be one non-missing logical value.",
      class = "planscanR_error_curation_records"
    )
  }
  feature_names <- selection_feature_names(config$topics, config$labels)
  scored_features <- NULL
  if (isTRUE(preserve_scored_features)) {
    missing_features <- setdiff(feature_names, names(records))
    if (length(missing_features) > 0L) {
      curation_abort(
        paste0(
          "The verified scored input is missing configured feature",
          if (length(missing_features) == 1L) "" else "s",
          ": ", paste(missing_features, collapse = ", "), "."
        ),
        class = "planscanR_error_curation_records"
      )
    }
    non_numeric <- feature_names[!vapply(records[feature_names], is.numeric, logical(1))]
    if (length(non_numeric) > 0L) {
      curation_abort(
        paste0(
          "The verified scored input has non-numeric configured feature",
          if (length(non_numeric) == 1L) "" else "s",
          ": ", paste(non_numeric, collapse = ", "), "."
        ),
        class = "planscanR_error_curation_records"
      )
    }
    scored_features <- records[feature_names]
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
  if (isTRUE(preserve_scored_features)) {
    for (feature in feature_names) {
      records[[feature]] <- scored_features[[feature]]
    }
  }

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
  # Recreate these derived columns in canonical order. A cache projection may
  # carry the download count after discarding its list status column; removing
  # stale instances first keeps its enriched frame byte-for-byte equivalent to
  # enrichment from the unprojected sidecar row.
  records[c("n_attachments", "has_attachments", "n_downloaded", "has_downloaded")] <- NULL
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
#' When the default [planscanR::index_cache()] is used, the provider passes the
#' bounded row-transform seam through to the leaf package. Its read callback
#' also retains a versioned fingerprint of every successfully decoded complete
#' row for [curation_records_snapshot()] provenance metadata.
#'
#' @param cache_dir Explicit planscanR cache root.
#' @param index_fn Injectable equivalent of [planscanR::index_cache()] for
#'   offline fixtures. It must accept `cache_dir` and `country`.
#' @param batch_size Number of cache rows combined per leaf index batch. The
#'   default is 512 and is passed to the leaf row-transform seam when the
#'   default [planscanR::index_cache()] is used.
#' @return A `planscanR_curation_records_provider` object.
#' @export
curation_records_provider_cache <- function(
  cache_dir,
  index_fn = planscanR::index_cache,
  batch_size = 512L
) {
  cache_dir <- curation_scalar_character(
    cache_dir,
    "cache_dir",
    class = "planscanR_error_curation_records"
  )
  if (!is.numeric(batch_size) || length(batch_size) != 1L ||
      is.na(batch_size) || !is.finite(batch_size) ||
      batch_size < 1 || batch_size != trunc(batch_size) ||
      batch_size > .Machine$integer.max) {
    curation_abort(
      "{.arg batch_size} must be one positive integer.",
      class = "planscanR_error_curation_records"
    )
  }
  batch_size <- as.integer(batch_size)
  if (!curation_function_accepts(index_fn, c("cache_dir", "country"))) {
    curation_abort(
      "{.arg index_fn} must accept {.arg cache_dir} and {.arg country}.",
      class = "planscanR_error_curation_records"
    )
  }
  leaf_index <- identical(index_fn, planscanR::index_cache)
  if (leaf_index) {
    leaf_formals <- names(formals(index_fn))
    if (!all(c("row_transform", "batch_size") %in% leaf_formals) && !"..." %in% leaf_formals) {
      curation_abort(
        "The installed planscanR index_cache() does not provide the bounded row-transform seam.",
        class = "planscanR_error_curation_records"
      )
    }
  }
  read_fn <- function(corpus_id, countries = NULL) {
    curation_scalar_character(
      corpus_id,
      "corpus_id",
      class = "planscanR_error_curation_records"
    )
    manifest <- new.env(parent = emptyenv())
    # Keep one binding per decoded row so a large cache does not repeatedly
    # copy and grow a single manifest list while the leaf is reading batches.
    manifest$rows <- new.env(parent = emptyenv())
    manifest$count <- 0L
    row_transform <- if (leaf_index) {
      function(record) {
        country <- as.character(record$country[[1L]])
        document_id <- as.character(record$document_id[[1L]])
        manifest$count <- manifest$count + 1L
        assign(as.character(manifest$count), list(
          country = country,
          document_id = document_id,
          digest = curation_artifact_content_fingerprint(record)
        ), envir = manifest$rows)
        if ("download_status" %in% names(record)) {
          record$n_downloaded <- curation_count_list_values(
            record$download_status,
            "downloads"
          )
        }
        drop <- intersect(
          names(record)[vapply(record, is.list, logical(1))],
          c("download_status", "discovery_log")
        )
        drop <- union(drop, grep("^(attachment_urls_|local_path_)", names(record), value = TRUE))
        if (length(drop) > 0L) {
          record[drop] <- NULL
        }
        record
      }
    } else {
      NULL
    }
    index <- function(country) {
      if (!leaf_index) {
        return(index_fn(cache_dir = cache_dir, country = country))
      }
      index_fn(
        cache_dir = cache_dir,
        country = country,
        row_transform = row_transform,
        batch_size = batch_size
      )
    }
    attach_manifest <- function(records) {
      records <- force(records)
      if (!leaf_index) {
        return(records)
      }
      entries <- if (manifest$count == 0L) {
        list()
      } else {
        unname(mget(
          as.character(seq_len(manifest$count)),
          envir = manifest$rows,
          inherits = FALSE
        ))
      }
      manifest_rows <- if (length(entries) == 0L) {
        data.frame(
          country = character(0),
          document_id = character(0),
          digest = character(0),
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          country = vapply(entries, `[[`, character(1), "country"),
          document_id = vapply(entries, `[[`, character(1), "document_id"),
          digest = vapply(entries, `[[`, character(1), "digest"),
          stringsAsFactors = FALSE
        )
      }
      if (nrow(manifest_rows) > 0L) {
        manifest_rows <- manifest_rows[order(
          manifest_rows$country,
          manifest_rows$document_id,
          manifest_rows$digest,
          method = "radix"
        ), , drop = FALSE]
        rownames(manifest_rows) <- NULL
      }
      fingerprint <- paste0(
        "decoded-row-manifest-v1:",
        curation_artifact_hash(list(
          schema = "planscanR.decoded-row-manifest.v1",
          rows = manifest_rows
        ))
      )
      attr(records, "curation_source_fingerprint") <- fingerprint
      records
    }
    if (is.null(countries)) {
      return(attach_manifest(index(NULL)))
    }
    if (!is.character(countries) || anyNA(countries) || any(!nzchar(countries))) {
      curation_abort(
        "{.arg countries} must be NULL or a non-empty character vector.",
        class = "planscanR_error_curation_records"
      )
    }
    parts <- lapply(countries, index)
    parts <- Filter(function(x) is.data.frame(x) && nrow(x) > 0L, parts)
    if (length(parts) == 0L) {
      return(attach_manifest(tibble::tibble(
        country = character(0),
        document_id = character(0)
      )))
    }
    attach_manifest(dplyr::bind_rows(parts))
  }
  provider <- curation_records_provider(read_fn = read_fn, refresh_fn = read_fn)
  if (leaf_index) {
    attr(provider, "curation_source_manifest_schema") <-
      "decoded-row-manifest-v1"
  }
  provider
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
#' With the default cache provider, `attr(snapshot, "curation_snapshot_metadata")$corpus_fingerprint`
#' is tagged `decoded-row-manifest-v1:<hash>`. The versioned manifest hashes
#' every successfully decoded complete planscanR row before projection, records
#' each row's `country`, `document_id`, and complete-row digest, sorts those
#' entries canonically, and hashes the serialized manifest. The `v1` tag names
#' this algorithm so later manifest changes can be distinguished explicitly.
#'
#' @param provider A `planscanR_curation_records_provider`.
#' @param config A `planscanR_curation_config` whose corpus identity is used.
#' @param countries Optional country filter.
#' @param refresh If `TRUE`, call the provider's refresh callback.
#' @param preserve_scored_features If `TRUE`, pass the verified scored-input
#'   preservation option through to [curation_enrich_records()].
#' @return An enriched corpus data frame.
#' @export
curation_records_snapshot <- function(
  provider,
  config,
  countries = NULL,
  refresh = FALSE,
  preserve_scored_features = FALSE
) {
  curation_check_records_provider(provider)
  curation_check_config(config)
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
    curation_abort(
      "{.arg refresh} must be one non-missing logical value.",
      class = "planscanR_error_curation_records"
    )
  }
  if (!is.logical(preserve_scored_features) ||
    length(preserve_scored_features) != 1L || is.na(preserve_scored_features)) {
    curation_abort(
      "{.arg preserve_scored_features} must be one non-missing logical value.",
      class = "planscanR_error_curation_records"
    )
  }
  records <- if (isTRUE(refresh)) {
    curation_records_refresh(provider, config$corpus_id, countries = countries)
  } else {
    curation_records_read(provider, config$corpus_id, countries = countries)
  }
  manifest_schema <- attr(provider, "curation_source_manifest_schema", exact = TRUE)
  if (identical(manifest_schema, "decoded-row-manifest-v1")) {
    raw_fingerprint <- attr(records, "curation_source_fingerprint", exact = TRUE)
    if (!is.character(raw_fingerprint) || length(raw_fingerprint) != 1L ||
        is.na(raw_fingerprint) ||
        !grepl("^decoded-row-manifest-v1:[[:xdigit:]]{32}$", raw_fingerprint)) {
      curation_abort(
        "The cache provider did not return a valid decoded-row manifest fingerprint.",
        class = "planscanR_error_curation_records"
      )
    }
    attr(records, "curation_source_fingerprint") <- NULL
  } else {
    raw_fingerprint <- curation_artifact_content_fingerprint(records)
  }
  enriched <- curation_enrich_records(
    records,
    config,
    preserve_scored_features = preserve_scored_features
  )
  metadata <- list(
    corpus_fingerprint = raw_fingerprint,
    config_fingerprint = curation_config_fingerprint(config),
    enrichment_schema = curation_artifact_enrichment_schema,
    filter_coverage = curation_artifact_filter_coverage(records, countries)
  )
  if (isTRUE(preserve_scored_features)) {
    feature_names <- selection_feature_names(config$topics, config$labels)
    metadata$preserve_scored_features <- TRUE
    metadata$scored_feature_names <- feature_names
    metadata$scored_feature_fingerprint <- curation_scored_feature_fingerprint(
      enriched,
      feature_names
    )
    metadata$prepared_record_fingerprint <- curation_artifact_content_fingerprint(enriched)
  }
  attr(enriched, "curation_snapshot_metadata") <- metadata
  enriched
}

# Compatibility name for callers that describe the operation as building a
# snapshot. The operation remains pure and configuration driven.
#' @rdname curation_records_snapshot
#' @export
curation_build_snapshot <- curation_records_snapshot
