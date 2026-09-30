# Configuration-independent sampling, queue, and progress helpers. Persistence
# of samples is injected through curation_artifact_store() and is intentionally
# outside this file.

curation_sampling_abort <- function(message) {
  curation_abort(message, class = "planscanR_error_curation_sampling")
}

curation_validate_sampling_frame <- function(records) {
  tryCatch(
    curation_validate_record_frame(records),
    planscanR_error_bad_input = function(e) {
      curation_sampling_abort(conditionMessage(e))
    }
  )
}

curation_validate_countries <- function(countries, records) {
  if (is.null(countries)) {
    return(unique(as.character(records$country)))
  }
  if (!is.character(countries) || length(countries) == 0L || anyNA(countries) || any(!nzchar(countries))) {
    curation_sampling_abort("{.arg countries} must be NULL or a non-empty character vector.")
  }
  unique(countries)
}

curation_validate_n_per_country <- function(n_per_country) {
  if (
    !is.numeric(n_per_country) ||
      length(n_per_country) != 1L ||
      !is.finite(n_per_country) ||
      n_per_country <= 0 ||
      n_per_country != trunc(n_per_country) ||
      n_per_country > .Machine$integer.max
  ) {
    curation_sampling_abort("{.arg n_per_country} must be one positive integer.")
  }
  as.integer(n_per_country)
}

curation_validate_seed <- function(seed) {
  if (is.null(seed) || (length(seed) == 1L && is.na(seed))) {
    return(NULL)
  }
  if (
    !is.numeric(seed) ||
      length(seed) != 1L ||
      !is.finite(seed) ||
      seed != trunc(seed) ||
      seed < -.Machine$integer.max ||
      seed > .Machine$integer.max
  ) {
    curation_sampling_abort("{.arg seed} must be NULL, NA, or one finite integer.")
  }
  as.integer(seed)
}

curation_with_restored_rng <- function(seed, fn) {
  seed <- curation_validate_seed(seed)
  # An omitted seed follows ordinary R sampling semantics and advances the
  # caller's RNG stream. An explicit seed is an isolated deterministic draw;
  # restore both the state and RNG kind even when the callback errors.
  if (is.null(seed)) {
    return(fn())
  }
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv, inherits = FALSE) else NULL
  old_kind <- RNGkind()
  on.exit({
    do.call("RNGkind", as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  fn()
}

curation_document_key <- function(country, document_id) {
  paste(as.character(country), as.character(document_id), sep = "\r")
}

curation_review_columns <- function(reviews) {
  if (is.null(reviews)) {
    return(tibble::tibble(
      country = character(0),
      document_id = character(0),
      reviewer_id = character(0),
      deleted = logical(0)
    ))
  }
  if (!is.data.frame(reviews)) {
    curation_sampling_abort("{.arg reviews} must be NULL or a data frame.")
  }
  required <- c("country", "document_id")
  if (!all(required %in% names(reviews))) {
    curation_sampling_abort("{.arg reviews} must include country and document_id columns.")
  }
  reviewer_column <- if ("reviewer_id" %in% names(reviews)) {
    "reviewer_id"
  } else if ("reviewer" %in% names(reviews)) {
    "reviewer"
  } else {
    curation_sampling_abort("{.arg reviews} must include reviewer_id (or legacy reviewer).")
  }
  out <- tibble::tibble(
    country = as.character(reviews$country),
    document_id = as.character(reviews$document_id),
    reviewer_id = as.character(reviews[[reviewer_column]]),
    deleted = if ("deleted" %in% names(reviews)) reviews$deleted else rep(FALSE, nrow(reviews))
  )
  if (!is.logical(out$deleted) || anyNA(out$deleted)) {
    curation_sampling_abort("Review deleted values must be non-missing logical values.")
  }
  if (anyNA(out$country) || anyNA(out$document_id) || anyNA(out$reviewer_id)) {
    curation_sampling_abort("Review country, document_id, and reviewer_id cannot be missing.")
  }
  out[!out$deleted, , drop = FALSE]
}

curation_filter_review_scope <- function(reviews, scope) {
  if (!is.null(scope) && !inherits(scope, "planscanR_curation_scope")) {
    curation_sampling_abort("{.arg scope} must be NULL or a planscanR_curation_scope.")
  }
  if (is.null(scope) || nrow(reviews) == 0L) {
    return(reviews)
  }
  # A repository list callback is already scoped even when it omits scope
  # columns. If present, however, both explicit columns are authoritative.
  keep <- rep(TRUE, nrow(reviews))
  if ("project_id" %in% names(reviews)) {
    keep <- keep & as.character(reviews$project_id) == scope$project_id
  }
  if ("corpus_id" %in% names(reviews)) {
    keep <- keep & as.character(reviews$corpus_id) == scope$corpus_id
  }
  reviews[keep, , drop = FALSE]
}

curation_review_scope_rows <- function(reviews, scope) {
  normalized <- curation_review_columns(reviews)
  # Preserve optional scope columns while normalizing reviewer/deleted values.
  if (!is.null(reviews) && is.data.frame(reviews)) {
    active <- if ("deleted" %in% names(reviews)) {
      deleted <- reviews$deleted
      if (!is.logical(deleted) || anyNA(deleted)) {
        curation_sampling_abort("Review deleted values must be non-missing logical values.")
      }
      !deleted
    } else {
      rep(TRUE, nrow(reviews))
    }
    extras <- reviews[active, setdiff(names(reviews), names(normalized)), drop = FALSE]
    normalized <- cbind(normalized, extras)
  }
  curation_filter_review_scope(normalized, scope)
}

curation_draw_random_sample_impl <- function(records, countries, n_per_country) {
  parts <- lapply(countries, function(country) {
    pool <- records[records$country == country, , drop = FALSE]
    if (nrow(pool) == 0L) {
      return(NULL)
    }
    n_take <- min(n_per_country, nrow(pool))
    index <- sample.int(nrow(pool), n_take)
    tibble::tibble(
      document_id = as.character(pool$document_id[index]),
      country = as.character(pool$country[index])
    )
  })
  out <- dplyr::bind_rows(parts)
  if (nrow(out) == 0L) {
    return(tibble::tibble(document_id = character(0), country = character(0)))
  }
  out[sample.int(nrow(out)), , drop = FALSE]
}

#' Draw a reproducible, country-stratified random sample.
#'
#' The sample is drawn from every supplied country before it is interleaved.
#' `country` remains part of every identity, so native document IDs reused by
#' two countries stay distinct. When an explicit `seed` is supplied, the
#' caller's RNG state is restored on success and error, including when
#' `.Random.seed` did not exist before the call.
#'
#' @param records A frame with `country` and `document_id` columns.
#' @param countries Countries to sample, in the desired stratum order. `NULL`
#'   uses the order first observed in `records`.
#' @param n_per_country Positive number of rows per country.
#' @param seed Optional integer seed. `NA` behaves like `NULL`; with `NULL`,
#'   sampling advances the caller's RNG stream.
#' @param n Legacy alias for `n_per_country`.
#' @return A tibble with `document_id` and `country` columns.
#' @export
curation_draw_random_sample <- function(
  records,
  countries = NULL,
  n_per_country = NULL,
  seed = NULL,
  n = NULL
) {
  records <- curation_validate_sampling_frame(records)
  if (!is.null(n)) {
    if (!is.null(n_per_country)) {
      curation_sampling_abort("Supply only one of {.arg n_per_country} and {.arg n}.")
    }
    n_per_country <- n
  }
  if (is.null(n_per_country)) {
    curation_sampling_abort("{.arg n_per_country} is required.")
  }
  n_per_country <- curation_validate_n_per_country(n_per_country)
  countries <- curation_validate_countries(countries, records)
  curation_with_restored_rng(seed, function() {
    curation_draw_random_sample_impl(records, countries, n_per_country)
  })
}

#' Build a reviewer-aware validation or fresh-review queue.
#'
#' Rows reviewed by another reviewer in the supplied project/corpus scope are
#' returned first with `mode = "validate"`. Once those are exhausted, a fresh
#' country-stratified sample of never-reviewed rows is returned with
#' `mode = "fresh"`; no available rows returns `mode = "empty"`. The input
#' review frame may use canonical `reviewer_id` or the legacy `reviewer` name.
#'
#' @param records A frame with `country` and `document_id` columns.
#' @param reviews `NULL` or review rows. Rows with `deleted = TRUE` are ignored.
#' @param reviewer_id Stable reviewer identity. `reviewer` is accepted as a
#'   legacy alias.
#' @param countries Optional country filter.
#' @param n_per_country Positive fresh-sample size per country.
#' @param seed Optional integer seed. `NA` behaves like `NULL`; with an
#'   explicit seed, the caller's RNG state is restored.
#' @param scope Optional curation scope used to filter explicit project/corpus
#'   columns in `reviews`.
#' @param reviewer Legacy alias for `reviewer_id`.
#' @return A tibble with `document_id` and `country` columns and a `mode`
#'   attribute.
#' @export
curation_build_review_queue <- function(
  records,
  reviews,
  reviewer_id = NULL,
  countries = NULL,
  n_per_country = 1L,
  seed = NULL,
  scope = NULL,
  reviewer = NULL
) {
  records <- curation_validate_sampling_frame(records)
  if (!is.null(reviewer)) {
    if (!is.null(reviewer_id)) {
      curation_sampling_abort("Supply only one of {.arg reviewer_id} and {.arg reviewer}.")
    }
    reviewer_id <- reviewer
  }
  if (!is.character(reviewer_id) || length(reviewer_id) != 1L || is.na(reviewer_id) || !nzchar(trimws(reviewer_id))) {
    curation_sampling_abort("{.arg reviewer_id} must be one non-empty character string.")
  }
  reviewer_id <- trimws(reviewer_id)
  n_per_country <- curation_validate_n_per_country(n_per_country)
  countries <- curation_validate_countries(countries, records)
  review_rows <- curation_review_scope_rows(reviews, scope)
  curation_with_restored_rng(seed, function() {
    empty <- tibble::tibble(document_id = character(0), country = character(0))
    snap <- records[records$country %in% countries, , drop = FALSE]
    if (nrow(snap) == 0L) {
      attr(empty, "mode") <- "empty"
      return(empty)
    }
    snap_keys <- curation_document_key(snap$country, snap$document_id)
    reviewed_any <- unique(curation_document_key(review_rows$country, review_rows$document_id))
    mine <- review_rows$reviewer_id == reviewer_id
    reviewed_me <- unique(curation_document_key(review_rows$country[mine], review_rows$document_id[mine]))
    validate_keys <- setdiff(reviewed_any, reviewed_me)
    pick <- snap_keys %in% validate_keys
    if (any(pick)) {
      out <- tibble::tibble(
        document_id = as.character(snap$document_id[pick]),
        country = as.character(snap$country[pick])
      )
      out <- out[sample.int(nrow(out)), , drop = FALSE]
      attr(out, "mode") <- "validate"
      attr(out, "reviewer_id") <- reviewer_id
      if (!is.null(scope)) attr(out, "scope") <- scope
      return(out)
    }
    fresh <- snap[!(snap_keys %in% reviewed_any), , drop = FALSE]
    if (nrow(fresh) == 0L) {
      attr(empty, "mode") <- "empty"
      attr(empty, "reviewer_id") <- reviewer_id
      if (!is.null(scope)) attr(empty, "scope") <- scope
      return(empty)
    }
    out <- curation_draw_random_sample_impl(fresh, countries, n_per_country)
    attr(out, "mode") <- if (nrow(out) > 0L) "fresh" else "empty"
    attr(out, "reviewer_id") <- reviewer_id
    if (!is.null(scope)) attr(out, "scope") <- scope
    out
  })
}

#' Summarise country-level review progress.
#'
#' Counts are based on distinct `(country, document_id)` identities. `labelled`
#' includes any active decision, while `keep`, `drop`, and `unsure` count the
#' corresponding decisions. `yours` restricts the labelled count to one
#' reviewer. Explicit project/corpus columns are filtered by `scope`.
#'
#' @param records Corpus records with `country` and `document_id`.
#' @param reviews `NULL` or review rows.
#' @param countries Countries to include, in output order.
#' @param reviewer_id Optional reviewer identity for the `yours` column.
#' @param scope Optional curation scope.
#' @param reviewer Legacy alias for `reviewer_id`.
#' @return A tibble with `country`, `records`, `labelled`, `pct`, `keep`,
#'   `drop`, `unsure`, and `yours`.
#' @export
curation_country_label_counts <- function(
  records,
  reviews,
  countries = NULL,
  reviewer_id = NULL,
  scope = NULL,
  reviewer = NULL
) {
  records <- curation_validate_sampling_frame(records)
  if (!is.null(reviewer)) {
    if (!is.null(reviewer_id)) {
      curation_sampling_abort("Supply only one of {.arg reviewer_id} and {.arg reviewer}.")
    }
    reviewer_id <- reviewer
  }
  if (!is.null(reviewer_id) && (!is.character(reviewer_id) || length(reviewer_id) != 1L || is.na(reviewer_id) || !nzchar(trimws(reviewer_id)))) {
    curation_sampling_abort("{.arg reviewer_id} must be NULL or one non-empty character string.")
  }
  if (!is.null(reviewer_id)) reviewer_id <- trimws(reviewer_id)
  countries <- curation_validate_countries(countries, records)
  review_rows <- curation_review_scope_rows(reviews, scope)
  record_count <- unname(vapply(countries, function(country) sum(records$country == country), integer(1)))
  key_count <- function(rows, country, decision = NULL, reviewer = NULL) {
    if (nrow(rows) == 0L) return(0L)
    keep <- rows$country == country
    if (!is.null(decision) && "decision" %in% names(rows)) keep <- keep & rows$decision == decision
    if (!is.null(reviewer)) keep <- keep & rows$reviewer_id == reviewer
    if (!any(keep)) return(0L)
    length(unique(curation_document_key(rows$country[keep], rows$document_id[keep])))
  }
  labelled <- unname(vapply(countries, function(country) key_count(review_rows, country), integer(1)))
  keep <- unname(vapply(countries, function(country) key_count(review_rows, country, "keep"), integer(1)))
  drop <- unname(vapply(countries, function(country) key_count(review_rows, country, "drop"), integer(1)))
  unsure <- unname(vapply(countries, function(country) key_count(review_rows, country, "unsure"), integer(1)))
  yours <- if (is.null(reviewer_id)) {
    rep(0L, length(countries))
  } else {
    unname(vapply(countries, function(country) key_count(review_rows, country, reviewer = reviewer_id), integer(1)))
  }
  tibble::tibble(
    country = countries,
    records = record_count,
    labelled = labelled,
    pct = ifelse(record_count > 0L, 100 * labelled / record_count, NA_real_),
    keep = keep,
    drop = drop,
    unsure = unsure,
    yours = yours
  )
}

# A descriptive alias used by dashboard callers.
#' @rdname curation_country_label_counts
#' @export
curation_review_progress <- curation_country_label_counts
