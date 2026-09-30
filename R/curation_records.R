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
