# Optional translation boundary. A deployment supplies the backend and cache
# policy; this package never imports a translation runtime or mutates Python
# process state.

curation_translation_disabled_result <- function(record, service = NA_character_) {
  title <- if ("title" %in% names(record)) as.character(record$title[[1L]]) else NA_character_
  summary <- if ("summary" %in% names(record)) as.character(record$summary[[1L]]) else NA_character_
  list(
    status = "disabled",
    service = service,
    title = title,
    summary = summary,
    title_en = NA_character_,
    summary_en = NA_character_
  )
}

curation_translation_record <- function(record) {
  if (is.data.frame(record)) {
    if (nrow(record) != 1L) {
      curation_abort(
        "{.arg record} must be a one-row data frame.",
        class = "planscanR_error_curation_translation"
      )
    }
    return(record)
  }
  if (is.list(record) && !is.null(names(record))) {
    return(as.data.frame(record, stringsAsFactors = FALSE))
  }
  curation_abort(
    "{.arg record} must be a one-row data frame or named record list.",
    class = "planscanR_error_curation_translation"
  )
}

#' Build an optional translation provider.
#'
#' The provider is a dependency-injected boundary around a translation worker.
#' It has no default engine and does not read options or environment variables.
#' A serialized write gate is required before translated values are written to a
#' shared cache or sidecar, so a deployment can coordinate one record at a time.
#'
#' @param translate_fn Function accepting `record`, `source_language`, and
#'   `target_language`, returning a named list with translated fields such as
#'   `title_en` and `summary_en`.
#' @param service_id Stable backend/version identifier recorded with results.
#' @param source_language_fn Optional function accepting `record` and returning
#'   a source-language code.
#' @param cache_read_fn Optional function accepting `key` and `service_id` and
#'   returning a previously translated result, or `NULL` on a miss.
#' @param cache_write_fn Optional function accepting `key` and `value`.
#' @param write_gate_fn Function accepting `key` and `write_fn`; it must invoke
#'   `write_fn` while holding the deployment's per-record serialized gate.
#' @param write_policy Either `"disabled"` or `"serialized"`.
#' @return A `planscanR_curation_translation_provider` object.
#' @export
curation_translation_provider <- function(
  translate_fn,
  service_id,
  source_language_fn = NULL,
  cache_read_fn = NULL,
  cache_write_fn = NULL,
  write_gate_fn = NULL,
  write_policy = c("disabled", "serialized")
) {
  service_id <- curation_scalar_character(
    service_id,
    "service_id",
    class = "planscanR_error_curation_translation"
  )
  write_policy <- match.arg(write_policy)
  if (!curation_function_accepts(translate_fn, c("record", "source_language", "target_language"))) {
    curation_abort(
      "{.arg translate_fn} must accept {.arg record}, {.arg source_language}, and {.arg target_language}.",
      class = "planscanR_error_curation_translation"
    )
  }
  if (!is.null(source_language_fn) && !curation_function_accepts(source_language_fn, "record")) {
    curation_abort(
      "{.arg source_language_fn} must accept {.arg record}.",
      class = "planscanR_error_curation_translation"
    )
  }
  if (!is.null(cache_read_fn) && !curation_function_accepts(cache_read_fn, c("key", "service_id"))) {
    curation_abort(
      "{.arg cache_read_fn} must accept {.arg key} and {.arg service_id}.",
      class = "planscanR_error_curation_translation"
    )
  }
  if (!is.null(cache_write_fn) && !curation_function_accepts(cache_write_fn, c("key", "value"))) {
    curation_abort(
      "{.arg cache_write_fn} must accept {.arg key} and {.arg value}.",
      class = "planscanR_error_curation_translation"
    )
  }
  if (!is.null(write_gate_fn) && !curation_function_accepts(write_gate_fn, c("key", "write_fn"))) {
    curation_abort(
      "{.arg write_gate_fn} must accept {.arg key} and {.arg write_fn}.",
      class = "planscanR_error_curation_translation"
    )
  }
  if (write_policy == "serialized" && (is.null(cache_write_fn) || is.null(write_gate_fn))) {
    curation_abort(
      "Serialized translation writes require both {.arg cache_write_fn} and {.arg write_gate_fn}.",
      class = "planscanR_error_curation_translation"
    )
  }
  structure(
    list(
      translate_fn = translate_fn,
      service_id = service_id,
      source_language_fn = source_language_fn,
      cache_read_fn = cache_read_fn,
      cache_write_fn = cache_write_fn,
      write_gate_fn = write_gate_fn,
      write_policy = write_policy
    ),
    class = "planscanR_curation_translation_provider"
  )
}

curation_translation_value <- function(value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (!is.list(value) || is.null(names(value))) {
    curation_abort(
      "A translation callback must return a named list.",
      class = "planscanR_error_curation_translation"
    )
  }
  value
}

#' Translate one record through an optional injected provider.
#'
#' @param provider A `planscanR_curation_translation_provider`, or `NULL`.
#' @param record One-row record data frame or named record list.
#' @param key Stable composite record key, conventionally country and
#'   document_id.
#' @param source_language Source language code. If `NULL`, the provider's
#'   resolver is used.
#' @param target_language Target language code.
#' @param write Whether a successful value may be written through the provider
#'   cache and serialized gate.
#' @return A named translation result with status, originals, translated fields,
#'   and service; worker failures return `status = "error"` and preserve source
#'   text.
#' @export
curation_translate <- function(
  provider,
  record,
  key,
  source_language = NULL,
  target_language = "eng_Latn",
  write = FALSE
) {
  record <- curation_translation_record(record)
  if (!is.list(key) || is.null(names(key)) || !all(c("country", "document_id") %in% names(key))) {
    curation_abort(
      "{.arg key} must include country and document_id.",
      class = "planscanR_error_curation_translation"
    )
  }
  key <- list(
    country = curation_scalar_character(
      key$country,
      "key$country",
      class = "planscanR_error_curation_translation"
    ),
    document_id = curation_scalar_character(
      key$document_id,
      "key$document_id",
      class = "planscanR_error_curation_translation"
    )
  )
  if (is.null(provider)) {
    return(curation_translation_disabled_result(record))
  }
  if (!inherits(provider, "planscanR_curation_translation_provider")) {
    curation_abort(
      "{.arg provider} must be NULL or a planscanR_curation_translation_provider.",
      class = "planscanR_error_curation_translation"
    )
  }
  if (!is.logical(write) || length(write) != 1L || is.na(write)) {
    curation_abort(
      "{.arg write} must be one non-missing logical value.",
      class = "planscanR_error_curation_translation"
    )
  }
  target_language <- curation_scalar_character(
    target_language,
    "target_language",
    class = "planscanR_error_curation_translation"
  )
  if (!is.null(source_language)) {
    source_language <- curation_scalar_character(
      source_language,
      "source_language",
      class = "planscanR_error_curation_translation"
    )
  } else if (!is.null(provider$source_language_fn)) {
    source_language <- provider$source_language_fn(record = record)
    source_language <- curation_scalar_character(
      source_language,
      "source_language",
      class = "planscanR_error_curation_translation"
    )
  }
  original <- curation_translation_disabled_result(record, service = provider$service_id)
  if (is.null(source_language)) {
    original$status <- "error"
    original$error <- "No source language was supplied or resolved."
    return(original)
  }
  if (!is.null(provider$cache_read_fn)) {
    cached <- provider$cache_read_fn(key = key, service_id = provider$service_id)
    if (!is.null(cached)) {
      cached <- curation_translation_value(cached)
      return(modifyList(original, c(status = "cached", cached)))
    }
  }
  translated <- tryCatch(
    provider$translate_fn(
      record = record,
      source_language = source_language,
      target_language = target_language
    ),
    error = function(e) e
  )
  if (inherits(translated, "error")) {
    original$status <- "error"
    original$error <- conditionMessage(translated)
    return(original)
  }
  translated <- curation_translation_value(translated)
  result <- modifyList(original, translated)
  result$status <- "translated"
  if (isTRUE(write)) {
    if (provider$write_policy != "serialized") {
      curation_abort(
        "Translation writes are disabled for this provider.",
        class = "planscanR_error_curation_translation"
      )
    }
    provider$write_gate_fn(
      key = key,
      write_fn = function() provider$cache_write_fn(key = key, value = result)
    )
  }
  result
}
