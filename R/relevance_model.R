# Pluggable relevance-model framework.
#
# A relevance model embeds short texts into vectors and declares which
# languages it has been trained on. Built-in models are S3 subclasses of
# `planscanR_embedding_model`. Custom models can be created with the generic
# constructor `embedding_model()` without writing any S3 methods, by passing
# an `embed_fn` and the model's supported-language list directly.
#
# Required S3 methods for any subclass:
#   * embed_text.<class>(model, x)  -> numeric matrix [length(x), dim]
#   * supported_languages.<class>(model) -> character vector (ISO-639-1)
#   * model_name.<class>(model) -> character scalar
# Optional:
#   * format.<class>(model) -> character (single-line summary)

#' Build a custom planscanR embedding model.
#'
#' Wraps a user-supplied `embed_fn` plus a language inventory into an S3
#' object that participates in the same interface as the built-in models
#' (e.g. [embedding_model_minilm()]). Pass it as `relevance_model` to
#' [planscanR::get_assessments()] or [score_records()].
#'
#' @param name Character scalar. Used in the language-support warning and in
#'   the sidecar JSON to record which model produced a score.
#' @param languages Character vector of ISO-639-1 codes the model is
#'   documented to support. Used by [score_records()] to warn when a record's
#'   country language falls outside this set.
#' @param embed_fn A function of one argument (`x`, a character vector) that
#'   returns a numeric matrix with one row per element and a stable
#'   embedding dimension across calls.
#' @return An S3 object of class
#'   `c("planscanR_embedding_custom", "planscanR_embedding_model")`.
#' @export
#' @examples
#' \dontrun{
#' # Trivial example: hash-based "embeddings" for testing
#' em <- embedding_model(
#'   name = "fake-hash",
#'   languages = c("en", "nl"),
#'   embed_fn = function(x) {
#'     matrix(rnorm(length(x) * 32), nrow = length(x))
#'   }
#' )
#' supported_languages(em)
#' embed_text(em, c("hello", "hallo"))
#' }
embedding_model <- function(name, languages, embed_fn) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    cli::cli_abort("{.arg name} must be a single non-empty string.")
  }
  if (!is.character(languages) || length(languages) == 0L) {
    cli::cli_abort("{.arg languages} must be a non-empty character vector.")
  }
  if (!is.function(embed_fn)) {
    cli::cli_abort("{.arg embed_fn} must be a function.")
  }
  structure(
    list(name = name, languages = languages, embed_fn = embed_fn),
    class = c("planscanR_embedding_custom", "planscanR_embedding_model")
  )
}

#' Embed a character vector with a planscanR embedding model.
#'
#' Returns a numeric matrix with one row per input element.
#'
#' @param model A `planscanR_embedding_model` object.
#' @param x Character vector to embed.
#' @return Numeric matrix.
#' @export
embed_text <- function(model, x) {
  UseMethod("embed_text")
}

#' @export
embed_text.planscanR_embedding_custom <- function(model, x) {
  out <- model$embed_fn(x)
  if (!is.matrix(out) || nrow(out) != length(x)) {
    cli::cli_abort(c(
      "Custom embed_fn returned an invalid shape.",
      i = "Expected a matrix with {.val {length(x)}} row{?s}; got a {.cls {class(out)[1]}}."
    ))
  }
  out
}

#' @export
embed_text.default <- function(model, x) {
  cli::cli_abort(
    "No {.fn embed_text} method for class {.cls {class(model)[1]}}.",
    class = "planscanR_error_no_method"
  )
}

#' Report which ISO-639-1 languages a model has been trained on.
#'
#' Used by [score_records()] to warn (one-shot per language/model) when a
#' record's country language falls outside the model's supported set.
#'
#' @param model A `planscanR_embedding_model` object.
#' @return Character vector of ISO-639-1 codes.
#' @export
supported_languages <- function(model) {
  UseMethod("supported_languages")
}

#' @export
supported_languages.planscanR_embedding_custom <- function(model) {
  model$languages
}

#' @export
supported_languages.default <- function(model) {
  cli::cli_abort(
    "No {.fn supported_languages} method for class {.cls {class(model)[1]}}.",
    class = "planscanR_error_no_method"
  )
}

#' Identify a model with a stable name string for logging / sidecars.
#'
#' @param model A `planscanR_embedding_model` object.
#' @return Character scalar.
#' @export
model_name <- function(model) {
  UseMethod("model_name")
}

#' @export
model_name.planscanR_embedding_custom <- function(model) {
  model$name
}

#' @export
model_name.default <- function(model) {
  cli::cli_abort(
    "No {.fn model_name} method for class {.cls {class(model)[1]}}.",
    class = "planscanR_error_no_method"
  )
}

#' @export
format.planscanR_embedding_model <- function(x, ...) {
  sprintf(
    "<planscanR_embedding_model> %s (%d language%s: %s)",
    model_name(x),
    length(supported_languages(x)),
    if (length(supported_languages(x)) == 1L) "" else "s",
    paste(supported_languages(x), collapse = ", ")
  )
}

#' @export
print.planscanR_embedding_model <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}

#' Normalise the `topic` argument into a named character vector.
#'
#' Accepts:
#'   * `NULL` -> `NULL`
#'   * a single character scalar -> length-1 named vector with auto-slug name
#'   * a named character vector -> returned as-is (names are the slugs)
#'   * an unnamed multi-element vector -> names auto-slugified from values
#'
#' @noRd
normalise_topics <- function(topic) {
  if (is.null(topic)) {
    return(NULL)
  }
  if (!is.character(topic) || length(topic) == 0L || any(!nzchar(topic))) {
    cli::cli_abort(
      "{.arg topic} must be a non-empty character vector (optionally named).",
      class = "planscanR_error_bad_input"
    )
  }
  nms <- names(topic)
  if (is.null(nms) || any(!nzchar(nms))) {
    # Auto-slug the topic strings into stable column suffixes.
    auto <- vapply(topic, slugify_topic, character(1))
    if (is.null(nms)) {
      nms <- auto
    } else {
      nms[!nzchar(nms)] <- auto[!nzchar(nms)]
    }
  }
  # Reject duplicate slugs — they'd silently collide as column names.
  if (anyDuplicated(nms) > 0L) {
    cli::cli_abort(
      "{.arg topic} slugs must be unique; got duplicates {.val {nms[duplicated(nms)]}}.",
      class = "planscanR_error_bad_input"
    )
  }
  names(topic) <- nms
  topic
}

#' Slugify a topic phrase into a column-safe suffix.
#' @noRd
slugify_topic <- function(s) {
  s <- tolower(s)
  s <- gsub("[^a-z0-9]+", "_", s)
  s <- gsub("(^_+|_+$)", "", s)
  if (!nzchar(s)) "topic" else s
}

#' Compute relevance scores for a set of records against one or more topics.
#'
#' Cosine similarity between each topic's embedding and each record's title +
#' summary embedding. Records are embedded **once** per call regardless of how
#' many topics are passed, so adding extra topics is essentially free.
#'
#' Emits a one-shot warning if any record's country language falls outside
#' `supported_languages(model)`.
#'
#' @param records A tibble; must include at least `country`, `title`, `summary`.
#' @param topic A character vector of topic phrases. Pass a named vector to
#'   control the column-suffix slugs; unnamed elements get auto-slugified from
#'   their phrase. See examples.
#' @param model A `planscanR_embedding_model` object.
#' @param text_fn Optional function `function(record) -> character` that
#'   builds the text to embed for each record. Default concatenates
#'   `title` and `summary`.
#' @return The input `records` tibble with one `relevance_score_<slug>` column
#'   per topic plus a single `relevance_model` column.
#' @export
#' @examples
#' \dontrun{
#' # Single topic — column will be relevance_score_wind_energy
#' score_records(recs, "wind energy", em)
#'
#' # Multiple topics in one pass — column names will be relevance_score_wind etc.
#' score_records(
#'   recs,
#'   c(wind  = "wind energy",
#'     solar = "solar energy",
#'     res   = "regional energy transition strategy and planning"),
#'   em
#' )
#' }
score_records <- function(records, topic, model, text_fn = NULL) {
  if (!inherits(model, "planscanR_embedding_model")) {
    cli::cli_abort(
      "{.arg model} must be a planscanR_embedding_model object."
    )
  }
  topics <- normalise_topics(topic)
  if (is.null(topics)) {
    cli::cli_abort("{.arg topic} must be a non-empty character vector.")
  }

  if (nrow(records) == 0L) {
    for (nm in names(topics)) {
      records[[paste0("relevance_score_", nm)]] <- numeric(0)
    }
    records$relevance_model <- character(0)
    return(records)
  }
  if (is.null(text_fn)) {
    text_fn <- function(rec) {
      paste(rec$title %||% "", rec$summary %||% "", sep = "\n")
    }
  }
  warn_unsupported_languages(records, model)

  texts <- vapply(seq_len(nrow(records)), function(i) text_fn(records[i, ]), character(1))
  doc_vecs <- embed_text(model, texts)
  topic_vecs <- embed_text(model, unname(topics))
  scores <- cosine_similarity_matrix(doc_vecs, topic_vecs)

  for (i in seq_along(topics)) {
    records[[paste0("relevance_score_", names(topics)[i])]] <- scores[, i]
  }
  records$relevance_model <- model_name(model)
  records
}

#' Cosine similarity between every row of `m` and every row of `topics`.
#'
#' Returns an `[nrow(m), nrow(topics)]` numeric matrix. Used by
#' `score_records()` to compute all topic scores in one shot.
#'
#' @noRd
cosine_similarity_matrix <- function(m, topics) {
  if (!is.matrix(m) || !is.matrix(topics)) {
    cli::cli_abort("cosine_similarity_matrix expects matrix inputs.")
  }
  m_norm <- sqrt(rowSums(m * m))
  t_norm <- sqrt(rowSums(topics * topics))
  # m %*% t(topics) gives [nrow(m), nrow(topics)] dot products.
  num <- m %*% t(topics)
  denom <- outer(m_norm, t_norm)
  out <- num / denom
  out[m_norm == 0, ] <- NA_real_
  out[, t_norm == 0] <- NA_real_
  out
}

#' Warn once per (model, country) if the model doesn't cover its language.
#'
#' Records are still scored — this only flags that quality may be reduced.
#' @noRd
warn_country_language <- function(country, model) {
  country <- tolower(country)
  langs <- languages_for_country(country)
  if (length(langs) == 0L) {
    return(invisible()) # unknown country code, skip silently
  }
  if (any(langs %in% supported_languages(model))) {
    return(invisible())
  }
  key <- paste(model_name(model), country, sep = ":")
  if (key %in% get_warned_languages()) {
    return(invisible())
  }
  warn_partial(c(
    "Country {.val {country}} uses language{?s} {.val {langs}}, which {?is/are} not in the supported set of model {.val {model_name(model)}}.",
    i = "Records will still be scored, but quality may be reduced."
  ))
  mark_warned_language(key)
}

#' One-shot warning when a record's country language is outside the model set.
#' @noRd
warn_unsupported_languages <- function(records, model) {
  for (cc in unique(tolower(records$country))) {
    warn_country_language(cc, model)
  }
}

# Per-session warning ledger
.planscanR_env <- new.env(parent = emptyenv())

get_warned_languages <- function() {
  if (!exists("warned", envir = .planscanR_env)) {
    return(character(0))
  }
  .planscanR_env$warned
}

mark_warned_language <- function(key) {
  warned <- if (exists("warned", envir = .planscanR_env)) {
    .planscanR_env$warned
  } else {
    character(0)
  }
  .planscanR_env$warned <- unique(c(warned, key))
}

#' Re-score an existing planscanR result tibble against (additional) topics.
#'
#' Thin wrapper over [score_records()] tuned for the case where you already
#' have a tibble of records (from [planscanR::get_assessments()] or [planscanR::index_cache()]) and
#' want to add — or refresh — relevance scores without re-fetching any portal
#' data. Optionally writes the updated scores back into the on-disk sidecars
#' so [planscanR::index_cache()] keeps them visible on the next session.
#'
#' @param records A tibble in the planscanR result shape.
#' @param topic Single string or named character vector. See [score_records()].
#' @param model A `planscanR_embedding_model`. Defaults to
#'   [embedding_model_minilm()].
#' @param write_sidecar If `TRUE`, every scored record's sidecar JSON is
#'   re-written with the merged scores. Default `FALSE` (in-memory only).
#' @return The input tibble with the additional `relevance_score_*` columns
#'   (and `relevance_model`).
#' @export
#' @examples
#' \dontrun{
#' # Reload everything in the cache and score against new topics offline.
#' recs <- index_cache("/path/to/cache")
#' scored <- score_assessments(
#'   recs,
#'   c(wind  = "wind energy",
#'     solar = "solar energy",
#'     res   = "regional energy transition strategy and planning"),
#'   write_sidecar = TRUE
#' )
#' }
score_assessments <- function(records, topic, model = NULL, write_sidecar = FALSE) {
  if (is.null(model)) {
    model <- embedding_model_minilm()
  }
  out <- score_records(records, topic = topic, model = model)
  if (write_sidecar && nrow(out) > 0L) {
    for (i in seq_len(nrow(out))) {
      tryCatch(
        planscanR::write_record_sidecar(out[i, ]),
        error = function(e) {
          warn_partial(
            "Could not write sidecar for {.val {out$document_id[i]}}: {conditionMessage(e)}"
          )
        }
      )
    }
  }
  out
}

#' Reset the relevance-model warning ledger for the current session.
#'
#' Useful in tests, or when you want a language-support warning to be
#' emitted again after it has already fired once.
#'
#' @return Nothing, invisibly.
#' @export
reset_relevance_warnings <- function() {
  .planscanR_env$warned <- character(0)
  invisible()
}
