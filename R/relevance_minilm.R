# Built-in embedding backend: sentence-transformers via reticulate.
#
# Default model: paraphrase-multilingual-MiniLM-L12-v2 (50+ languages, ~80 MB,
# documented at https://www.sbert.net/docs/sentence_transformer/pretrained_models.html).
# The Python model object is constructed lazily on first use and cached in a
# session-level environment so subsequent calls are cheap.

#' Languages supported by paraphrase-multilingual-MiniLM-L12-v2.
#'
#' Mirrors the list documented at
#' <https://www.sbert.net/docs/sentence_transformer/pretrained_models.html>.
#'
#' @noRd
minilm_languages <- function() {
  # fmt: skip
  c(
    "ar", "bg", "ca", "cs", "da", "de", "el", "en", "es", "et", "fa", "fi",
    "fr", "gl", "gu", "he", "hi", "hr", "hu", "hy", "id", "it", "ja", "ka",
    "ko", "ku", "lt", "lv", "mk", "mn", "mr", "ms", "my", "nb", "nl", "pl",
    "pt", "ro", "ru", "sk", "sl", "sq", "sr", "sv", "th", "tr", "uk", "ur",
    "vi"
  )
}

#' Build an embedding model backed by sentence-transformers.
#'
#' First call lazily initialises a Python `SentenceTransformer` via
#' [reticulate::import()]. The model is cached in a session-level environment;
#' subsequent calls re-use it. The Python package `sentence-transformers` must
#' be available — see [reticulate::py_install()].
#'
#' @param model_id Hugging Face model ID. Defaults to
#'   `"sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2"`.
#' @param languages Override the documented language list. Defaults to the
#'   50 languages documented for the default model. Ignored if the user is
#'   plugging in a different model whose language inventory differs.
#' @return A `planscanR_embedding_minilm` S3 object.
#' @export
#' @examples
#' \dontrun{
#' em <- embedding_model_minilm()
#' supported_languages(em)
#' v <- embed_text(em, c("wind energy", "windenergie"))
#' }
embedding_model_minilm <- function(
  model_id = "sentence-transformers/paraphrase-multilingual-MiniLM-L12-v2",
  languages = minilm_languages()
) {
  structure(
    list(model_id = model_id, languages = languages),
    class = c(
      "planscanR_embedding_minilm",
      "planscanR_embedding_model"
    )
  )
}

# Session-level cache: model_id -> Python SentenceTransformer object
.minilm_cache <- new.env(parent = emptyenv())

#' Lazily import sentence-transformers and load the model.
#' @noRd
minilm_python_model <- function(model_id) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg reticulate} is required to use sentence-transformers backends.",
        i = "Install with {.code install.packages(\"reticulate\")}."
      ),
      class = "planscanR_error_missing_python"
    )
  }
  if (exists(model_id, envir = .minilm_cache)) {
    return(get(model_id, envir = .minilm_cache))
  }
  if (!reticulate::py_module_available("sentence_transformers")) {
    cli::cli_abort(
      c(
        "Python module {.pkg sentence-transformers} is not available.",
        i = "Install it with {.code reticulate::py_install(\"sentence-transformers\")}."
      ),
      class = "planscanR_error_missing_python"
    )
  }
  st <- reticulate::import("sentence_transformers", delay_load = FALSE)
  model <- st$SentenceTransformer(model_id)
  assign(model_id, model, envir = .minilm_cache)
  model
}

#' @export
embed_text.planscanR_embedding_minilm <- function(model, x) {
  py_model <- minilm_python_model(model$model_id)
  # encode() returns a numpy array; reticulate converts to either a 1D `array`
  # (for length-1 input) or a 2D `matrix array` (for batches). Normalise to a
  # length(x) × dim matrix in both cases.
  out <- py_model$encode(x, convert_to_numpy = TRUE)
  if (length(dim(out)) < 2L) {
    out <- matrix(as.numeric(out), nrow = length(x))
  } else {
    out <- as.matrix(out)
  }
  out
}

#' @export
supported_languages.planscanR_embedding_minilm <- function(model) {
  model$languages
}

#' @export
model_name.planscanR_embedding_minilm <- function(model) {
  model$model_id
}

#' Reset the cached Python model for testing or to free memory.
#'
#' Removes the cached `SentenceTransformer` for a given model ID (or all
#' cached models if `model_id` is `NULL`). The next call to
#' [embed_text()] on that model will re-load it from disk / Hugging Face.
#'
#' @param model_id Optional model ID to evict. `NULL` clears all.
#' @return Nothing, invisibly.
#' @export
reset_embedding_cache <- function(model_id = NULL) {
  if (is.null(model_id)) {
    rm(list = ls(.minilm_cache), envir = .minilm_cache)
  } else if (exists(model_id, envir = .minilm_cache)) {
    rm(list = model_id, envir = .minilm_cache)
  }
  invisible()
}
