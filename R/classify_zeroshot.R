# Pluggable zero-shot classifier framework + a local transformers backend.
#
# Parallels the embedding-model framework in relevance_model.R /
# relevance_minilm.R: a classifier assigns each text a probability over a set
# of candidate labels. Built-in backend is a HuggingFace
# `zero-shot-classification` pipeline (multilingual NLI) run locally via
# reticulate. Custom backends can be plugged in with `classifier()` for tests
# or alternative models without writing S3 methods.
#
# Required S3 methods for any subclass:
#   * classify_text.<class>(model, x, labels, multi_label) -> numeric matrix
#       [length(x), length(labels)], columns named by the slugs of `labels`.
#   * classifier_name.<class>(model) -> character scalar.

#' Build a custom planscanR classifier.
#'
#' Wraps a user-supplied `classify_fn` into an S3 object that participates in
#' the same interface as the built-in [classify_model_zeroshot()]. Useful for
#' tests (a deterministic mock) or to plug in a different backend.
#'
#' @param name Character scalar; recorded in the sidecar so a stored
#'   classification knows which model produced it.
#' @param classify_fn A function `function(x, labels, multi_label)` where `x`
#'   is a character vector and `labels` is a named character vector
#'   (names = slugs, values = hypothesis phrases). Must return a numeric
#'   matrix with `length(x)` rows and one column per label, columns named by
#'   the label slugs (`names(labels)`).
#' @return An S3 object of class
#'   `c("planscanR_classifier_custom", "planscanR_classifier")`.
#' @export
classifier <- function(name, classify_fn) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    cli::cli_abort("{.arg name} must be a single non-empty string.")
  }
  if (!is.function(classify_fn)) {
    cli::cli_abort("{.arg classify_fn} must be a function.")
  }
  structure(
    list(name = name, classify_fn = classify_fn),
    class = c("planscanR_classifier_custom", "planscanR_classifier")
  )
}

#' Classify a character vector against candidate labels.
#'
#' Returns a numeric matrix with one row per element of `x` and one column per
#' label (columns named by the label slugs, i.e. `names(labels)`). Values are
#' label probabilities. With `multi_label = FALSE` each row sums to ~1
#' (softmax over labels); with `multi_label = TRUE` each cell is an
#' independent probability.
#'
#' @param model A `planscanR_classifier` object.
#' @param x Character vector to classify.
#' @param labels Named character vector: names are stable slugs (used as
#'   output column names), values are the natural-language hypothesis phrases
#'   fed to the model.
#' @param multi_label Logical; passed through to the backend.
#' @return Numeric matrix `[length(x), length(labels)]`.
#' @export
classify_text <- function(model, x, labels, multi_label = FALSE) {
  UseMethod("classify_text")
}

#' @export
classify_text.planscanR_classifier_custom <- function(model, x, labels, multi_label = FALSE) {
  out <- model$classify_fn(x, labels, multi_label)
  validate_classify_output(out, x, labels)
}

#' @export
classify_text.default <- function(model, x, labels, multi_label = FALSE) {
  cli::cli_abort(
    "No {.fn classify_text} method for class {.cls {class(model)[1]}}.",
    class = "planscanR_error_no_method"
  )
}

#' Shared shape check for classifier output.
#' @noRd
validate_classify_output <- function(out, x, labels) {
  if (
    !is.matrix(out) ||
      nrow(out) != length(x) ||
      ncol(out) != length(labels)
  ) {
    cli::cli_abort(c(
      "Classifier returned an invalid shape.",
      i = "Expected a {.val {length(x)}} x {.val {length(labels)}} matrix."
    ))
  }
  colnames(out) <- names(labels)
  out
}

#' Identify a classifier with a stable name string for sidecars / logging.
#' @param model A `planscanR_classifier`.
#' @return Character scalar.
#' @export
classifier_name <- function(model) {
  UseMethod("classifier_name")
}

#' @export
classifier_name.planscanR_classifier_custom <- function(model) {
  model$name
}

#' @export
classifier_name.default <- function(model) {
  cli::cli_abort(
    "No {.fn classifier_name} method for class {.cls {class(model)[1]}}.",
    class = "planscanR_error_no_method"
  )
}

# -----------------------------------------------------------------------------
# Built-in backend: HuggingFace zero-shot-classification via reticulate
# -----------------------------------------------------------------------------

#' Local zero-shot classifier backed by a HuggingFace NLI model.
#'
#' Lazily constructs a `transformers` `zero-shot-classification` pipeline on
#' first use (cached for the session). The default model is the multilingual
#' NLI model `MoritzLaurer/mDeBERTa-v3-base-xnli-multilingual-nli-2mil7`, which
#' handles German / Dutch / English text natively — no translation required.
#'
#' Requires the Python packages `transformers`, `torch`, `sentencepiece` and
#' `protobuf` (the last two are needed by the mDeBERTa tokenizer). Install with
#' `reticulate::py_require(c("transformers","torch","sentencepiece","protobuf"))`.
#'
#' @param model_id HuggingFace model ID.
#' @param device Torch device: `"mps"`, `"cuda"`, `"cpu"`, or `NULL` to
#'   auto-detect (MPS on Apple Silicon, else CUDA, else CPU).
#' @param batch_size GPU batch size for the pipeline (number of
#'   text-hypothesis NLI pairs evaluated per forward pass). On a single
#'   GPU this is the main throughput lever: `16` roughly doubles throughput
#'   over unbatched on MPS. Larger values (e.g. 64) are often *slower* because
#'   variable-length sequences get padded to the batch maximum, so the long
#'   outliers dominate — 16 is a good default. There is no benefit to
#'   process-level parallelism on a single GPU (workers contend for one device
#'   and each reloads the model).
#' @return A `planscanR_classifier_zeroshot` S3 object.
#' @export
#' @examples
#' \dontrun{
#' clf <- classify_model_zeroshot()
#' labels <- c(wind = "wind energy project", other = "unrelated to energy")
#' classify_text(clf, c("Windpark Test", "Wohnungsbau"), labels)
#' }
classify_model_zeroshot <- function(
  model_id = "MoritzLaurer/mDeBERTa-v3-base-xnli-multilingual-nli-2mil7",
  device = NULL,
  batch_size = 16L
) {
  structure(
    list(model_id = model_id, device = device, batch_size = as.integer(batch_size)),
    class = c("planscanR_classifier_zeroshot", "planscanR_classifier")
  )
}

# Session cache: "<model_id>@<device>" -> python pipeline object.
.zeroshot_cache <- new.env(parent = emptyenv())

#' Resolve the torch device string, auto-detecting when `NULL`.
#' @noRd
zeroshot_resolve_device <- function(device) {
  if (!is.null(device)) {
    return(device)
  }
  torch <- tryCatch(reticulate::import("torch"), error = function(e) NULL)
  if (is.null(torch)) {
    return("cpu")
  }
  if (isTRUE(torch$backends$mps$is_available())) {
    return("mps")
  }
  if (isTRUE(torch$cuda$is_available())) {
    return("cuda")
  }
  "cpu"
}

#' Lazily build (and cache) the transformers zero-shot pipeline.
#' @noRd
zeroshot_python_pipeline <- function(model) {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    cli::cli_abort(
      c(
        "Package {.pkg reticulate} is required to use the zero-shot classifier.",
        i = "Install with {.code install.packages(\"reticulate\")}."
      ),
      class = "planscanR_error_missing_python"
    )
  }
  device <- zeroshot_resolve_device(model$device)
  key <- paste(model$model_id, device, sep = "@")
  if (exists(key, envir = .zeroshot_cache)) {
    return(get(key, envir = .zeroshot_cache))
  }
  if (!reticulate::py_module_available("transformers")) {
    cli::cli_abort(
      c(
        "Python module {.pkg transformers} is not available.",
        i = "Install it with {.code reticulate::py_require(c(\"transformers\",\"torch\",\"sentencepiece\",\"protobuf\"))}."
      ),
      class = "planscanR_error_missing_python"
    )
  }
  transformers <- reticulate::import("transformers", delay_load = FALSE)
  pipe <- transformers$pipeline(
    "zero-shot-classification",
    model = model$model_id,
    device = device
  )
  assign(key, pipe, envir = .zeroshot_cache)
  pipe
}

#' @export
classify_text.planscanR_classifier_zeroshot <- function(model, x, labels, multi_label = FALSE) {
  pipe <- zeroshot_python_pipeline(model)
  hyp <- unname(labels)
  # The pipeline accepts a list of sequences and returns, per sequence, a dict
  # with `labels` (sorted by score, desc) and `scores`. We reorder back to the
  # caller's label order so the output columns are stable. `batch_size` batches
  # the text-hypothesis NLI pairs through the GPU — the key throughput lever.
  bs <- model$batch_size %||% 16L
  res <- pipe(as.list(x), hyp, multi_label = multi_label, batch_size = as.integer(bs))
  # Single-input pipelines return one dict, not a length-1 list; normalise.
  if (!is.null(res$labels)) {
    res <- list(res)
  }
  mat <- matrix(NA_real_, nrow = length(x), ncol = length(hyp))
  for (i in seq_along(res)) {
    r <- res[[i]]
    lab_i <- unlist(r$labels, use.names = FALSE)
    sc_i <- as.numeric(unlist(r$scores, use.names = FALSE))
    mat[i, ] <- sc_i[match(hyp, lab_i)]
  }
  colnames(mat) <- names(labels)
  mat
}

#' @export
classifier_name.planscanR_classifier_zeroshot <- function(model) {
  model$model_id
}

#' Evict cached zero-shot pipeline(s) to free memory or for tests.
#' @param model_id Optional model ID to evict (all device variants). `NULL`
#'   clears the whole cache.
#' @return Nothing, invisibly.
#' @export
reset_classifier_cache <- function(model_id = NULL) {
  if (is.null(model_id)) {
    rm(list = ls(.zeroshot_cache), envir = .zeroshot_cache)
  } else {
    keys <- ls(.zeroshot_cache)
    drop <- keys[startsWith(keys, paste0(model_id, "@"))]
    if (length(drop)) {
      rm(list = drop, envir = .zeroshot_cache)
    }
  }
  invisible()
}
