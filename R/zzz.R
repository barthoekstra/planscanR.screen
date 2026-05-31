.onLoad <- function(libname, pkgname) {
  # Python deps owned by this package, declared once at load. reticulate's
  # py_require() is additive across the session, so this does not clobber a
  # sibling package's earlier declaration (e.g. planscanR.biogain's ibridges /
  # argostranslate). Per the project memory note, sentence-transformers must be
  # declared at the start of every R session for relevance scoring to resolve.
  #   * sentence-transformers — the MiniLM embedding backend
  #     ([embedding_model_minilm()]).
  #   * transformers + torch + sentencepiece + protobuf — the zero-shot
  #     classifier backend ([classify_model_zeroshot()]).
  if (requireNamespace("reticulate", quietly = TRUE)) {
    try(
      reticulate::py_require(c(
        "sentence-transformers",
        "transformers",
        "torch",
        "sentencepiece",
        "protobuf"
      )),
      silent = TRUE
    )
  }
  invisible()
}
