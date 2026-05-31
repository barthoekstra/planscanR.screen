# Small leaf helpers screen shares with planscanR. planscanR owns the
# originals (it's the leaf); these self-contained copies keep planscanR.screen
# from reaching into planscanR internals via ::: for one-line utilities.

# Null-coalescing, matching planscanR's internal operator.
`%||%` <- function(a, b) if (is.null(a)) b else a

#' @noRd
warn_partial <- function(message, ..., .envir = parent.frame()) {
  cli::cli_warn(
    message,
    ...,
    class = "planscanR_warning_partial",
    .envir = .envir
  )
}

#' Normalise text for fuzzy/substring matching: NFD strip + German vowel-
#' digraph collapse + lowercase. Mirrors the planscanR helper of the same name.
#' @noRd
normalise_text_for_match <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) {
    return("")
  }
  # NFD + remove combining marks (U+0300-U+036F)
  s <- gsub(
    "[\u0300-\u036f]",
    "",
    iconv(s, from = "UTF-8", to = "UTF-8")
  )
  # Manual umlaut + sharp-s expansion *before* casefold so we don't lose
  # information that iconv-stripping would have folded away.
  s <- chartr(
    "\u00c4\u00d6\u00dc\u00e4\u00f6\u00fc\u00df",
    "AOUaous",
    s
  )
  s <- tolower(s)
  # Collapse vowel digraphs that German writers swap with diacritics.
  s <- gsub("oe", "o", s, fixed = TRUE)
  s <- gsub("ae", "a", s, fixed = TRUE)
  s <- gsub("ue", "u", s, fixed = TRUE)
  s <- gsub("ss", "s", s, fixed = TRUE)
  # Non-alphanumeric to space, collapse whitespace.
  s <- gsub("[^a-z0-9]+", " ", s, perl = TRUE)
  trimws(gsub("\\s+", " ", s))
}
