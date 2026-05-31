# Country-to-language mapping used by the relevance gate's language-support
# warning. Values are ISO-639-1 codes. Multi-language countries return a
# vector and are treated as supported when *any* of the listed languages is
# in the model's supported set.

#' Primary spoken/written language(s) of each supported country.
#'
#' @return Named list `country_code -> character vector of ISO-639-1 lang codes`.
#' @noRd
country_languages <- function() {
  list(
    nl = "nl",
    de = "de",
    at = "de",
    dk = "da",
    be = c("nl", "fr", "de"),
    ch = c("de", "fr", "it"),
    lu = c("lb", "fr", "de"),
    ie = "en",
    mt = c("mt", "en"),
    fr = "fr",
    es = "es",
    pt = "pt",
    it = "it",
    pl = "pl",
    cz = "cs",
    sk = "sk",
    si = "sl",
    hr = "hr",
    hu = "hu",
    ro = "ro",
    bg = "bg",
    gr = "el",
    cy = c("el", "tr"),
    se = "sv",
    no = "nb",
    fi = c("fi", "sv"),
    ee = "et",
    lv = "lv",
    lt = "lt",
    is = "is",
    uk = "en"
  )
}

#' Look up the language(s) of a country code; returns `character(0)` if unknown.
#' @noRd
languages_for_country <- function(country) {
  m <- country_languages()
  v <- m[[tolower(country)]]
  if (is.null(v)) character(0) else v
}
