# Score records against the keyword lexicon.

Adds one `kw_<topic>` integer column per lexicon topic (the number of
term occurrences in the record's title + summary + category) plus a
`kw_total` column. Matching is on normalised text (lowercased,
diacritics stripped, German vowel digraphs collapsed). Each term is
matched as a plain substring, so a stem like `"wind"` also matches
compounds such as `"windpark"`.

## Usage

``` r
score_keywords(records, lexicon, text_fn = NULL)
```

## Arguments

- records:

  A tibble; uses `title`, `summary`, and (if present) `native_type`.

- lexicon:

  Named list of term vectors, one per topic (required). The BIOGAIN
  lexicon is `biogain_keyword_lexicon()` in the `planscanR.biogain`
  package.

- text_fn:

  Optional `function(record) -> character` building the text to scan.
  Default concatenates title + summary + native_type.

## Value

`records` with `kw_<topic>` and `kw_total` columns added.

## Examples

``` r
if (FALSE) { # \dontrun{
recs <- index_cache(country = "nl")
scored <- score_keywords(recs, lexicon = biogain_keyword_lexicon())
scored[scored$kw_total == 0, "title"] # likely non-energy
} # }
```
