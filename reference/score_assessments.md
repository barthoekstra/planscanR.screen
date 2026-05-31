# Re-score an existing planscanR result tibble against (additional) topics.

Thin wrapper over `planscanR::score_records()` tuned for the case where
you already have a tibble of records (from
[`planscanR::get_assessments()`](https://barthoekstra.github.io/planscanR/reference/get_assessments.html)
or
[`planscanR::index_cache()`](https://barthoekstra.github.io/planscanR/reference/index_cache.html))
and want to add — or refresh — relevance scores without re-fetching any
portal data. Optionally writes the updated scores back into the on-disk
sidecars so
[`planscanR::index_cache()`](https://barthoekstra.github.io/planscanR/reference/index_cache.html)
keeps them visible on the next session.

## Usage

``` r
score_assessments(records, topic, model = NULL, write_sidecar = FALSE)
```

## Arguments

- records:

  A tibble in the planscanR result shape.

- topic:

  Single string or named character vector. See
  `planscanR::score_records()`.

- model:

  A `planscanR_embedding_model`. Defaults to
  `planscanR::embedding_model_minilm()`.

- write_sidecar:

  If `TRUE`, every scored record's sidecar JSON is re-written with the
  merged scores. Default `FALSE` (in-memory only).

## Value

The input tibble with the additional `relevance_score_*` columns (and
`relevance_model`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Reload everything in the cache and score against new topics offline.
recs <- index_cache("/path/to/cache")
scored <- score_assessments(
  recs,
  c(wind  = "wind energy",
    solar = "solar energy",
    res   = "regional energy transition strategy and planning"),
  write_sidecar = TRUE
)
} # }
```
