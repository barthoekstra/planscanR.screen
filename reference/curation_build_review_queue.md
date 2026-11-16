# Build a reviewer-aware validation or fresh-review queue.

Rows reviewed by another reviewer in the supplied project/corpus scope
are returned first with `mode = "validate"`. Once those are exhausted, a
fresh country-stratified sample of never-reviewed rows is returned with
`mode = "fresh"`; no available rows returns `mode = "empty"`. The input
review frame may use canonical `reviewer_id` or the legacy `reviewer`
name.

## Usage

``` r
curation_build_review_queue(
  records,
  reviews,
  reviewer_id = NULL,
  countries = NULL,
  n_per_country = 1L,
  seed = NULL,
  scope = NULL,
  reviewer = NULL
)
```

## Arguments

- records:

  A frame with `country` and `document_id` columns.

- reviews:

  `NULL` or review rows. Rows with `deleted = TRUE` are ignored.

- reviewer_id:

  Stable reviewer identity. `reviewer` is accepted as a legacy alias.

- countries:

  Optional country filter.

- n_per_country:

  Positive fresh-sample size per country.

- seed:

  Optional integer seed. `NA` behaves like `NULL`; with an explicit
  seed, the caller's RNG state is restored.

- scope:

  Optional curation scope used to filter explicit project/corpus columns
  in `reviews`.

- reviewer:

  Legacy alias for `reviewer_id`.

## Value

A tibble with `document_id` and `country` columns and a `mode`
attribute.
