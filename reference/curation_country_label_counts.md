# Summarise country-level review progress.

Counts are based on distinct `(country, document_id)` identities.
`labelled` includes any active decision, while `keep`, `drop`, and
`unsure` count the corresponding decisions. `yours` restricts the
labelled count to one reviewer. Explicit project/corpus columns are
filtered by `scope`.

## Usage

``` r
curation_country_label_counts(
  records,
  reviews,
  countries = NULL,
  reviewer_id = NULL,
  scope = NULL,
  reviewer = NULL
)

curation_review_progress(
  records,
  reviews,
  countries = NULL,
  reviewer_id = NULL,
  scope = NULL,
  reviewer = NULL
)
```

## Arguments

- records:

  Corpus records with `country` and `document_id`.

- reviews:

  `NULL` or review rows.

- countries:

  Countries to include, in output order.

- reviewer_id:

  Optional reviewer identity for the `yours` column.

- scope:

  Optional curation scope.

- reviewer:

  Legacy alias for `reviewer_id`.

## Value

A tibble with `country`, `records`, `labelled`, `pct`, `keep`, `drop`,
`unsure`, and `yours`.
