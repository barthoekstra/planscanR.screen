# Compare configured selection output with unanimous human decisions.

Evaluation filters the requested review source before applying the
existing unanimous
[`consensus_reviews()`](https://barthoekstra.github.io/planscanR.screen/reference/consensus_reviews.md)
rule. The default `eval_source = "random"` preserves the legacy unbiased
evaluation path.

## Usage

``` r
curation_selection_vs_human(
  records,
  reviews,
  config = NULL,
  eval_source = "random",
  by_country = FALSE,
  selection_col = "selected"
)
```

## Arguments

- records:

  Records containing `country`, `document_id`, and the selected logical
  column.

- reviews:

  Review rows with canonical or legacy reviewer identity.

- config:

  Optional configuration used to scope explicit project/corpus columns.

- eval_source:

  Review source(s) to evaluate, defaulting to `"random"`; `NULL`
  evaluates all sources.

- by_country:

  If `TRUE`, prepend `country` with an `all` row.

- selection_col:

  Name of the logical selection column, default `"selected"`.

## Value

One metrics row, country metrics, or `NULL` when no agreed labels match
the records.
