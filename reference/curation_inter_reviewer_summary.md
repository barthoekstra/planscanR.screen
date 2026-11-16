# Summarise unanimous agreement across reviewers.

Agreement uses the legacy rule over keep/drop/unsure rows: a record
counts only when at least two distinct reviewers have decisions, and
agrees when all those decisions are identical. Explicit project/corpus
columns are filtered by `config` when supplied.

## Usage

``` r
curation_inter_reviewer_summary(
  reviews,
  config = NULL,
  decisions = c("keep", "drop", "unsure")
)
```

## Arguments

- reviews:

  Review rows with canonical or legacy reviewer identity.

- config:

  Optional configuration used to scope explicit project/corpus columns.

- decisions:

  Decisions included in the agreement summary.

## Value

A one-row tibble with `n_multi`, `n_agree`, and `agreement_rate`, or
`NULL` when no multi-reviewed record exists.
