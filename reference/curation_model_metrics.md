# Compute stored out-of-fold model metrics through the existing API.

Compute stored out-of-fold model metrics through the existing API.

## Usage

``` r
curation_model_metrics(
  model,
  config = NULL,
  threshold = NULL,
  by_country = FALSE
)
```

## Arguments

- model:

  A `planscanR_selection_model`.

- config:

  Optional compatible curation configuration.

- threshold:

  Optional probability cutoff.

- by_country:

  If `TRUE`, include an `all` row and country rows.

## Value

A metrics tibble, or `NULL` when no out-of-fold data is stored.
