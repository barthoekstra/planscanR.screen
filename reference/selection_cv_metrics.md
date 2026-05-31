# Out-of-fold metrics for a trained model at an arbitrary threshold.

Recomputes precision/recall/F1 + confusion from the model's stored
out-of-fold predictions — no retraining — so a UI can sweep the decision
threshold cheaply.

## Usage

``` r
selection_cv_metrics(model, threshold = NULL, by_country = FALSE)
```

## Arguments

- model:

  A `planscanR_selection_model`.

- threshold:

  Probability cutoff (defaults to the model's own).

- by_country:

  If `TRUE`, return one row per country plus an `"all"` row (a `country`
  column is prepended); otherwise a single overall row.

## Value

A metrics tibble (one row, or one per country + `"all"` when
`by_country = TRUE`), or `NULL` if the model has no OOF data.
