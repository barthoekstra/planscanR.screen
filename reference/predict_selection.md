# Predict the learned selection decision for records.

Adds two columns: `select_prob` (the model's P(keep)) and
`selected_model` (logical, `select_prob >= threshold`). Network-free —
it reuses the per-record scores already on the sidecars via
`planscanR::selection_features()`.

## Usage

``` r
predict_selection(model, records, threshold = NULL)
```

## Arguments

- model:

  A `planscanR_selection_model`.

- records:

  A tibble carrying the `planscanR::selection_features()` columns.

- threshold:

  Probability cutoff (defaults to the model's own).

## Value

`records` with `select_prob` and `selected_model` added.

## Examples

``` r
if (FALSE) { # \dontrun{
recs <- predict_selection(model, index_cache(country = "de"))
table(recs$selected_model)
} # }
```
