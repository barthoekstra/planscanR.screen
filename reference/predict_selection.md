# Predict the learned selection decision for records.

Adds two columns: `select_prob` (the model's P(keep)) and
`selected_model` (logical, `select_prob >= threshold`). Network-free —
it reuses the per-record scores already on the sidecars via
[`selection_features()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_features.md).

## Usage

``` r
predict_selection(model, records, threshold = NULL)
```

## Arguments

- model:

  A `planscanR_selection_model`.

- records:

  A tibble carrying the
  [`selection_features()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_features.md)
  columns.

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
