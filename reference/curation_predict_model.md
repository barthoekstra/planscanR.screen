# Predict with a curation selection model after validating configuration.

Predict with a curation selection model after validating configuration.

## Usage

``` r
curation_predict_model(model, records, config = NULL, threshold = NULL)
```

## Arguments

- model:

  A `planscanR_selection_model`.

- records:

  A scored and classified record data frame.

- config:

  Optional configuration whose topic and label vectors must match the
  model.

- threshold:

  Optional probability cutoff.

## Value

`records` with `select_prob` and `selected_model` columns.
