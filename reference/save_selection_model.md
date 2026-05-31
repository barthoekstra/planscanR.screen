# Persist / restore a trained selection model.

Thin wrappers over [`saveRDS()`](https://rdrr.io/r/base/readRDS.html) /
[`readRDS()`](https://rdrr.io/r/base/readRDS.html) with a class check,
so the app and the acquisition runbook can share one artifact.

## Usage

``` r
save_selection_model(model, path)

load_selection_model(path)
```

## Arguments

- model:

  A `planscanR_selection_model`.

- path:

  File path (conventionally `selection_model.rds` in the cache root,
  alongside `reviews.csv`).

## Value

`save_selection_model()` returns `path` invisibly;
`load_selection_model()` returns the model (or `NULL` if absent).
