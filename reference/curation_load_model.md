# Load a legacy-compatible curation model artifact.

Load a legacy-compatible curation model artifact.

## Usage

``` r
curation_load_model(path, config = NULL)
```

## Arguments

- path:

  RDS source; a missing path returns `NULL`.

- config:

  Optional compatible curation configuration.

## Value

A `planscanR_selection_model`, or `NULL` when `path` is absent.
