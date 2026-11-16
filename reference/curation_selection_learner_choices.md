# Return the configured learner choices available to a curation UI.

The registry and availability check are owned by the selection
framework; this adapter only supplies the stable display label used by
curation callers.

## Usage

``` r
curation_selection_learner_choices()
```

## Value

A named character vector mapping display labels to learner keys.
