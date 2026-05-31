# Registry of the built-in learners.

Maps a stable key to a zero-argument constructor, for UIs that let the
user pick a learner. `available_only = TRUE` returns an empty list when
the tidymodels packages needed to fit any learner aren't installed.

## Usage

``` r
selection_learners(available_only = FALSE)
```

## Arguments

- available_only:

  If `TRUE`, return learners only when they can actually be trained (the
  tidymodels packages are installed).

## Value

A named list of constructor functions, keyed by learner key.

## Examples

``` r
names(selection_learners())
#> [1] "logistic"
```
