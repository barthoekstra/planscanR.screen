# Compute a held-out learning curve with explicit curation configuration.

The fixed held-out split and learner implementation remain in
[`selection_learning_curve()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learning_curve.md).

## Usage

``` r
curation_learning_curve(
  records,
  reviews,
  config,
  learner = NULL,
  learner_key = NULL,
  sizes = NULL,
  test_frac = 0.25,
  repeats = 10L,
  eval_source = "random",
  threshold = 0.5,
  seed = NULL,
  by_country = FALSE
)

curation_compute_learning_curve(
  records,
  reviews,
  config,
  learner = NULL,
  learner_key = NULL,
  sizes = NULL,
  test_frac = 0.25,
  repeats = 10L,
  eval_source = "random",
  threshold = 0.5,
  seed = NULL,
  by_country = FALSE
)
```

## Arguments

- records:

  A scored and classified record data frame.

- reviews:

  Review rows accepted by
  [`curation_train_model()`](https://barthoekstra.github.io/planscanR.screen/reference/curation_train_model.md).

- config:

  A `planscanR_curation_config`.

- learner:

  Optional existing learner object.

- learner_key:

  Optional learner-registry key.

- sizes:

  Optional training sizes.

- test_frac:

  Fraction held out per repeat.

- repeats:

  Number of held-out repeats.

- eval_source:

  Review source(s), defaulting to `"random"`.

- threshold:

  Probability cutoff.

- seed:

  Optional seed.

- by_country:

  If `TRUE`, include held-out country slices.

## Value

The long learning-curve tibble from
[`selection_learning_curve()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learning_curve.md).
