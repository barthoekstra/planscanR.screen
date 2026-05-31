# Learning curve for the learned selection model.

Estimates how the model's held-out F1 / precision / recall improve as
the number of human keep/drop labels grows. For each repeat it makes one
stratified train/test split (the test set is FIXED across all training
sizes within that repeat, so the metric is comparable as the training
pool grows), then fits the learner on increasing stratified subsamples
of the training pool and scores the held-out test set. Repeating the
whole thing `repeats` times and averaging (see
[`learning_curve_summary()`](https://barthoekstra.github.io/planscanR.screen/reference/learning_curve_summary.md))
smooths out the split noise and shows where the curve flattens.

## Usage

``` r
selection_learning_curve(
  records,
  reviews,
  topics,
  labels,
  learner = selection_learner_logistic(),
  sizes = NULL,
  test_frac = 0.25,
  repeats = 10,
  eval_source = "random",
  threshold = 0.5,
  seed = NULL,
  by_country = FALSE
)
```

## Arguments

- records:

  A scored + classified tibble (from
  [`planscanR::get_assessments()`](https://barthoekstra.github.io/planscanR/reference/get_assessments.html),
  [`planscanR::index_cache()`](https://barthoekstra.github.io/planscanR/reference/index_cache.html),
  or a review-app snapshot) carrying the
  [`selection_features()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_features.md)
  columns.

- reviews:

  The review-decision tibble (e.g. a review tool's `reviews.csv`), with
  `document_id`, `country`, `decision`, `source`, `reviewed_at`.

- topics, labels:

  The topic and classifier-label vectors naming the feature columns
  (required); see
  [`selection_feature_names()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_feature_names.md).

- learner:

  A
  [selection_learner](https://barthoekstra.github.io/planscanR.screen/reference/selection_learner.md).
  Defaults to
  [`selection_learner_logistic()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learners_builtin.md).

- sizes:

  Optional integer vector of training-label counts to evaluate. `NULL`
  builds a grid of ~10-12 increasing sizes from 30 up to the maximum
  train-pool size (always including that maximum). Supplied sizes larger
  than the train pool are dropped, but the pool maximum is always kept.

- test_frac:

  Fraction of the labelled data held out as the fixed test set (per
  repeat).

- repeats:

  Number of repeated held-out resamples.

- eval_source:

  Restrict labels to this review `source` (default `"random"` — the
  unbiased sample). `NULL` uses every keep/drop label.

- threshold:

  Probability cutoff for the keep decision when scoring.

- seed:

  Optional RNG seed for reproducible splits.

- by_country:

  If `TRUE`, also slice the held-out test set per country and compute
  one set of metrics per country at every (size, repeat), plus an
  `"all"` row that matches the default output. A `country` column is
  prepended. The model is still trained corpus-wide — this only varies
  the evaluation slice, so it shows where the shared model performs as
  the training pool grows. Slices smaller than 1 record are dropped
  silently.

## Value

A long tibble with one row per (size, repeat), columns in order: `size`,
`n_train_used`, `rep`, `n_test`, `precision`, `recall`, `f1`. When
`by_country = TRUE`, a leading `country` column is added and there is
one row per (country, size, repeat) plus an `"all"` row per (size,
repeat).

## See also

[`learning_curve_summary()`](https://barthoekstra.github.io/planscanR.screen/reference/learning_curve_summary.md)
to aggregate the curve.

## Examples

``` r
if (FALSE) { # \dontrun{
recs <- index_cache(country = "nl")
rev <- read.csv(file.path(cache_dir, "reviews.csv"), colClasses = "character")
curve <- selection_learning_curve(recs, rev, repeats = 10)
learning_curve_summary(curve)
} # }
```
