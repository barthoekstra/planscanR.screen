# Train the learned selection model from human review labels.

Train the learned selection model from human review labels.

## Usage

``` r
train_selection_model(
  records,
  reviews,
  topics,
  labels,
  learner = selection_learner_logistic(),
  eval_source = "random",
  include = character(0),
  v = 5L,
  repeats = 1L,
  threshold = 0.5,
  seed = NULL
)
```

## Arguments

- records:

  A scored + classified tibble (from
  [`planscanR::get_assessments()`](https://barthoekstra.github.io/planscanR/reference/get_assessments.html),
  [`planscanR::index_cache()`](https://barthoekstra.github.io/planscanR/reference/index_cache.html),
  or the review-app snapshot) carrying the
  `planscanR::selection_features()` columns. Only records that also
  appear in `reviews` with a keep/drop decision are used for training.

- reviews:

  The review-decision tibble (the app's `reviews.csv`), with
  `document_id`, `country`, `decision`, `source`, `reviewed_at`.

- topics, labels:

  The topic and classifier-label vectors naming the feature columns
  (required); see `planscanR::selection_feature_names()`. Stored on the
  returned model so `planscanR::predict_selection()` rebuilds the same
  feature frame.

- learner:

  A planscanR::selection_learner. Defaults to
  `planscanR::selection_learner_logistic()`.

- eval_source:

  Restrict labels to this review `source` (default `"random"` — the
  unbiased sample). `NULL` uses every keep/drop label.

- include:

  Optional extra feature columns; see `planscanR::selection_features()`.

- v, repeats:

  Cross-validation folds and repeats for the out-of-fold metrics.

- threshold:

  Default probability cutoff for the keep decision.

- seed:

  Optional RNG seed for reproducible folds.

## Value

A `planscanR_selection_model`: the fitted workflow plus provenance
(`learner_name`, `features`, `n_train`, `trained_at`, ...), the
out-of-fold predictions (`oof`), and the CV metrics at `threshold`
(`cv`).

## Examples

``` r
if (FALSE) { # \dontrun{
recs <- index_cache(country = "nl")
rev <- read.csv(file.path(cache_dir, "reviews.csv"))
m <- train_selection_model(recs, rev)
m$cv
} # }
```
