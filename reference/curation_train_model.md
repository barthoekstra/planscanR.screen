# Train a curation selection model using explicit configuration.

`eval_source = "random"` keeps model fitting and out-of-fold evaluation
on the unbiased random-review sample. The existing trainer performs
unanimous reviewer consensus and retains the established
`planscanR_selection_model` fields and class.

## Usage

``` r
curation_train_model(
  records,
  reviews,
  config,
  learner = NULL,
  learner_key = NULL,
  eval_source = "random",
  include = character(0),
  v = 5L,
  repeats = 1L,
  threshold = 0.5,
  seed = NULL,
  input_provenance = NULL,
  cohort_receipt = NULL
)
```

## Arguments

- records:

  A scored and classified record data frame.

- reviews:

  Review rows with country, document_id, reviewer_id (or reviewer),
  decision, source, and reviewed_at.

- config:

  A `planscanR_curation_config` supplying topics and labels.

- learner:

  Optional existing `planscanR_selection_learner`.

- learner_key:

  Optional key from
  [`selection_learners()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learners.md).

- eval_source:

  Review source(s) used for training and evaluation; defaults to
  `"random"`, or `NULL` for all sources.

- include:

  Optional extra feature columns accepted by
  [`selection_features()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_features.md).

- v, repeats:

  Cross-validation folds and repeats.

- threshold:

  Probability cutoff stored on the model.

- seed:

  Optional seed passed to the existing trainer.

- input_provenance:

  Optional identity for the explicitly selected scored input snapshot.

- cohort_receipt:

  Optional immutable historical-cohort receipt identity.

## Value

A `planscanR_selection_model`.
