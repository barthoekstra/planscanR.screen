# Write an immutable model version.

Write an immutable model version.

## Usage

``` r
curation_artifact_write_model(
  store,
  scope,
  model,
  provenance,
  actor,
  expected_revision = NULL
)
```

## Arguments

- store:

  A planscanR_curation_artifact_store.

- scope:

  A planscanR_curation_scope.

- model:

  Model payload in the existing selection-model format.

- provenance:

  Training-review/configuration provenance.

- actor:

  Trusted authenticated actor.

- expected_revision:

  Required current revision, or 0 for a new version.

## Value

A mutation result containing `ok`, `model_id`, and the resulting
`revision`.
