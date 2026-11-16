# Write a project/reviewer-scoped sample.

Write a project/reviewer-scoped sample.

## Usage

``` r
curation_artifact_write_sample(
  store,
  scope,
  sample,
  reviewer_id,
  actor,
  expected_revision = NULL
)
```

## Arguments

- store:

  A planscanR_curation_artifact_store.

- scope:

  A planscanR_curation_scope.

- sample:

  Sample payload.

- reviewer_id:

  Trusted reviewer ID.

- actor:

  Trusted authenticated actor.

- expected_revision:

  Required current revision, or 0 for a new sample.

## Value

A mutation result containing `ok`, `sample_id`, `reviewer_id`, and the
resulting `revision`.
