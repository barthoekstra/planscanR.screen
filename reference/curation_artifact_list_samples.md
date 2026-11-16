# List project/reviewer-scoped samples.

List project/reviewer-scoped samples.

## Usage

``` r
curation_artifact_list_samples(store, scope, reviewer_id = NULL, actor)
```

## Arguments

- store:

  A planscanR_curation_artifact_store.

- scope:

  A planscanR_curation_scope.

- reviewer_id:

  Optional trusted reviewer ID filter.

- actor:

  Trusted authenticated actor.

## Value

Sample rows containing at least `sample_id`, `reviewer_id`, scope,
`revision`, and sample artifact metadata.
