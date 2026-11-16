# Read one immutable sample.

Read one immutable sample.

## Usage

``` r
curation_artifact_read_sample(store, scope, sample_id, actor)
```

## Arguments

- store:

  A planscanR_curation_artifact_store.

- scope:

  A planscanR_curation_scope.

- sample_id:

  Sample identifier.

- actor:

  Trusted authenticated actor.

## Value

`NULL` or a sample result containing `sample_id`, `reviewer_id`, scope,
`revision`, and sample payload/metadata.
