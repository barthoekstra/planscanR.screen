# List immutable model versions.

List immutable model versions.

## Usage

``` r
curation_artifact_list_models(store, scope, actor)
```

## Arguments

- store:

  A planscanR_curation_artifact_store.

- scope:

  A planscanR_curation_scope.

- actor:

  Trusted authenticated actor.

## Value

Model rows containing at least `model_id`, scope/config identity,
`revision`, provenance, and artifact metadata.
