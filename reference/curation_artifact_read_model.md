# Read an immutable model version.

Read an immutable model version.

## Usage

``` r
curation_artifact_read_model(store, scope, model_id, actor)
```

## Arguments

- store:

  A planscanR_curation_artifact_store.

- scope:

  A planscanR_curation_scope.

- model_id:

  Model version identifier.

- actor:

  Trusted authenticated actor.

## Value

`NULL` or a model result containing `model_id`, scope/config identity,
`revision`, provenance, and model payload/metadata.
