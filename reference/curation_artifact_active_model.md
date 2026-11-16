# Read the active model reference.

Read the active model reference.

## Usage

``` r
curation_artifact_active_model(store, scope, actor)
```

## Arguments

- store:

  A planscanR_curation_artifact_store.

- scope:

  A planscanR_curation_scope.

- actor:

  Trusted authenticated actor.

## Value

`NULL` when no model is active, or an active-model result containing
`active_model_id`, `active_revision`, and model provenance.
