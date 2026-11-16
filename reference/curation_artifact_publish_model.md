# Atomically publish an immutable model as active.

Atomically publish an immutable model as active.

## Usage

``` r
curation_artifact_publish_model(
  store,
  scope,
  model_id,
  actor,
  expected_active_revision = NULL
)
```

## Arguments

- store:

  A planscanR_curation_artifact_store.

- scope:

  A planscanR_curation_scope.

- model_id:

  Immutable model version identifier.

- actor:

  Trusted authenticated actor.

- expected_active_revision:

  Required current active-reference revision; 0 means no active model.

## Value

A publication result containing `ok`, `active_model_id`, and the
resulting `active_revision`; publication is atomic.
