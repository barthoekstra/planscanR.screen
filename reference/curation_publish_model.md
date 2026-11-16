# Write and atomically publish a model through an injected C3 artifact store.

The write callback must return a model identifier (unless `model_id` is
supplied); publication is attempted only after a successful write.
Callback errors and explicit `ok = FALSE` results propagate to the
caller.

## Usage

``` r
curation_publish_model(
  store,
  scope,
  model,
  provenance,
  actor,
  expected_revision = 0L,
  expected_active_revision = 0L,
  model_id = NULL,
  config = NULL
)
```

## Arguments

- store:

  A `planscanR_curation_artifact_store`.

- scope:

  A `planscanR_curation_scope`.

- model:

  A `planscanR_selection_model`.

- provenance:

  Training and configuration provenance.

- actor:

  A trusted `planscanR_curation_actor`.

- expected_revision:

  Expected immutable-model revision, default `0`.

- expected_active_revision:

  Expected active-reference revision, default `0`.

- model_id:

  Optional model identifier when the write callback does not return one.

- config:

  Optional compatible curation configuration.

## Value

The result returned by the publication callback.
