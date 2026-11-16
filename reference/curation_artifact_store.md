# Build an injected artifact-store contract.

The store receives explicit scope, actor, and revision values. Model
payloads retain the existing planscanR selection-model serialization; C3
decides how immutable versions and active references are persisted under
these paths.

## Usage

``` r
curation_artifact_store(
  paths,
  snapshot_read_fn,
  snapshot_write_fn,
  sample_list_fn,
  sample_read_fn,
  sample_write_fn,
  model_list_fn,
  model_read_fn,
  model_write_fn,
  model_publish_fn,
  active_model_fn
)
```

## Arguments

- paths:

  Explicit named artifact paths, including root.

- snapshot_read_fn:

  Function accepting scope, version, and actor.

- snapshot_write_fn:

  Function accepting scope, snapshot, actor, and expected_revision.

- sample_list_fn:

  Function accepting scope, reviewer_id, and actor.

- sample_read_fn:

  Function accepting scope, sample_id, and actor.

- sample_write_fn:

  Function accepting scope, sample, reviewer_id, actor, and
  expected_revision.

- model_list_fn:

  Function accepting scope and actor.

- model_read_fn:

  Function accepting scope, model_id, and actor.

- model_write_fn:

  Function accepting scope, model, provenance, actor, and
  expected_revision.

- model_publish_fn:

  Function accepting scope, model_id, actor, and
  expected_active_revision.

- active_model_fn:

  Function accepting scope and actor.

## Value

A planscanR_curation_artifact_store object.
