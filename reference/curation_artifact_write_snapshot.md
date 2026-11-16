# Write a versioned corpus snapshot.

Write a versioned corpus snapshot.

## Usage

``` r
curation_artifact_write_snapshot(
  store,
  scope,
  snapshot,
  actor,
  expected_revision = NULL
)
```

## Arguments

- store:

  A planscanR_curation_artifact_store.

- scope:

  A planscanR_curation_scope.

- snapshot:

  Snapshot payload.

- actor:

  Trusted authenticated actor.

- expected_revision:

  Required current revision, or 0 for a new version.

## Value

A mutation result containing `ok`, `snapshot_id`, `version`, and the
resulting `revision`.
