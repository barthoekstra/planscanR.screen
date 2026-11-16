# Read a versioned corpus snapshot.

Read a versioned corpus snapshot.

## Usage

``` r
curation_artifact_read_snapshot(store, scope, actor, version = NULL)
```

## Arguments

- store:

  A planscanR_curation_artifact_store.

- scope:

  A planscanR_curation_scope.

- actor:

  Trusted authenticated actor.

- version:

  Optional immutable snapshot version.

## Value

`NULL` for a never-created version, or a snapshot result containing
`snapshot_id`, scope/config identity, `version`, `revision`, and its
artifact path/payload.
