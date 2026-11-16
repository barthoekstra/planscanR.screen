# Build the concrete SQLite/RDS curation artifact store.

SQLite stores scoped artifact metadata, immutable revisions, and the
active model reference. Snapshot, sample, and model payloads are written
atomically as RDS files below the explicit artifact root. Model
publication validates the target and uses a compare-and-swap active
revision, so failed or stale publications leave the prior active model
usable.

## Usage

``` r
curation_artifact_store_sqlite(paths, busy_timeout = 5000L)
```

## Arguments

- paths:

  Explicit named artifact paths. `root` is required; optional
  `database`, `snapshots`, `samples`, and `models` values default to
  children of that root.

- busy_timeout:

  SQLite busy timeout in milliseconds.

## Value

A `planscanR_curation_artifact_store_sqlite`, which also implements the
generic `planscanR_curation_artifact_store` callbacks.
