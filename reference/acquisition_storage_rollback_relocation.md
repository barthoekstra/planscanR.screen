# Roll back migrated sidecar references to the retained verified source copies

Rollback only switches references and advances the preferred manifest
location back to the verified source. It never removes the target object
or any unverified file.

## Usage

``` r
acquisition_storage_rollback_relocation(
  catalog,
  relocation_id,
  actor,
  cache_root = NULL
)
```

## Arguments

- catalog:

  Acquisition catalog.

- relocation_id:

  Relocation ID returned by
  [`acquisition_storage_enqueue_relocation()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_storage_enqueue_relocation.md).

- actor:

  Authenticated administrator.

- cache_root:

  Cache root for sidecars and portable project manifests.

## Value

A list with `status` and the count of references restored.
