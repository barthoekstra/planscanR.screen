# Synchronize one verified catalog download into its source sidecar.

The receipt is checked before the locked sidecar callback runs. The
callback validates country, portal, native project ID, and attachment
URL against the latest sidecar row, so a same-country portal collision
is blocked without a write. Legacy claims remain unverified and cannot
enter this path.

## Usage

``` r
acquisition_queue_sync_sidecar(
  catalog,
  document_ref_id,
  result,
  cache_root = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- document_ref_id:

  Registered document reference ID.

- result:

  Verified publication result from a queue worker.

- cache_root:

  Cache root containing the published attachment.

## Value

A list with `status` and synchronization details.
