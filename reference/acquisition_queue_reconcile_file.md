# Reconcile a verified catalog file and synchronize its source sidecar.

Only `planscan-cache` locations are resolved by this compact service. A
legacy or unverified claim is reported without promoting its bytes.

## Usage

``` r
acquisition_queue_reconcile_file(catalog, file_ref_id, cache_root = NULL)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- file_ref_id:

  Registered file reference ID.

- cache_root:

  Cache root containing the published attachment.

## Value

A list with `status` and reconciliation details.
