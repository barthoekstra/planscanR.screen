# Recover an exhausted download job from its existing publication receipt.

This explicit local operation never downloads or resets retry limits. It
appends a receipt-only audited attempt and uses normal lease fencing.
Active leases are refused; only failed or blocked download jobs may be
reconciled. Catalog-only sources with no implicit sidecar report
synchronization skipped.

## Usage

``` r
acquisition_queue_reconcile_job(catalog, job_id, cache_root = NULL)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- job_id:

  Failed or blocked download job ID.

- cache_root:

  Root containing the canonical published attachment.

## Value

A list with `ok`, `status`, and recovery details.
