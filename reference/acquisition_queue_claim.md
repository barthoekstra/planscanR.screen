# Claim one available job with a unique expiring lease token.

The transaction only selects and records state. Worker work must happen
after this function returns.

## Usage

``` r
acquisition_queue_claim(
  catalog,
  worker_id,
  lease_seconds = 300L,
  kinds = acquisition_queue_default_job_kinds
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- worker_id:

  Stable worker process identity.

- lease_seconds:

  Lease duration.

## Value

A claim row with `ok`, `job_id`, `attempt_no`, `lease_token`, and job
fields, or `ok = FALSE` when no claim is available.
