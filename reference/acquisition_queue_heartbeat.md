# Heartbeat a current, unexpired queue lease.

Heartbeat a current, unexpired queue lease.

## Usage

``` r
acquisition_queue_heartbeat(catalog, job_id, lease_token, lease_seconds = 300L)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- job_id:

  Claimed job ID.

- lease_token:

  Current attempt token.

- lease_seconds:

  New lease duration from the heartbeat time.

## Value

A list with `ok` and `status`.
