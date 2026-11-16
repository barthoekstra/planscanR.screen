# Configure the expected independent acquisition workers.

The generic registry is bounded to 32 identities. Missing identities are
represented as `unknown` until they publish a lifecycle heartbeat.

## Usage

``` r
acquisition_worker_health_configure(catalog, worker_ids)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- worker_ids:

  One to 32 stable worker process identities.

## Value

A list containing the configured `worker_ids`.
