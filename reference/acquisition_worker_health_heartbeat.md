# Record an independent worker heartbeat.

Heartbeats contain lifecycle state only. Queue attempts and their byte
receipts remain separate records.

## Usage

``` r
acquisition_worker_health_heartbeat(
  catalog,
  worker_id,
  state = c("idle", "busy"),
  now = Sys.time()
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- worker_id:

  Stable worker process identity.

- state:

  Either `idle` or `busy`.

- now:

  Observation time, primarily for deterministic tests.

## Value

A list with the worker identity and lifecycle state.
