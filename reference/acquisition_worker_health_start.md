# Record a worker startup and initial idle heartbeat.

Record a worker startup and initial idle heartbeat.

## Usage

``` r
acquisition_worker_health_start(
  catalog,
  worker_id,
  expected = TRUE,
  now = Sys.time()
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- worker_id:

  Stable worker process identity.

- expected:

  Whether this identity belongs to the expected worker pool.

- now:

  Observation time, primarily for deterministic tests.

## Value

A list with the worker identity and lifecycle state.
