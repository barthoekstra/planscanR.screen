# Record a graceful stop or worker error.

Error text is redacted and bounded before persistence. A process that is
killed without calling this function remains `stale` at the next read.

## Usage

``` r
acquisition_worker_health_stop(
  catalog,
  worker_id,
  state = c("stopped", "error"),
  error_category = NULL,
  error_message = NULL,
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

  Either `stopped` or `error`.

- error_category:

  Optional bounded diagnostic category.

- error_message:

  Optional diagnostic text; credential-shaped values are redacted before
  persistence.

- now:

  Observation time, primarily for deterministic tests.

## Value

A list with the worker identity and lifecycle state.
