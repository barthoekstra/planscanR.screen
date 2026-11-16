# Persisted independent acquisition worker health

Records and reads lifecycle heartbeats for the bounded independent
acquisition worker pool. Health is separate from queue attempts and does
not claim completed bytes or files.

## Usage

``` r
acquisition_worker_health_configure(catalog, worker_ids)
acquisition_worker_health_start(catalog, worker_id, expected = TRUE,
  now = Sys.time())
acquisition_worker_health_heartbeat(catalog, worker_id,
  state = c("idle", "busy"), now = Sys.time())
acquisition_worker_health_stop(catalog, worker_id,
  state = c("stopped", "error"), error_category = NULL,
  error_message = NULL, now = Sys.time())
acquisition_worker_health_status(catalog, expected_workers = NULL,
  freshness_seconds = 30, now = Sys.time())
```

## Arguments

- catalog:

  An acquisition catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- worker_ids:

  One to 32 trusted expected worker identities.

- worker_id:

  A stable worker process identity.

- expected:

  Whether the worker belongs to the expected pool.

- state:

  Lifecycle state to record.

- error_category:

  Optional bounded error category.

- error_message:

  Optional diagnostic text; credential-shaped values are redacted.

- expected_workers:

  Optional one to 32 trusted expected identities.

- freshness_seconds:

  Maximum age for an idle or busy heartbeat.

- now:

  Observation time, primarily for deterministic tests.

## Value

The mutating functions return a list describing the recorded identity
and state. `acquisition_worker_health_status()` returns a list with a
bounded `workers` data frame, named `counts`, and `as_of`. States
include `idle`, `busy`, `stopped`, `error`, derived `stale`, and
`unknown`.

## See also

[acquisition_worker_run](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_worker_run.md),
[acquisition_catalog_open](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md)
