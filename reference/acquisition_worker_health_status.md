# Read bounded independent worker health.

`stale` and `unknown` are derived from the persisted lifecycle rows and
are never inferred from the queue's running-attempt table. When
`expected_workers` is omitted, configured identities in the catalog are
read; callers may pass trusted deployment identities to include an
unstarted worker as `unknown`.

## Usage

``` r
acquisition_worker_health_status(
  catalog,
  expected_workers = NULL,
  freshness_seconds = 30,
  now = Sys.time()
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- expected_workers:

  Optional one to 32 trusted worker identities.

- freshness_seconds:

  Maximum age of an idle/busy heartbeat in seconds.

- now:

  Observation time, primarily for deterministic tests.

## Value

A list with `workers`, bounded `counts`, and `as_of`. `workers` has
lifecycle and freshness fields but no progress or byte claims.
