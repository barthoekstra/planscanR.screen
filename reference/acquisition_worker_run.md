# Run supervised acquisition workers outside Shiny.

A worker claims one job, performs handler work outside the SQLite
transaction, and fences its completion with the lease token. The default
handlers use persisted catalog identities; tests and deployments may
inject handlers with the same `(job, catalog)` signature.

## Usage

``` r
acquisition_worker_run(
  catalog,
  worker_id = paste0("worker-", Sys.getpid()),
  max_jobs = 1L,
  lease_seconds = 300L,
  handlers = NULL,
  poll_seconds = 0,
  lock_timeout = 60,
  job_kinds = acquisition_queue_default_job_kinds,
  ingest_fn = NULL,
  embedding_model = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- worker_id:

  Stable process identity.

- max_jobs:

  Maximum number of jobs to process; defaults to one.

- lease_seconds:

  Lease duration for each claim.

- handlers:

  Optional named list with queue handlers.

- poll_seconds:

  Optional delay after an empty claim before returning.

- lock_timeout:

  Download publication lock timeout in seconds.

- job_kinds:

  Job kinds eligible for this worker. Defaults to discovery, download,
  and storage migration; ingestion requires an explicit worker.

- ingest_fn:

  Optional injected RAG `ingest_verified_file` callback.

- embedding_model:

  Optional model identifier passed to `ingest_fn`.

## Value

A list of worker outcomes, one per claimed job.
