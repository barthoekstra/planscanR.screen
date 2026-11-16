# Enqueue ingestion for one verified immutable file version.

The file and its current leaf receipt are checked before a durable job
is created. Repeated requests for the same file version and digest
replay the existing job.

## Usage

``` r
acquisition_queue_enqueue_ingestion(
  catalog,
  file_ref_id,
  version_id = NULL,
  priority = 0L,
  max_attempts = 3L
)
```

## Arguments

- catalog:

  An acquisition catalog.

- file_ref_id:

  Registered file reference.

- version_id:

  Optional immutable version identity; defaults to the file reference
  and may not differ from it in the current catalog schema.

- priority:

  Queue priority.

- max_attempts:

  Maximum attempts.

## Value

A durable queue job reference.
