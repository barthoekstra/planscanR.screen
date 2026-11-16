# Enqueue one durable acquisition job.

Repeating the same target is idempotent for discovery and download jobs;
relocation jobs use their source/target idempotency key. The target must
already be an identity in the acquisition catalog.

## Usage

``` r
acquisition_queue_enqueue(
  catalog,
  kind,
  target_type,
  target_ref_id,
  host = NULL,
  priority = 0L,
  max_attempts = 3L,
  delay_seconds = 0,
  idempotency_key = NULL,
  estimated_bytes = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- kind:

  `discover`, `download`, `migrate_storage`, or `ingest`.

- target_type:

  `source`, `document`, `file`, or `storage_root`.

- target_ref_id:

  Persisted source, document, file, or storage-root ID.

- host:

  Optional shared host throttle key.

- priority:

  Integer priority; larger values claim first.

- max_attempts:

  Bounded number of worker attempts.

- delay_seconds:

  Delay before the first claim.

- idempotency_key:

  Optional caller key. The persisted target identity remains unique even
  when a caller key is supplied.

- estimated_bytes:

  Optional transfer-size estimate used by storage reservations.

## Value

A list containing `job_id` and `status`.
