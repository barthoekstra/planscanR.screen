# Request an asynchronous storage relocation.

The operation only records a relocation and queues `migrate_storage`;
the worker must perform copy, verification, reference switch, and
retention policy explicitly. No file is moved by this function.

## Usage

``` r
acquisition_storage_enqueue_relocation(
  catalog,
  source_root_id,
  target_root_id,
  actor,
  estimated_bytes = 0
)
```

## Arguments

- catalog:

  Acquisition catalog.

- source_root_id:

  Existing source root.

- target_root_id:

  Existing target root.

- actor:

  Authenticated administrator.

- estimated_bytes:

  Expected temporary copy size.

## Value

A list containing `relocation_id` and `job_id`.
