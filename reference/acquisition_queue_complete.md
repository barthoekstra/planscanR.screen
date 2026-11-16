# Complete a current queue attempt, fenced by token and lease expiry.

A stale worker receives `ok = FALSE` and cannot change job state or file
claims. Optional file fields in `result` are registered in the same
short transaction only after the worker's network/file work has
finished.

## Usage

``` r
acquisition_queue_complete(
  catalog,
  job_id,
  lease_token,
  status = "succeeded",
  result = NULL,
  error_category = NULL,
  error_message = NULL,
  retry_after_seconds = NULL,
  cooldown_seconds = NULL,
  outcome = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- job_id:

  Claimed job ID.

- lease_token:

  Current attempt token.

- status:

  Terminal or retry status.

- result:

  Optional handler result list.

- error_category:

  Optional durable error category.

- error_message:

  Optional durable error message.

- retry_after_seconds:

  Optional retry delay, including a server Retry-After value.

- cooldown_seconds:

  Optional shared host cooldown.

- outcome:

  Optional alias for `status`.

## Value

A list with `ok`, `status`, and the resulting job state.
