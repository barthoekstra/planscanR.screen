# Open the local SQLite account/session store.

The path must be a file outside the corpus cache and protected workspace
data root. No account or secret is created by this operation.

## Usage

``` r
curation_auth_open(
  path,
  busy_timeout = 5000L,
  max_failures = 5L,
  global_max_failures = 20L,
  failure_window = 300L,
  block_seconds = 300L,
  session_ttl = 43200L,
  idle_timeout = 3600L
)
```

## Arguments

- path:

  Explicit local SQLite path for accounts and sessions.

- busy_timeout:

  Bounded SQLite busy timeout in milliseconds.

- max_failures:

  Failed attempts allowed in one rate-limit window.

- global_max_failures:

  Failed attempts allowed in the persistent global rate-limit bucket.
  Unknown usernames use this bucket without creating unbounded per-name
  rows.

- failure_window:

  Rate-limit window in seconds.

- block_seconds:

  Block duration after the failure threshold.

- session_ttl:

  Absolute session lifetime in seconds.

- idle_timeout:

  Maximum idle session lifetime in seconds.

## Value

A `planscanR_curation_auth` store with a
[`close()`](https://rdrr.io/r/base/connections.html) method.
