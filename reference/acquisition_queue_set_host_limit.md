# Set one persisted shared host concurrency and cooldown limit.

Set one persisted shared host concurrency and cooldown limit.

## Usage

``` r
acquisition_queue_set_host_limit(
  catalog,
  host,
  max_concurrency = acquisition_queue_default_host_concurrency,
  cooldown_until = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- host:

  Non-empty host key.

- max_concurrency:

  Maximum number of active non-expired leases.

- cooldown_until:

  Optional UTC timestamp after which claims may resume.

## Value

A list with the persisted host limit.
