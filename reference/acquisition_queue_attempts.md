# List durable acquisition attempts.

List durable acquisition attempts.

## Usage

``` r
acquisition_queue_attempts(catalog, job_id = NULL)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- job_id:

  Optional job filter.

## Value

A data frame of attempts ordered by job and attempt number.
