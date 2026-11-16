# List durable acquisition jobs.

List durable acquisition jobs.

## Usage

``` r
acquisition_queue_jobs(catalog, state = NULL)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- state:

  Optional state filter.

## Value

A data frame of jobs ordered for inspection.
