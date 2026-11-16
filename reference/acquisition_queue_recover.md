# Recover expired leases and make them retryable or exhausted.

Recover expired leases and make them retryable or exhausted.

## Usage

``` r
acquisition_queue_recover(catalog)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

## Value

The number of expired jobs recovered.
