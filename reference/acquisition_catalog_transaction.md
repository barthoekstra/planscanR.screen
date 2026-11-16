# Run a short transaction against the acquisition catalog.

`fn` receives the DBI connection. Keep callbacks free of network, model,
and long-running file work; ACQ-1b uses this seam for short claims.

## Usage

``` r
acquisition_catalog_transaction(catalog, fn)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- fn:

  Function receiving the DBI connection.

## Value

The value returned by `fn`.
