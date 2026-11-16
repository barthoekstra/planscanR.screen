# List file claims in the acquisition catalog.

List file claims in the acquisition catalog.

## Usage

``` r
acquisition_catalog_files(
  catalog,
  document_ref_id = NULL,
  verification_status = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- document_ref_id:

  Optional document filter.

- verification_status:

  Optional verification filter.

## Value

A data frame of file claims.
