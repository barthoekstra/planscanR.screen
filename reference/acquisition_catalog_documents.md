# List document references in the acquisition catalog.

List document references in the acquisition catalog.

## Usage

``` r
acquisition_catalog_documents(
  catalog,
  source_ref_id = NULL,
  identity_status = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- source_ref_id:

  Optional source filter.

- identity_status:

  Optional `identified`/`ambiguous` filter.

## Value

A data frame of document references.
