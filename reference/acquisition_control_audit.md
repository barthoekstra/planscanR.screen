# List audited acquisition control actions.

List audited acquisition control actions.

## Usage

``` r
acquisition_control_audit(
  catalog,
  target_type = NULL,
  target_id = NULL,
  action = NULL,
  limit = 100L
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- target_type:

  Optional `job` or `campaign` filter.

- target_id:

  Optional target filter.

- action:

  Optional action filter.

- limit:

  Maximum number of rows.

## Value

A data frame ordered newest first.
