# List a filtered, paginated acquisition queue projection.

List a filtered, paginated acquisition queue projection.

## Usage

``` r
acquisition_dashboard_jobs(
  catalog,
  campaign_id = NULL,
  country = NULL,
  energy = NULL,
  state = NULL,
  portal = NULL,
  search = NULL,
  page = 1L,
  page_size = 50L
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- campaign_id:

  Optional campaign filter.

- country:

  Optional source country filter.

- energy:

  Optional normalized energy-kind filter.

- state:

  Optional queue-state filter.

- portal:

  Optional source portal filter.

- search:

  Optional ID or target search string.

- page:

  One-based page number.

- page_size:

  Rows per page.

## Value

A data frame with a `total_count` attribute.
