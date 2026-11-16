# Return database-derived acquisition status, progress, and heartbeat data.

Return database-derived acquisition status, progress, and heartbeat
data.

## Usage

``` r
acquisition_dashboard_summary(
  catalog,
  project_page = 1L,
  file_page = 1L,
  job_page = 1L,
  page_size = 50L
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- project_page:

  One-based project detail page.

- file_page:

  One-based file detail page.

- job_page:

  One-based queue detail page.

- page_size:

  Rows per detail page.

## Value

A list containing project/file/stage counts, progress, heartbeats, and
explicit unknown totals.
