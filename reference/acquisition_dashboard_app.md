# Build the standalone authenticated acquisition dashboard Shiny app.

The app owns only read projections and authenticated control requests.
It never starts or stops acquisition workers, so closing a browser
session does not affect worker continuity.

## Usage

``` r
acquisition_dashboard_app(
  catalog,
  actor_resolver,
  request = NULL,
  actor_probe = NULL,
  preview = NULL,
  submit_fn = acquisition_campaign_submit_authenticated,
  refresh_ms = 2000L,
  page_size = 50L,
  target_limit = 100L,
  storage_context = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- actor_resolver:

  A trusted authenticated actor resolver.

- request:

  Optional deployment request or function accepting `session`.

- actor_probe:

  Optional non-touching resolver used for session checks.

- preview:

  Optional campaign preview input passed to the acquisition campaign
  panel.

- submit_fn:

  Authenticated campaign submission function.

- refresh_ms:

  Database refresh interval in milliseconds.

- page_size:

  Queue rows shown per refresh.

- target_limit:

  Maximum number of control targets shown.

- storage_context:

  Optional explicit state/cache storage context.

## Value

A `shiny.appobj`.
