# Run the server portion of the namespaced acquisition dashboard module.

The module only reads the injected catalog and submits authenticated
control requests. It never owns acquisition workers or curation
snapshots.

## Usage

``` r
acquisition_dashboard_server(
  id,
  catalog,
  actor_resolver,
  request = NULL,
  actor_probe = NULL,
  preview = NULL,
  submit_fn = acquisition_campaign_submit_authenticated,
  refresh_ms = 2000L,
  page_size = 50L,
  active = NULL,
  target_limit = 100L,
  storage_context = NULL
)
```

## Arguments

- id:

  Shiny module namespace.

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

- active:

  Optional function or reactive value indicating whether the Acquisition
  tab is active. Standalone dashboards are active by default.

- target_limit:

  Maximum number of job or campaign control targets shown.

- storage_context:

  Optional explicit state/cache storage context.

## Value

The value returned by
[`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html).
