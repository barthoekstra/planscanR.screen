# Run the standalone acquisition dashboard.

Run the standalone acquisition dashboard.

## Usage

``` r
run_acquisition_dashboard(
  catalog,
  actor_resolver,
  request = NULL,
  actor_probe = NULL,
  preview = NULL,
  submit_fn = acquisition_campaign_submit_authenticated,
  refresh_ms = 2000L,
  page_size = 50L,
  storage_context = NULL,
  launch.browser = interactive(),
  ...
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

- storage_context:

  Optional explicit state/cache storage context.

- launch.browser:

  Passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

- ...:

  Additional arguments passed to
  [`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).

## Value

Invisibly, the result of
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html).
