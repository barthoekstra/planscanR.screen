# Run the optional Acquisition campaign preview/submission module.

Run the optional Acquisition campaign preview/submission module.

## Usage

``` r
acquisition_campaign_panel_server(
  id,
  catalog,
  preview,
  actor_resolver,
  request = list(),
  submit_fn = acquisition_campaign_submit_authenticated,
  active = NULL
)
```

## Arguments

- id:

  Shiny module namespace.

- catalog:

  Acquisition catalog.

- preview:

  Static preview arguments or a function returning them.

- actor_resolver:

  Trusted actor resolver.

- request:

  Optional request context.

- submit_fn:

  Optional policy adapter; defaults to the generic authenticated
  submitter.

- active:

  Optional function or reactive value indicating whether the Acquisition
  tab is active. Standalone panels are active by default.

## Value

Invisibly, `NULL`.
