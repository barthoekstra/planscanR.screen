# Apply an authenticated, audited acquisition control action.

Apply an authenticated, audited acquisition control action.

## Usage

``` r
acquisition_control_apply(
  catalog,
  action,
  target_type,
  target_id,
  actor_resolver,
  request = list(),
  reason = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- action:

  One of `pause`, `resume`, `retry`, or `cancel`.

- target_type:

  Either `job` or `campaign`.

- target_id:

  Durable job or campaign ID.

- actor_resolver:

  A trusted authenticated actor resolver.

- request:

  Deployment-supplied request/session context.

- reason:

  Optional operator reason retained in the audit trail.

## Value

A list describing the authenticated action and affected jobs.
