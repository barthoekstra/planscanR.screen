# Cancel pending work or request a fenced safe stop for a running job.

Cancel pending work or request a fenced safe stop for a running job.

## Usage

``` r
acquisition_control_cancel(
  catalog,
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
