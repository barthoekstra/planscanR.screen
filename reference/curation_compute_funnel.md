# Compute configured funnel-stage counts.

Compute configured funnel-stage counts.

## Usage

``` r
curation_compute_funnel(config, records, context = list())
```

## Arguments

- config:

  A `planscanR_curation_config`.

- records:

  Records after selection/model enrichment.

- context:

  Optional per-call context passed to each stage predicate.

## Value

A data frame with `order`, `stage`, `key`, `n`, and `pct` columns.
