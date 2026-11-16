# List campaign items and their queue state.

List campaign items and their queue state.

## Usage

``` r
acquisition_campaign_items(catalog, campaign_id)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- campaign_id:

  Campaign ID.

## Value

A data frame with source, energy, rule, and queue provenance.
