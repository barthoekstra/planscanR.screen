# Reprioritize pending campaign jobs without changing active leases.

Reprioritize pending campaign jobs without changing active leases.

## Usage

``` r
acquisition_campaign_reprioritize(
  catalog,
  campaign_id,
  priority_rules,
  changed_by = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- campaign_id:

  Campaign ID.

- priority_rules:

  New ordered caller-supplied rules.

- changed_by:

  Optional trusted actor identity.

## Value

A list with revision and counts of updated and active jobs.
