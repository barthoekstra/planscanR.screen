# Submit an immutable acquisition campaign selection.

Submit an immutable acquisition campaign selection.

## Usage

``` r
acquisition_campaign_submit(
  catalog,
  selection,
  model_id,
  input_snapshot_id,
  threshold,
  policy_version,
  priority_rules,
  submitted_by = NULL,
  campaign_id = NULL,
  metadata = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- selection:

  Data frame with `source_ref_id` (or `country`, `portal`, and
  `native_project_id`); optional `selected`, probability, energy
  evidence, confidence, and manual tag columns.

- model_id:

  Registered immutable model reference.

- input_snapshot_id:

  Immutable input snapshot identity.

- threshold:

  Selection probability threshold recorded as evidence.

- policy_version:

  Caller-supplied policy version.

- priority_rules:

  Ordered caller-supplied country/energy rules.

- submitted_by:

  Optional trusted actor identity.

- campaign_id:

  Optional stable campaign ID. A deterministic ID is used when omitted,
  so replaying the same submission is idempotent.

- metadata:

  Optional additional immutable submission metadata.

## Value

A list with campaign ID, selection ID, status, and item outcomes.
