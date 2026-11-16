# Register an immutable selection reference.

The referenced model must already be registered. Reusing a selection ID
with different snapshot, model, or metadata is rejected.

## Usage

``` r
acquisition_catalog_register_selection(
  catalog,
  selection_id,
  input_snapshot_id,
  model_id,
  metadata = NULL,
  submitted_by = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- selection_id:

  Immutable selection ID.

- input_snapshot_id:

  Input corpus snapshot identity.

- model_id:

  Registered model ID.

- metadata:

  Selection threshold/policy metadata.

- submitted_by:

  Optional trusted actor ID.

## Value

A list with `selection_id` and `status`.
