# Register an immutable model artifact reference.

The artifact is copied into a managed `models/` directory beside the
catalog before its SHA-256 reference is recorded, so the selection
reference retains a readable model even if the caller later removes the
source path.

## Usage

``` r
acquisition_catalog_register_model(
  catalog,
  model_id,
  artifact_path,
  config_id,
  input_snapshot_id,
  metadata = NULL,
  created_by = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- model_id:

  Immutable model identifier.

- artifact_path:

  Existing model artifact path.

- config_id:

  Configuration/policy identity.

- input_snapshot_id:

  Training input snapshot identity.

- metadata:

  Optional provenance metadata.

- created_by:

  Optional trusted creator ID.

## Value

A list containing the model reference and `status`.
