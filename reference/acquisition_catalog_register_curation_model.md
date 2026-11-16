# Register a published C3 model in the acquisition catalog.

The model file is copied and fingerprinted by the existing catalog
registry; this helper only connects the two immutable artifact stores.

## Usage

``` r
acquisition_catalog_register_curation_model(
  catalog,
  artifact_store,
  scope,
  model_id,
  actor,
  input_snapshot_id = NULL,
  metadata = NULL
)
```

## Arguments

- catalog:

  Acquisition catalog.

- artifact_store:

  Published curation artifact store.

- scope:

  Immutable curation scope.

- model_id:

  Published model identifier.

- actor:

  Trusted actor allowed to read the model artifact.

- input_snapshot_id:

  Optional override for the model input snapshot.

- metadata:

  Optional additional model provenance metadata.

## Value

A registered model reference.
