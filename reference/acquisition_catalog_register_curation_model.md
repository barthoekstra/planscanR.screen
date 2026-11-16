# Register a published C3 curation model in the acquisition catalog

Reads the published immutable model artifact, copies it through the
existing acquisition registry, and preserves its artifact hash and input
snapshot.

## Usage

``` r
acquisition_catalog_register_curation_model(
  catalog, artifact_store, scope, model_id, actor,
  input_snapshot_id = NULL, metadata = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- artifact_store:

  A C3 curation artifact store.

- scope:

  The immutable curation scope.

- model_id:

  Published C3 model identifier.

- actor:

  Trusted authenticated actor allowed to read the artifact.

- input_snapshot_id:

  Optional override for the model input snapshot identity.

- metadata:

  Optional additional model provenance metadata.

## Value

A model reference returned by
[`acquisition_catalog_register_model()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_register_model.md).
