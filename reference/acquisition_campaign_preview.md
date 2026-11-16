# Preview an immutable model-driven campaign without writing catalog state.

Attachment and file totals remain explicitly unknown until supervised
source discovery runs. The returned `jobs` count is always zero because
preview is read-only.

## Usage

``` r
acquisition_campaign_preview(
  catalog,
  selection,
  model_id,
  input_snapshot_id,
  threshold,
  policy_version,
  priority_rules,
  artifact_path = NULL,
  config_id = NULL,
  model_metadata = NULL,
  created_by = NULL,
  model_input_snapshot_id = NULL,
  model_input_snapshot_version = NULL,
  prepared_snapshot_id = NULL,
  prepared_snapshot_version = NULL,
  prepared_snapshot_fingerprint = NULL,
  expected_artifact_sha256 = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- selection:

  Source rows carrying portal-aware identity and probability.

- model_id:

  Registered immutable model identifier.

- input_snapshot_id:

  Model input snapshot identifier.

- threshold:

  Selection probability threshold.

- policy_version:

  Immutable caller policy identity.

- priority_rules:

  Ordered country/energy rules.

- artifact_path:

  Optional model artifact path for an unregistered preview.

- config_id:

  Optional model configuration identity for an unregistered preview.

- model_metadata:

  Optional model provenance metadata.

- created_by:

  Optional model creator identity.

- model_input_snapshot_id:

  Optional model training snapshot identity.

- model_input_snapshot_version:

  Optional model training snapshot version.

- prepared_snapshot_id:

  Optional prepared acquisition snapshot identity.

- prepared_snapshot_version:

  Optional prepared acquisition snapshot version.

- prepared_snapshot_fingerprint:

  Optional prepared acquisition snapshot fingerprint.

- expected_artifact_sha256:

  Optional digest captured before a later submit.

## Value

A read-only preview list containing provenance, items, exclusions,
counts, and explicit unknown attachment totals.
