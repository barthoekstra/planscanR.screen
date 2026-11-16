# Preview and submit an immutable acquisition campaign

Preview performs no queue writes. Authenticated submission verifies the
current admin immediately before delegating to the immutable campaign
API. Attachment and file totals remain unknown until supervised
discovery runs.

## Usage

``` r
acquisition_campaign_preview(
  catalog, selection, model_id, input_snapshot_id, threshold,
  policy_version, priority_rules, artifact_path = NULL, config_id = NULL,
  model_metadata = NULL, created_by = NULL, model_input_snapshot_id = NULL,
  model_input_snapshot_version = NULL, prepared_snapshot_id = NULL,
  prepared_snapshot_version = NULL, prepared_snapshot_fingerprint = NULL,
  expected_artifact_sha256 = NULL
)

acquisition_campaign_submit_authenticated(
  catalog, selection, model_id, input_snapshot_id, threshold,
  policy_version, priority_rules, actor_resolver, request = list(),
  campaign_id = NULL, metadata = NULL, model_artifact_path = NULL,
  model_config_id = NULL, model_metadata = NULL, artifact_path = NULL,
  config_id = NULL, model_input_snapshot_id = NULL,
  model_input_snapshot_version = NULL, prepared_snapshot_id = NULL,
  prepared_snapshot_version = NULL, prepared_snapshot_fingerprint = NULL,
  expected_artifact_sha256 = NULL
)

acquisition_campaign_panel_ui(id = NULL)

acquisition_campaign_panel_server(
  id, catalog, preview, actor_resolver, request = list(),
  submit_fn = acquisition_campaign_submit_authenticated, active = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- selection:

  A data frame with portal-aware source identity and model probability
  columns.

- model_id:

  Immutable model identifier.

- input_snapshot_id:

  Immutable model input snapshot identity.

- threshold:

  Selection probability threshold.

- policy_version:

  Immutable caller policy identity.

- priority_rules:

  Ordered country and energy priority rules.

- artifact_path:

  Optional published artifact path used for read-only preview.

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

- actor_resolver:

  Trusted resolver for the current authenticated actor.

- request:

  Optional request context passed to the actor resolver.

- campaign_id:

  Optional stable campaign identifier.

- metadata:

  Optional immutable campaign metadata.

- model_artifact_path:

  Optional artifact copied and fingerprinted on submission.

- model_config_id:

  Optional model configuration identity on submission.

- preview:

  Static preview arguments or a function returning them.

- submit_fn:

  Policy adapter used for explicit submission.

- active:

  Optional function or reactive value indicating whether the Acquisition
  tab is active. Standalone panels are active by default.

- id:

  Shiny module namespace.

## Value

`acquisition_campaign_preview()` returns a read-only preview with model,
snapshot, priority, scope, exclusions, counts, and explicit unknown
attachment totals. `acquisition_campaign_submit_authenticated()` returns
the existing campaign submission result after resolving an admin. The
panel functions return Shiny UI/module values.
