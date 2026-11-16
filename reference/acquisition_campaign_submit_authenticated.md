# Submit a campaign after resolving the current authenticated admin.

The resolver is evaluated at mutation time, so a reviewer or revoked
actor cannot reuse a previously rendered preview to submit work.

## Usage

``` r
acquisition_campaign_submit_authenticated(
  catalog,
  selection,
  model_id,
  input_snapshot_id,
  threshold,
  policy_version,
  priority_rules,
  actor_resolver,
  request = list(),
  campaign_id = NULL,
  metadata = NULL,
  model_artifact_path = NULL,
  model_config_id = NULL,
  model_metadata = NULL,
  artifact_path = NULL,
  config_id = NULL,
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

  Acquisition catalog.

- selection:

  Selection data frame carrying source identity and scores.

- model_id:

  Registered immutable model identifier.

- input_snapshot_id:

  Immutable input snapshot identity.

- threshold:

  Selection probability threshold recorded as evidence.

- policy_version:

  Caller-supplied policy version.

- priority_rules:

  Ordered caller-supplied country/energy rules.

- actor_resolver:

  Trusted resolver for the current authenticated actor.

- request:

  Optional request context passed to the actor resolver.

- campaign_id:

  Optional stable campaign identifier.

- metadata:

  Optional additional immutable campaign metadata.

- model_artifact_path:

  Optional artifact path used during registration.

- model_config_id:

  Optional model configuration identity.

- model_metadata:

  Optional model provenance metadata.

- artifact_path:

  Alias for `model_artifact_path`.

- config_id:

  Alias for `model_config_id`.

- model_input_snapshot_id:

  Optional model training snapshot identity when the campaign uses a
  separate prepared snapshot.

- model_input_snapshot_version:

  Optional model training snapshot version.

- prepared_snapshot_id:

  Optional prepared acquisition snapshot identity.

- prepared_snapshot_version:

  Optional prepared acquisition snapshot version.

- prepared_snapshot_fingerprint:

  Optional prepared acquisition snapshot fingerprint.

- expected_artifact_sha256:

  Digest captured by preview and checked before model registration or
  campaign writes.
