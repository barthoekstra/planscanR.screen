# Create a configurable curation Shiny application.

All corpus, review, artifact, identity, and translation boundaries are
supplied by the caller. The app resolves its trusted actor at session
start, rechecks it before mutations, and uses an optional non-touching
probe to close an expired socket; display names are informational and
cannot select a reviewer.

## Usage

``` r
curation_app(
  config,
  records_provider,
  review_repository,
  artifact_store,
  actor_resolver,
  request = NULL,
  actor_probe = NULL,
  logout_path = NULL,
  acquisition = NULL,
  historical_reviews = NULL,
  input_provenance = NULL,
  cohort_receipt = NULL
)
```

## Arguments

- config:

  A `planscanR_curation_config`.

- records_provider:

  A `planscanR_curation_records_provider`.

- review_repository:

  A `planscanR_curation_review_repository`.

- artifact_store:

  A `planscanR_curation_artifact_store` used for sample, snapshot, and
  model seams.

- actor_resolver:

  A trusted `planscanR_curation_actor_resolver`.

- request:

  Optional deployment request value or function accepting `session`; it
  is passed to the actor resolver.

- actor_probe:

  Optional non-touching actor resolver used by the periodic
  socket-expiry check. Mutation paths always use `actor_resolver`.

- logout_path:

  Optional local POST path rendered as a visible Log out control, for
  example `/app/logout`. It is absent by default.

- acquisition:

  Optional injected acquisition service. A catalog can be supplied
  directly, or a list with `catalog`, `available`, and optional module
  settings. It is absent by default.

- historical_reviews:

  Optional data frame of imported historical review linkage and
  attribution rows. Unresolved rows are shown read-only.

- input_provenance:

  Optional BIOGAIN or caller-supplied identity for the compatible scored
  input. The app adds the loaded snapshot identity and content
  fingerprint before training.

- cohort_receipt:

  Optional immutable historical-cohort receipt carried into the model
  and publication provenance.

## Value

A `shiny.appobj`.
