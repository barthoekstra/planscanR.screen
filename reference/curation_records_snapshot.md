# Read and enrich a corpus snapshot through an injected provider.

This is a pure service boundary: it performs no RDS or cache writes. C3
can persist the returned value through
[`curation_artifact_store()`](https://barthoekstra.github.io/planscanR.screen/reference/curation_artifact_store.md)
or a test double, while a changed configuration always rebuilds the
derived columns. With the default cache provider,
`attr(snapshot, "curation_snapshot_metadata")$corpus_fingerprint` is
tagged `decoded-row-manifest-v1:<hash>`. The versioned manifest hashes
every successfully decoded complete planscanR row before projection,
records each row's `country`, `document_id`, and complete-row digest,
sorts those entries canonically, and hashes the serialized manifest. The
`v1` tag names this algorithm so later manifest changes can be
distinguished explicitly.

## Usage

``` r
curation_records_snapshot(
  provider,
  config,
  countries = NULL,
  refresh = FALSE,
  preserve_scored_features = FALSE
)

curation_build_snapshot(
  provider,
  config,
  countries = NULL,
  refresh = FALSE,
  preserve_scored_features = FALSE
)
```

## Arguments

- provider:

  A `planscanR_curation_records_provider`.

- config:

  A `planscanR_curation_config` whose corpus identity is used.

- countries:

  Optional country filter.

- refresh:

  If `TRUE`, call the provider's refresh callback.

- preserve_scored_features:

  If `TRUE`, pass the verified scored-input preservation option through
  to
  [`curation_enrich_records()`](https://barthoekstra.github.io/planscanR.screen/reference/curation_enrich_records.md).

## Value

An enriched corpus data frame.
