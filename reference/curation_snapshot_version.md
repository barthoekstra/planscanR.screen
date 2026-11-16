# Compute an invalidation-aware snapshot version.

The version combines payload content, configuration policy content,
enrichment schema, and filter coverage. Native document IDs alone never
identify a reusable snapshot.

## Usage

``` r
curation_snapshot_version(snapshot, scope, config = NULL, countries = NULL)
```

## Arguments

- snapshot:

  A snapshot data frame or serializable snapshot payload.

- scope:

  A `planscanR_curation_scope`.

- config:

  Optional `planscanR_curation_config`; metadata attached by
  [`curation_records_snapshot()`](https://barthoekstra.github.io/planscanR.screen/reference/curation_records_snapshot.md)
  is used when omitted.

- countries:

  Optional requested country filter.

## Value

One character immutable snapshot version.
