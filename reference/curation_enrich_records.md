# Enrich a corpus frame with the scalar curation signals.

`config` is the only source of topic and keyword policy. Existing
derived keyword columns are removed before scoring, so re-enriching a
frame with a new configuration cannot retain a previous lexicon's
totals. Raw sidecar scores for the configured topic names are reused;
this function does not invoke an embedding or classifier backend.

## Usage

``` r
curation_enrich_records(
  records,
  config,
  drop_list_columns = TRUE,
  preserve_scored_features = FALSE
)
```

## Arguments

- records:

  A data frame with non-empty `country` and `document_id` columns. It
  may contain planscanR list columns such as `attachment_urls` and
  `download_status`.

- config:

  A `planscanR_curation_config` supplying `topics` and `lexicon`.

- drop_list_columns:

  If `TRUE`, remove UI-unfriendly attachment, download, discovery, and
  local-path list columns after deriving counts.

- preserve_scored_features:

  If `TRUE`, require the configured scored feature columns before
  enrichment and restore their exact values, including `NA`, after
  enrichment. This is the opt-in path for a verified scored-input
  context; the default retains ordinary keyword recomputation.

## Value

The enriched input frame with `cosine_max`, keyword scores,
attachment/download counts, and classifier fallback columns.
