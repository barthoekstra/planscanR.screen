# Compute relevance scores for a set of records against one or more topics.

Cosine similarity between each topic's embedding and each record's
title + summary embedding. Records are embedded **once** per call
regardless of how many topics are passed, so adding extra topics is
essentially free.

## Usage

``` r
score_records(records, topic, model, text_fn = NULL)
```

## Arguments

- records:

  A tibble; must include at least `country`, `title`, `summary`.

- topic:

  A character vector of topic phrases. Pass a named vector to control
  the column-suffix slugs; unnamed elements get auto-slugified from
  their phrase. See examples.

- model:

  A `planscanR_embedding_model` object.

- text_fn:

  Optional function `function(record) -> character` that builds the text
  to embed for each record. Default concatenates `title` and `summary`.

## Value

The input `records` tibble with one `relevance_score_<slug>` column per
topic plus a single `relevance_model` column.

## Details

Emits a one-shot warning if any record's country language falls outside
`supported_languages(model)`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Single topic — column will be relevance_score_wind_energy
score_records(recs, "wind energy", em)

# Multiple topics in one pass — column names will be relevance_score_wind etc.
score_records(
  recs,
  c(wind  = "wind energy",
    solar = "solar energy",
    res   = "regional energy transition strategy and planning"),
  em
)
} # }
```
