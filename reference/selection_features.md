# Build the selection-model feature frame from records.

Produces a tibble carrying the record keys (`document_id`, `country`)
plus one column per feature in
[`selection_feature_names()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_feature_names.md).
Missing or non-finite numeric features are filled with `0` (an unscored
/ unclassified record reads as "no signal"), so the frame is fully
determined by the records and the feature spec — the key property that
keeps training and prediction aligned.

## Usage

``` r
selection_features(records, topics, labels, include = character(0))
```

## Arguments

- records:

  A tibble with `document_id`, `country`, and the `relevance_score_*` /
  `class_score_*` / `kw_total` columns (whatever is present; absent
  columns are treated as `0`).

- topics:

  Named topic vector naming the cosine columns (required) — the same
  vector you passed to
  [`score_records()`](https://barthoekstra.github.io/planscanR.screen/reference/score_records.md).

- labels:

  Named classifier-label vector naming the classifier columns (required)
  — the same vector you passed to
  [`classify_assessments()`](https://barthoekstra.github.io/planscanR.screen/reference/classify_assessments.md).

- include:

  Optional extra feature columns to append (off by default). Recognised:
  `"country"`, `"native_type"`. These are country-specific and will not
  transfer to an unseen portal — opt in only when training and
  predicting on the same set of countries.

## Value

A tibble of `document_id`, `country`, and the feature columns, with a
`"feature_names"` attribute.

## Details

The returned tibble carries a `"feature_names"` attribute listing the
predictor columns (everything except the two key columns), which the
trainer uses to assign recipe roles.

## Examples

``` r
if (FALSE) { # \dontrun{
recs <- index_cache(country = "nl")
X <- selection_features(recs)
attr(X, "feature_names")
} # }
```
