# Names of the features the selection model is trained on.

The default set is the three numeric relevance signals already persisted
on every sidecar: one cosine score per BIOGAIN topic
(`relevance_score_<slug>`), one zero-shot classifier score per candidate
label (`class_score_<slug>`), and the keyword total (`kw_total`).

## Usage

``` r
selection_feature_names(topics, labels, include = character(0))
```

## Arguments

- topics:

  Named topic vector naming the cosine columns (required). The BIOGAIN
  set is `biogain_assessment_topics()` in `planscanR.biogain`.

- labels:

  Named classifier-label vector naming the classifier columns
  (required). The BIOGAIN set is `biogain_classification_labels()` in
  `planscanR.biogain`.

- include:

  Optional extra feature columns to append (off by default). Recognised:
  `"country"`, `"native_type"`. These are country-specific and will not
  transfer to an unseen portal — opt in only when training and
  predicting on the same set of countries.

## Value

A character vector of feature column names, in a stable order.

## Examples

``` r
if (FALSE) { # \dontrun{
selection_feature_names(topics, labels)
selection_feature_names(topics, labels, include = "country")
} # }
```
