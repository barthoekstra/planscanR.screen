# Resolve a review store to one agreed decision per record.

Collapses the per-(record, reviewer) review rows into a single
ground-truth decision per `(country, document_id)`, in two steps:

## Usage

``` r
consensus_reviews(reviews, decisions = c("keep", "drop"))
```

## Arguments

- reviews:

  The review-decision tibble (e.g. a review tool's `reviews.csv`), with
  `document_id`, `country`, `decision`, `reviewer`, `reviewed_at`.

- decisions:

  Decisions treated as a verdict. Default `c("keep", "drop")` (so
  `"unsure"` rows are ignored).

## Value

A tibble with one row per agreed record: `document_id`, `country`,
`decision`, `n_reviewers`, `reviewed_at` (the latest among the agreeing
reviewers). Empty if no record has an agreed decision.

## Details

1.  **Per reviewer:** keep each reviewer's *most-recent* verdict on a
    record (so re-deciding overrides an earlier opinion).

2.  **Across reviewers:** keep the record only if all its reviewers
    **unanimously agree**. Records with conflicting decisions are
    dropped as ambiguous ground truth.

Records reviewed by a single reviewer trivially pass. This is the rule
used for both the training labels
([`train_selection_model()`](https://barthoekstra.github.io/planscanR.screen/reference/train_selection_model.md))
and the automated-vs-human comparison, so a disagreement never silently
becomes a label.

## Examples

``` r
if (FALSE) { # \dontrun{
rev <- read.csv("reviews.csv", colClasses = "character")
consensus_reviews(rev)
} # }
```
