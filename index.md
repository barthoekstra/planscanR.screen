# planscanR.screen

`planscanR.screen` screens a table of text records by how relevant they
are to the topics you care about. Give it a tibble with a `title` and
`summary` (and, optionally, a portal `category`) plus a few topic
phrases, and it scores, classifies, and — once you have human keep/drop
labels — learns which records to keep.

It is the general-purpose **screening** package of the planscanR family.
Its sibling [planscanR](https://github.com/barthoekstra/planscanR)
fetches environmental-assessment records from European portals; this
package decides which of them are worth your attention. Nothing here is
project-specific — you supply the topics, labels, and keyword lists.

## What it does

Four independent layers, each usable on its own:

| Layer | Function | What it gives you |
|----|----|----|
| **Embedding relevance** | [`score_assessments()`](https://barthoekstra.github.io/planscanR.screen/reference/score_assessments.md) / [`score_records()`](https://barthoekstra.github.io/planscanR.screen/reference/score_records.md) | A cosine-similarity score (−1…1) per topic, from a multilingual sentence-embedding model. One English topic phrase matches Dutch, German, or Danish text — no per-language translation. |
| **Zero-shot classification** | [`classify_assessments()`](https://barthoekstra.github.io/planscanR.screen/reference/classify_assessments.md) | A probability per candidate label from a local NLI model, including explicit *negative* classes that filter out look-alikes a bare cosine cutoff lets through. |
| **Keyword lexicon** | [`score_keywords()`](https://barthoekstra.github.io/planscanR.screen/reference/score_keywords.md) | A transparent multilingual substring count — the explainable counterpart to the two semantic signals. |
| **Learned selection** | [`train_selection_model()`](https://barthoekstra.github.io/planscanR.screen/reference/train_selection_model.md) / [`predict_selection()`](https://barthoekstra.github.io/planscanR.screen/reference/predict_selection.md) | A tidymodels classifier that *learns* the keep/drop decision from human review labels over the three signals above, instead of hand-tuning a rule. |

The embedding and classification backends are pluggable S3 interfaces
([`embedding_model()`](https://barthoekstra.github.io/planscanR.screen/reference/embedding_model.md),
[`classifier()`](https://barthoekstra.github.io/planscanR.screen/reference/classifier.md),
[`selection_learner()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learner.md)),
so you can swap in a different model — or a deterministic mock for
testing — without touching the callers.

## Place in the family

`planscanR.screen` builds on
[planscanR](https://github.com/barthoekstra/planscanR), the pure-R
package that fetches the records and owns the on-disk cache. This
package imports it for sidecar cache I/O — it reads records, scores
them, and writes the scores back onto the same records — and adds the
Python toolchain (via
[reticulate](https://rstudio.github.io/reticulate/)) that the fetcher
deliberately avoids. You can also use it on its own with any tibble of
text.

## Installation

``` r

# install.packages("pak")
pak::pak("barthoekstra/planscanR.screen")
```

The embedding and classification backends run small Python models
through reticulate. The package declares its Python dependencies
(`sentence-transformers`, `transformers`, `torch`, …) on load, so
reticulate provisions them on first use; no manual `py_install()` is
required. The learned-selection layer additionally needs the tidymodels
glue, which is optional:

``` r

# Only if you train a selection model:
install.packages(c("parsnip", "recipes", "rsample", "workflows"))
```

## Quick start

``` r

library(planscanR.screen)

# Records usually come from planscanR, but any tibble with title/summary works.
records <- planscanR::index_cache(country = "nl")

# 1. Score each record against one or more topics.
records <- score_assessments(
  records,
  topic = c(wind = "wind energy", solar = "solar energy")
)
records$relevance_score_wind

# 2. Layer on a zero-shot classifier and a keyword count. Both take their
#    vocabulary explicitly — there are no project defaults in this package.
labels  <- c(wind = "wind energy project", other = "unrelated to energy")
lexicon <- list(wind = c("wind", "turbine", "windpark"))
records <- classify_assessments(records, labels = labels)
records <- score_keywords(records, lexicon = lexicon)
```

### Learn a selection model

With human keep/drop labels (e.g. from a review tool), train a model
over the per-record scores instead of hand-tuning a threshold rule:

``` r

model   <- train_selection_model(records, reviews, topics = topics, labels = labels)
model$cv                                   # honest out-of-fold metrics
records <- predict_selection(model, records)  # adds select_prob + selected_model
```

The model stores its `topics`/`labels`, so
[`predict_selection()`](https://barthoekstra.github.io/planscanR.screen/reference/predict_selection.md)
rebuilds exactly the feature frame it was trained on — no train/serve
skew.

See
[`vignette("scoring")`](https://barthoekstra.github.io/planscanR.screen/articles/scoring.md)
for the end-to-end walkthrough.

## Bring your own vocabulary

This package ships no topics, labels, or keyword lists of its own — that
is what keeps it general-purpose. Every entry point that needs project
vocabulary takes it as a required argument:
`score_assessments(topic =)`, `classify_assessments(labels =)`,
`score_keywords(lexicon =)`, and the selection functions’ `topics =` /
`labels =`. Define them once and pass them through.

## License

GPL (\>= 3).
