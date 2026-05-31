# Scoring records by topic relevance

`planscanR.screen` scores a tibble of text records by how relevant they
are to topics you care about. It is the general-purpose screening
framework of the planscanR family: records usually come from
[`planscanR::get_assessments()`](https://barthoekstra.github.io/planscanR/reference/get_assessments.html)
or
[`planscanR::index_cache()`](https://barthoekstra.github.io/planscanR/reference/index_cache.html),
but any tibble with `title` / `summary` columns works.

## Score by relevance

Pass a `topic` and each record’s title and summary are compared against
it with a multilingual embedding model, producing a relevance score
between -1 and 1 (higher means a closer match). A topic is just a short
phrase; pass one, or several named ones:

``` r

records <- planscanR::index_cache(country = "nl")
records <- score_assessments(
  records,
  topic = c(wind = "wind energy", solar = "solar energy")
)
```

This adds `relevance_score_wind` and `relevance_score_solar` columns.
Because the comparison uses a multilingual model, an English topic
phrase still matches Dutch or German text — you do not need to translate
your topics per country.

The embedding model is pluggable. The built-in backend is
[`embedding_model_minilm()`](https://barthoekstra.github.io/planscanR.screen/reference/embedding_model_minilm.md)
(sentence-transformers MiniLM, via reticulate); wrap any embedding
function with
[`embedding_model()`](https://barthoekstra.github.io/planscanR.screen/reference/embedding_model.md)
to use a different one.

> **One-time setup.** The MiniLM backend runs a small Python model
> through reticulate; the package declares `sentence-transformers` via
> [`reticulate::py_require()`](https://rstudio.github.io/reticulate/reference/py_require.html)
> on load, so it is provisioned on first use.

## Classify and keyword-score

Two complementary signals layer on top of the cosine score. Both take
their vocabulary as an explicit argument — the framework ships no
project defaults, so you define the candidate labels and keyword terms
yourself:

``` r

# Zero-shot classification against a labelled set of candidate classes. Names
# are stable slugs (used as column suffixes); values are the hypotheses fed to
# the model. Include explicit negative classes to catch look-alikes.
labels <- c(
  wind  = "wind energy project",
  solar = "solar energy project",
  other = "not about renewable energy"
)
records <- classify_assessments(records, labels = labels)

# A transparent lexical (substring) count over a multilingual lexicon: a named
# list of term vectors, one per topic.
lexicon <- list(
  wind  = c("wind", "turbine", "windpark", "windmolen"),
  solar = c("solar", "zonne", "photovolta", "pv")
)
records <- score_keywords(records, lexicon = lexicon)
```

## Learn a selection model

Given human keep/drop labels (e.g. exported from a review tool), you can
train a selection model over the per-record scores instead of
hand-tuning a rule. Pass the same `topics` and `labels` you scored and
classified with, so the feature columns line up:

``` r

model <- train_selection_model(
  records,
  reviews,
  topics = c(wind = "wind energy", solar = "solar energy"),
  labels = labels
)
records <- predict_selection(model, records)
```

The model stores its `topics`/`labels` so
[`predict_selection()`](https://barthoekstra.github.io/planscanR.screen/reference/predict_selection.md)
rebuilds exactly the same feature frame it was trained on (no
train/serve skew).

## Where to go next

- [`?score_assessments`](https://barthoekstra.github.io/planscanR.screen/reference/score_assessments.md),
  [`?score_records`](https://barthoekstra.github.io/planscanR.screen/reference/score_records.md)
  — relevance scoring options.
- [`?classify_assessments`](https://barthoekstra.github.io/planscanR.screen/reference/classify_assessments.md),
  [`?classify_model_zeroshot`](https://barthoekstra.github.io/planscanR.screen/reference/classify_model_zeroshot.md)
  — the classifier.
- [`?train_selection_model`](https://barthoekstra.github.io/planscanR.screen/reference/train_selection_model.md),
  [`?selection_features`](https://barthoekstra.github.io/planscanR.screen/reference/selection_features.md)
  — the learned selection model.
- [`?embedding_model`](https://barthoekstra.github.io/planscanR.screen/reference/embedding_model.md),
  [`?classifier`](https://barthoekstra.github.io/planscanR.screen/reference/classifier.md),
  [`?selection_learner`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learner.md)
  — plug in your own backends.
