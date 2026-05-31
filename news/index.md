# Changelog

## planscanR.screen 0.0.0.9000

- Initial release as a standalone package. The scoring, classification,
  and selection layers were extracted from `planscanR` when fetching and
  screening were split into separate packages: a pure-R fetcher
  (`planscanR`) and this general-purpose screening framework.
- **Embedding relevance.**
  [`score_records()`](https://barthoekstra.github.io/planscanR.screen/reference/score_records.md)
  /
  [`score_assessments()`](https://barthoekstra.github.io/planscanR.screen/reference/score_assessments.md)
  score records against one or more topics by multilingual cosine
  similarity over a pluggable
  [`embedding_model()`](https://barthoekstra.github.io/planscanR.screen/reference/embedding_model.md);
  the built-in backend is
  [`embedding_model_minilm()`](https://barthoekstra.github.io/planscanR.screen/reference/embedding_model_minilm.md)
  (sentence-transformers MiniLM via reticulate). Records are embedded
  once per call, so extra topics are essentially free.
- **Zero-shot classification.**
  [`classify_assessments()`](https://barthoekstra.github.io/planscanR.screen/reference/classify_assessments.md)
  assigns per-record label probabilities over a pluggable
  [`classifier()`](https://barthoekstra.github.io/planscanR.screen/reference/classifier.md);
  the built-in
  [`classify_model_zeroshot()`](https://barthoekstra.github.io/planscanR.screen/reference/classify_model_zeroshot.md)
  runs a local multilingual NLI pipeline with device auto-detection and
  GPU batching, and persists each batch so an interrupted run is
  resumable.
- **Keyword lexicon.**
  [`score_keywords()`](https://barthoekstra.github.io/planscanR.screen/reference/score_keywords.md)
  adds a transparent multilingual substring-count signal over a
  caller-supplied lexicon.
- **Learned selection.**
  [`train_selection_model()`](https://barthoekstra.github.io/planscanR.screen/reference/train_selection_model.md)
  /
  [`predict_selection()`](https://barthoekstra.github.io/planscanR.screen/reference/predict_selection.md)
  learn the keep/drop decision from human review labels over the three
  signals, with a pluggable tidymodels
  [`selection_learner()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learner.md)
  (default logistic regression). Metrics are honest out-of-fold (k-fold
  CV);
  [`selection_learning_curve()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learning_curve.md)
  reports held-out performance as the label pool grows. The tidymodels
  glue is an optional dependency.
- **Config-agnostic by design.** No project-specific topics, labels, or
  keyword lists ship here — `classify_assessments(labels=)`,
  `score_keywords(lexicon=)`, and the selection-feature functions all
  take their vocabulary as required arguments.
- Reads and writes scores through the `planscanR` sidecar cache, so
  screening results survive across sessions and are visible to
  [`planscanR::index_cache()`](https://barthoekstra.github.io/planscanR/reference/index_cache.html).
