# planscanR.screen 0.0.0.9000

* Initial release as a standalone package. The scoring, classification, and
  selection layers were extracted from `planscanR` when the family split into a
  pure-R fetcher (`planscanR`), this general-purpose screening framework, and
  the BIOGAIN-specific configuration (`planscanR.biogain`).
* **Embedding relevance.** `score_records()` / `score_assessments()` score
  records against one or more topics by multilingual cosine similarity over a
  pluggable `embedding_model()`; the built-in backend is
  `embedding_model_minilm()` (sentence-transformers MiniLM via reticulate).
  Records are embedded once per call, so extra topics are essentially free.
* **Zero-shot classification.** `classify_assessments()` assigns per-record
  label probabilities over a pluggable `classifier()`; the built-in
  `classify_model_zeroshot()` runs a local multilingual NLI pipeline with device
  auto-detection and GPU batching, and persists each batch so an interrupted run
  is resumable.
* **Keyword lexicon.** `score_keywords()` adds a transparent multilingual
  substring-count signal over a caller-supplied lexicon.
* **Learned selection.** `train_selection_model()` / `predict_selection()` learn
  the keep/drop decision from human review labels over the three signals, with a
  pluggable tidymodels `selection_learner()` (default logistic regression).
  Metrics are honest out-of-fold (k-fold CV); `selection_learning_curve()`
  reports held-out performance as the label pool grows. The tidymodels glue is
  an optional dependency.
* **Config-agnostic by design.** No project-specific topics, labels, or keyword
  lists ship here — `classify_assessments(labels=)`, `score_keywords(lexicon=)`,
  and the selection-feature functions all take their vocabulary as required
  arguments. The BIOGAIN sets live in `planscanR.biogain`.
* Reads and writes scores through the `planscanR` sidecar cache, so screening
  results survive across sessions and are visible to `planscanR::index_cache()`.
