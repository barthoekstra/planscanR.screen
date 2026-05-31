# Package index

## Relevance scoring

Score records by semantic similarity to one or more topics, over a
pluggable embedding-model interface.

- [`score_assessments()`](https://barthoekstra.github.io/planscanR.screen/reference/score_assessments.md)
  : Re-score an existing planscanR result tibble against (additional)
  topics.
- [`score_records()`](https://barthoekstra.github.io/planscanR.screen/reference/score_records.md)
  : Compute relevance scores for a set of records against one or more
  topics.
- [`embedding_model()`](https://barthoekstra.github.io/planscanR.screen/reference/embedding_model.md)
  : Build a custom planscanR embedding model.
- [`embedding_model_minilm()`](https://barthoekstra.github.io/planscanR.screen/reference/embedding_model_minilm.md)
  : Build an embedding model backed by sentence-transformers.
- [`embed_text()`](https://barthoekstra.github.io/planscanR.screen/reference/embed_text.md)
  : Embed a character vector with a planscanR embedding model.
- [`model_name()`](https://barthoekstra.github.io/planscanR.screen/reference/model_name.md)
  : Identify a model with a stable name string for logging / sidecars.
- [`supported_languages()`](https://barthoekstra.github.io/planscanR.screen/reference/supported_languages.md)
  : Report which ISO-639-1 languages a model has been trained on.
- [`reset_embedding_cache()`](https://barthoekstra.github.io/planscanR.screen/reference/reset_embedding_cache.md)
  : Reset the cached Python model for testing or to free memory.
- [`reset_relevance_warnings()`](https://barthoekstra.github.io/planscanR.screen/reference/reset_relevance_warnings.md)
  : Reset the relevance-model warning ledger for the current session.

## Classification

Assign classes to records with a zero-shot model, over a pluggable
classifier interface. Labels are caller-supplied (no project defaults).

- [`classify_assessments()`](https://barthoekstra.github.io/planscanR.screen/reference/classify_assessments.md)
  : Classify assessment records with a zero-shot model.
- [`classifier()`](https://barthoekstra.github.io/planscanR.screen/reference/classifier.md)
  : Build a custom planscanR classifier.
- [`classify_model_zeroshot()`](https://barthoekstra.github.io/planscanR.screen/reference/classify_model_zeroshot.md)
  : Local zero-shot classifier backed by a HuggingFace NLI model.
- [`classify_text()`](https://barthoekstra.github.io/planscanR.screen/reference/classify_text.md)
  : Classify a character vector against candidate labels.
- [`classifier_name()`](https://barthoekstra.github.io/planscanR.screen/reference/classifier_name.md)
  : Identify a classifier with a stable name string for sidecars /
  logging.
- [`reset_classifier_cache()`](https://barthoekstra.github.io/planscanR.screen/reference/reset_classifier_cache.md)
  : Evict cached zero-shot pipeline(s) to free memory or for tests.

## Keyword scoring

A transparent lexical (substring) signal over a caller-supplied lexicon.

- [`score_keywords()`](https://barthoekstra.github.io/planscanR.screen/reference/score_keywords.md)
  : Score records against the keyword lexicon.

## Learned selection model

Learn the keep/drop decision from human review labels over the
per-record scores, with a pluggable tidymodels learner.

- [`selection_features()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_features.md)
  : Build the selection-model feature frame from records.
- [`selection_feature_names()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_feature_names.md)
  : Names of the features the selection model is trained on.
- [`selection_learner()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learner.md)
  : Construct a custom selection learner.
- [`selection_learner_logistic()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learners_builtin.md)
  : Built-in selection learner.
- [`selection_learners()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learners.md)
  : Registry of the built-in learners.
- [`train_selection_model()`](https://barthoekstra.github.io/planscanR.screen/reference/train_selection_model.md)
  : Train the learned selection model from human review labels.
- [`predict_selection()`](https://barthoekstra.github.io/planscanR.screen/reference/predict_selection.md)
  : Predict the learned selection decision for records.
- [`selection_cv_metrics()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_cv_metrics.md)
  : Out-of-fold metrics for a trained model at an arbitrary threshold.
- [`selection_learning_curve()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learning_curve.md)
  : Learning curve for the learned selection model.
- [`learning_curve_summary()`](https://barthoekstra.github.io/planscanR.screen/reference/learning_curve_summary.md)
  : Aggregate a learning curve to mean +/- sd per training size.
- [`consensus_reviews()`](https://barthoekstra.github.io/planscanR.screen/reference/consensus_reviews.md)
  : Resolve a review store to one agreed decision per record.
- [`save_selection_model()`](https://barthoekstra.github.io/planscanR.screen/reference/save_selection_model.md)
  [`load_selection_model()`](https://barthoekstra.github.io/planscanR.screen/reference/save_selection_model.md)
  : Persist / restore a trained selection model.

## Package

- [`planscanR.screen`](https://barthoekstra.github.io/planscanR.screen/reference/planscanR.screen-package.md)
  [`planscanR.screen-package`](https://barthoekstra.github.io/planscanR.screen/reference/planscanR.screen-package.md)
  : planscanR.screen: score, classify, and select text records by topic
  relevance
