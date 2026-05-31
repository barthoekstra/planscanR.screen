# AGENTS.md — planscanR.screen orientation

Written for AI agents and human contributors landing in this repo cold.
`planscanR.screen` is the general-purpose **scoring / classification /
selection** framework of the planscanR family.

## What this package is

A config-agnostic toolkit for screening a tibble of text records by
topic relevance. Four layers, each independently usable:

- **Embedding relevance** — multilingual cosine similarity between topic
  phrases and each record’s text
  ([R/relevance_model.R](https://barthoekstra.github.io/planscanR.screen/R/relevance_model.R),
  MiniLM backend in
  [R/relevance_minilm.R](https://barthoekstra.github.io/planscanR.screen/R/relevance_minilm.R)).
- **Zero-shot classification** — per-record probabilities over candidate
  labels via a local HuggingFace NLI pipeline
  ([R/classify_zeroshot.R](https://barthoekstra.github.io/planscanR.screen/R/classify_zeroshot.R),
  driver in
  [R/classify_assessments.R](https://barthoekstra.github.io/planscanR.screen/R/classify_assessments.R)).
- **Keyword lexicon** — a transparent multilingual substring count layer
  ([R/score_keywords.R](https://barthoekstra.github.io/planscanR.screen/R/score_keywords.R)).
- **Learned selection** — a tidymodels model that learns the keep/drop
  decision from human review labels over the three signals above
  ([R/select_features.R](https://barthoekstra.github.io/planscanR.screen/R/select_features.R),
  [R/select_learner.R](https://barthoekstra.github.io/planscanR.screen/R/select_learner.R),
  [R/train_selection.R](https://barthoekstra.github.io/planscanR.screen/R/train_selection.R),
  [R/select_learning_curve.R](https://barthoekstra.github.io/planscanR.screen/R/select_learning_curve.R)).

It is built for the planscanR family (environmental-assessment records)
but works with any tibble carrying `country`, `title`, `summary` (and,
where the selection model is used, `document_id` plus the scored
columns). It brings the **Python toolchain** (via `reticulate`) that the
leaf `planscanR` fetcher deliberately avoids.

## Place in the family

Three sibling repos under `biogain-tools/`, dependency direction
left→right:

    planscanR  ←──  planscanR.screen  ←──  planscanR.biogain
      (leaf)        (THIS package)          (BIOGAIN config)

- `planscanR` — outward-facing leaf: fetches records, owns the cache +
  sidecar JSON schema. Pure-R, no Python. See
  [../planscanR/AGENTS.md](https://barthoekstra.github.io/planscanR/AGENTS.md).
- **`planscanR.screen`** — Imports `planscanR`; adds `reticulate` and
  (Suggests) the tidymodels glue. Config-agnostic.
- `planscanR.biogain` — Imports both; holds the BIOGAIN
  topics/labels/lexicon, ensemble select rule, review Shiny app, Yoda
  sync, runbook. Defer to its
  [../planscanR.biogain/AGENTS.md](https://barthoekstra.github.io/planscanR.biogain/AGENTS.md)
  for project specifics.

Parent-level orientation:
[../CLAUDE.md](https://barthoekstra.github.io/CLAUDE.md).

## Architecture / key files

- [R/relevance_model.R](https://barthoekstra.github.io/planscanR.screen/R/relevance_model.R)
  — the
  [`embedding_model()`](https://barthoekstra.github.io/planscanR.screen/reference/embedding_model.md)
  S3 constructor + the
  [`embed_text()`](https://barthoekstra.github.io/planscanR.screen/reference/embed_text.md)
  /
  [`model_name()`](https://barthoekstra.github.io/planscanR.screen/reference/model_name.md)
  /
  [`supported_languages()`](https://barthoekstra.github.io/planscanR.screen/reference/supported_languages.md)
  generics;
  [`score_records()`](https://barthoekstra.github.io/planscanR.screen/reference/score_records.md)
  (cosine, embeds docs once per call) and
  [`score_assessments()`](https://barthoekstra.github.io/planscanR.screen/reference/score_assessments.md)
  (sidecar-writing wrapper); pure helpers `normalise_topics` /
  `slugify_topic` / `cosine_similarity_matrix`; the one-shot
  language-coverage warning ledger.
- [R/relevance_minilm.R](https://barthoekstra.github.io/planscanR.screen/R/relevance_minilm.R)
  —
  [`embedding_model_minilm()`](https://barthoekstra.github.io/planscanR.screen/reference/embedding_model_minilm.md)
  (sentence-transformers `paraphrase-multilingual-MiniLM-L12-v2`),
  session model cache,
  [`reset_embedding_cache()`](https://barthoekstra.github.io/planscanR.screen/reference/reset_embedding_cache.md).
- [R/classify_zeroshot.R](https://barthoekstra.github.io/planscanR.screen/R/classify_zeroshot.R)
  —
  [`classifier()`](https://barthoekstra.github.io/planscanR.screen/reference/classifier.md)
  S3 +
  [`classify_text()`](https://barthoekstra.github.io/planscanR.screen/reference/classify_text.md)
  /
  [`classifier_name()`](https://barthoekstra.github.io/planscanR.screen/reference/classifier_name.md)
  generics;
  [`classify_model_zeroshot()`](https://barthoekstra.github.io/planscanR.screen/reference/classify_model_zeroshot.md)
  (mDeBERTa NLI pipeline, device auto-detect, GPU batching);
  [`reset_classifier_cache()`](https://barthoekstra.github.io/planscanR.screen/reference/reset_classifier_cache.md).
- [R/classify_assessments.R](https://barthoekstra.github.io/planscanR.screen/R/classify_assessments.R)
  —
  [`classify_assessments()`](https://barthoekstra.github.io/planscanR.screen/reference/classify_assessments.md)
  driver. **`labels` is a REQUIRED arg** (no project default); persists
  per-batch when `write_sidecar = TRUE` so an interrupted run is
  resumable.
- [R/score_keywords.R](https://barthoekstra.github.io/planscanR.screen/R/score_keywords.R)
  —
  [`score_keywords()`](https://barthoekstra.github.io/planscanR.screen/reference/score_keywords.md).
  **`lexicon` is a REQUIRED arg.** Matches normalised text (diacritics
  stripped, German digraphs collapsed) as plain substrings.
- [R/select_features.R](https://barthoekstra.github.io/planscanR.screen/R/select_features.R)
  —
  [`selection_feature_names()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_feature_names.md)
  /
  [`selection_features()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_features.md);
  both take **REQUIRED `topics` + `labels`**. The feature frame is fully
  determined by records + spec (missing features → 0), which is what
  keeps train/serve aligned.
- [R/select_learner.R](https://barthoekstra.github.io/planscanR.screen/R/select_learner.R)
  —
  [`selection_learner()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learner.md)
  S3 +
  [`selection_learner_logistic()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learners_builtin.md)
  (default, `glm` engine) +
  [`selection_learners()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learners.md)
  registry + the default recipe.
- [R/train_selection.R](https://barthoekstra.github.io/planscanR.screen/R/train_selection.R)
  — `train_selection_model(records, reviews, topics, labels, ...)`
  (stores `topics`/`labels` on the model so
  [`predict_selection()`](https://barthoekstra.github.io/planscanR.screen/reference/predict_selection.md)
  rebuilds the same frame),
  [`consensus_reviews()`](https://barthoekstra.github.io/planscanR.screen/reference/consensus_reviews.md),
  [`selection_cv_metrics()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_cv_metrics.md),
  [`predict_selection()`](https://barthoekstra.github.io/planscanR.screen/reference/predict_selection.md),
  [`save_selection_model()`](https://barthoekstra.github.io/planscanR.screen/reference/save_selection_model.md)
  /
  [`load_selection_model()`](https://barthoekstra.github.io/planscanR.screen/reference/save_selection_model.md).
  Metrics are **out-of-fold** (k-fold CV).
- [R/select_learning_curve.R](https://barthoekstra.github.io/planscanR.screen/R/select_learning_curve.R)
  —
  [`selection_learning_curve()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learning_curve.md)
  /
  [`learning_curve_summary()`](https://barthoekstra.github.io/planscanR.screen/reference/learning_curve_summary.md)
  (honest held-out protocol with a fixed test set per repeat).
- [R/utils_screen.R](https://barthoekstra.github.io/planscanR.screen/R/utils_screen.R)
  — self-contained copies of three one-line `planscanR` leaf helpers
  (`%||%`, `warn_partial`, `normalise_text_for_match`) so screen never
  reaches into `planscanR` via `:::`.
- [R/utils_language.R](https://barthoekstra.github.io/planscanR.screen/R/utils_language.R)
  — copy of the country→language map for the coverage warning.
- [R/zzz.R](https://barthoekstra.github.io/planscanR.screen/R/zzz.R) —
  declares Python deps in `.onLoad` via
  [`reticulate::py_require()`](https://rstudio.github.io/reticulate/reference/py_require.html)
  (sentence-transformers + transformers / torch / sentencepiece /
  protobuf).

## Contracts you must not break

- **Sidecar I/O is owned by `planscanR`.** Read/write via
  [`planscanR::read_record_sidecar()`](https://barthoekstra.github.io/planscanR/reference/read_record_sidecar.html)
  /
  [`planscanR::write_record_sidecar()`](https://barthoekstra.github.io/planscanR/reference/write_record_sidecar.html);
  resolve the cache root only via
  [`planscanR::cache_dir_default()`](https://barthoekstra.github.io/planscanR/reference/cache_dir_default.html).
  Never reimplement cache-root resolution or the sidecar JSON schema
  (currently v2, asserted on read by planscanR’s reader). A schema
  change needs a version bump and matching reads in all three packages —
  see the sidecar-schema note in the parent
  [../CLAUDE.md](https://barthoekstra.github.io/CLAUDE.md).
- **S3 generics are owned here.** `embedding_model` / `embed_text` /
  `model_name` / `supported_languages`, `classifier` / `classify_text` /
  `classifier_name`, and `selection_learner` are *defined* in this
  package. Add new methods (a new backend, a new learner) **here**, in
  the package that owns the generic — not in the package that introduces
  the subclass.
- **Config-agnosticism.** No project-specific topics / labels / lexicon
  defaults live here. `classify_assessments(labels=)`,
  `score_keywords(lexicon=)`, and `selection_features(topics=, labels=)`
  are all REQUIRED args; callers pass them. The BIOGAIN config lives in
  `planscanR.biogain` (`biogain_classification_labels()`,
  `biogain_keyword_lexicon()`, `biogain_assessment_topics()`). Do not
  add a project default here.
- **Python deps are additive.** `zzz.R` declares this package’s deps via
  `py_require()`, which is additive across the session — a sibling later
  declaring its own deps must not clobber screen’s
  sentence-transformers. Keep declarations in `.onLoad`, never overwrite
  the Python environment.

## Conventions

- **Required-arg discipline.** When a function needs project config
  (topics / labels / lexicon), make it a required argument with a
  classed `cli_abort` pointing at the `planscanR.biogain` accessor —
  match the existing messages.
- **Offline tests, always.** The 135-passing suite never touches live
  reticulate / Python. Use the deterministic mock embedding model
  (`make_fake_model()` in `tests/testthat/helper-screen.R`) and the mock
  zero-shot classifier (in `test-classify.R`). New backends get an
  offline mock, not a live model call.
- **tidymodels is Suggests.** parsnip / recipes / rsample / workflows
  are optional; the selection paths gate on them via
  `require_tidymodels()` with a classed `planscanR_error_missing_dep`.
  Don’t promote them to Imports.
- **Per-package commits.** Commit and branch within this repo only; a
  change spanning siblings is one commit per package in dependency
  order.

## Pointers

- Generics-ownership table and the sidecar-schema contract: the parent
  [../CLAUDE.md](https://barthoekstra.github.io/CLAUDE.md).
- Cosine scoring entry point:
  [`score_records()`](https://barthoekstra.github.io/planscanR.screen/reference/score_records.md)
  / `score_assessments(write_sidecar = TRUE)`.
- Selection model round-trip:
  [`train_selection_model()`](https://barthoekstra.github.io/planscanR.screen/reference/train_selection_model.md)
  →
  [`save_selection_model()`](https://barthoekstra.github.io/planscanR.screen/reference/save_selection_model.md)
  (conventionally `selection_model.rds` in the cache root) →
  [`load_selection_model()`](https://barthoekstra.github.io/planscanR.screen/reference/save_selection_model.md)
  →
  [`predict_selection()`](https://barthoekstra.github.io/planscanR.screen/reference/predict_selection.md).
- BIOGAIN-specific wiring (ensemble rule, runbook, review app): the
  `planscanR.biogain` repo, not here.
