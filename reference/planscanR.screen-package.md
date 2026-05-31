# planscanR.screen: score, classify, and select text records by topic relevance

The general-purpose screening framework of the planscanR family:

## Details

- **Relevance scoring** —
  [`score_assessments()`](https://barthoekstra.github.io/planscanR.screen/reference/score_assessments.md)
  /
  [`score_records()`](https://barthoekstra.github.io/planscanR.screen/reference/score_records.md)
  over a pluggable
  [`embedding_model()`](https://barthoekstra.github.io/planscanR.screen/reference/embedding_model.md)
  (built-in MiniLM backend
  [`embedding_model_minilm()`](https://barthoekstra.github.io/planscanR.screen/reference/embedding_model_minilm.md)).

- **Zero-shot classification** —
  [`classify_assessments()`](https://barthoekstra.github.io/planscanR.screen/reference/classify_assessments.md)
  over a pluggable
  [`classifier()`](https://barthoekstra.github.io/planscanR.screen/reference/classifier.md)
  (built-in
  [`classify_model_zeroshot()`](https://barthoekstra.github.io/planscanR.screen/reference/classify_model_zeroshot.md)).

- **Lexical keywords** —
  [`score_keywords()`](https://barthoekstra.github.io/planscanR.screen/reference/score_keywords.md)
  over a caller-supplied lexicon.

- **Learned selection** —
  [`train_selection_model()`](https://barthoekstra.github.io/planscanR.screen/reference/train_selection_model.md)
  /
  [`predict_selection()`](https://barthoekstra.github.io/planscanR.screen/reference/predict_selection.md)
  over the per-record scores, with a pluggable
  [`selection_learner()`](https://barthoekstra.github.io/planscanR.screen/reference/selection_learner.md).

Reads and writes the planscanR sidecar cache through `planscanR::` (the
cache owner). The BIOGAIN-specific topics, labels, lexicon, and ensemble
rule live in planscanR.biogain.

## See also

Useful links:

- <https://github.com/barthoekstra/planscanR.screen>

- <https://barthoekstra.github.io/planscanR.screen/>

- Report bugs at
  <https://github.com/barthoekstra/planscanR.screen/issues>

## Author

**Maintainer**: Bart Hoekstra <mail@barthoekstra.nl>
