# planscanR.screen: score, classify, and select text records by topic relevance

The general-purpose screening framework of the planscanR family:

## Details

- **Relevance scoring** — `planscanR::score_assessments()` /
  `planscanR::score_records()` over a pluggable
  `planscanR::embedding_model()` (built-in MiniLM backend
  `planscanR::embedding_model_minilm()`).

- **Zero-shot classification** — `planscanR::classify_assessments()`
  over a pluggable `planscanR::classifier()` (built-in
  `planscanR::classify_model_zeroshot()`).

- **Lexical keywords** — `planscanR::score_keywords()` over a
  caller-supplied lexicon.

- **Learned selection** — `planscanR::train_selection_model()` /
  `planscanR::predict_selection()` over the per-record scores, with a
  pluggable `planscanR::selection_learner()`.

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
