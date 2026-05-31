#' planscanR.screen: score, classify, and select text records by topic relevance
#'
#' The general-purpose screening framework of the planscanR family:
#'
#' * **Relevance scoring** — [score_assessments()] / [score_records()] over a
#'   pluggable [embedding_model()] (built-in MiniLM backend
#'   [embedding_model_minilm()]).
#' * **Zero-shot classification** — [classify_assessments()] over a pluggable
#'   [classifier()] (built-in [classify_model_zeroshot()]).
#' * **Lexical keywords** — [score_keywords()] over a caller-supplied lexicon.
#' * **Learned selection** — [train_selection_model()] / [predict_selection()]
#'   over the per-record scores, with a pluggable [selection_learner()].
#'
#' Reads and writes the planscanR sidecar cache through `planscanR::` (the cache
#' owner). The BIOGAIN-specific topics, labels, lexicon, and ensemble rule live
#' in \pkg{planscanR.biogain}.
#'
#' @keywords internal
#' @importFrom rlang .data
"_PACKAGE"
