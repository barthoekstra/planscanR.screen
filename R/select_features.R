# Feature contract for the learned selection model.
#
# A single function turns scored + classified records (from get_assessments(),
# index_cache(), or the review app's snapshot) into the model matrix used to
# LEARN the BIOGAIN selection decision from human keep/drop labels. Both
# training and prediction go through `selection_features()` so there is no
# train/serve skew: whatever columns the learner saw at fit time are rebuilt the
# same way at predict time.
#
# The default feature set is deliberately country-agnostic — the per-topic
# cosine scores and per-label classifier scores are language-independent
# numbers, so a model fit on NL/DE/AT should transfer to a new portal. Country
# and native_type are *available* via the `include` argument but OFF by default
# (they boost in-sample fit but won't generalise to an unseen country / a portal
# whose taxonomy we've never seen).

#' Names of the features the selection model is trained on.
#'
#' The default set is the three numeric relevance signals already persisted on
#' every sidecar: one cosine score per BIOGAIN topic
#' (`relevance_score_<slug>`), one zero-shot classifier score per candidate
#' label (`class_score_<slug>`), and the keyword total (`kw_total`).
#'
#' @param topics Named topic vector naming the cosine columns (required). The
#'   BIOGAIN set is `biogain_assessment_topics()` in `planscanR.biogain`.
#' @param labels Named classifier-label vector naming the classifier columns
#'   (required). The BIOGAIN set is `biogain_classification_labels()` in
#'   `planscanR.biogain`.
#' @param include Optional extra feature columns to append (off by default).
#'   Recognised: `"country"`, `"native_type"`. These are country-specific and
#'   will not transfer to an unseen portal — opt in only when training and
#'   predicting on the same set of countries.
#' @return A character vector of feature column names, in a stable order.
#' @export
#' @examples
#' \dontrun{
#' selection_feature_names(topics, labels)
#' selection_feature_names(topics, labels, include = "country")
#' }
selection_feature_names <- function(
  topics,
  labels,
  include = character(0)
) {
  if (missing(topics) || missing(labels)) {
    cli::cli_abort(c(
      "{.arg topics} and {.arg labels} are required.",
      i = "Pass the topic and classifier-label vectors, e.g. {.code planscanR.biogain::biogain_assessment_topics()} and {.code planscanR.biogain::biogain_classification_labels()}."
    ))
  }
  cosine <- paste0("relevance_score_", names(topics))
  clf <- paste0("class_score_", names(labels))
  feats <- c(cosine, clf, "kw_total")
  extra <- intersect(c("country", "native_type"), include)
  c(feats, extra)
}

#' Build the selection-model feature frame from records.
#'
#' Produces a tibble carrying the record keys (`document_id`, `country`) plus
#' one column per feature in [selection_feature_names()]. Missing or
#' non-finite numeric features are filled with `0` (an unscored / unclassified
#' record reads as "no signal"), so the frame is fully determined by the
#' records and the feature spec — the key property that keeps training and
#' prediction aligned.
#'
#' The returned tibble carries a `"feature_names"` attribute listing the
#' predictor columns (everything except the two key columns), which the trainer
#' uses to assign recipe roles.
#'
#' @param records A tibble with `document_id`, `country`, and the
#'   `relevance_score_*` / `class_score_*` / `kw_total` columns (whatever is
#'   present; absent columns are treated as `0`).
#' @inheritParams selection_feature_names
#' @return A tibble of `document_id`, `country`, and the feature columns, with a
#'   `"feature_names"` attribute.
#' @export
#' @examples
#' \dontrun{
#' recs <- index_cache(country = "nl")
#' X <- selection_features(recs)
#' attr(X, "feature_names")
#' }
selection_features <- function(
  records,
  topics,
  labels,
  include = character(0)
) {
  if (!is.data.frame(records)) {
    cli::cli_abort(
      "{.arg records} must be a data frame.",
      class = "planscanR_error_bad_input"
    )
  }
  if (missing(topics) || missing(labels)) {
    cli::cli_abort(c(
      "{.arg topics} and {.arg labels} are required.",
      i = "Pass the topic and classifier-label vectors, e.g. {.code planscanR.biogain::biogain_assessment_topics()} and {.code planscanR.biogain::biogain_classification_labels()}."
    ))
  }
  n <- nrow(records)
  feature_names <- selection_feature_names(topics, labels, include)
  numeric_feats <- c(
    paste0("relevance_score_", names(topics)),
    paste0("class_score_", names(labels)),
    "kw_total"
  )

  out <- tibble::tibble(
    document_id = if ("document_id" %in% names(records)) {
      as.character(records$document_id)
    } else {
      rep(NA_character_, n)
    },
    country = if ("country" %in% names(records)) {
      as.character(records$country)
    } else {
      rep(NA_character_, n)
    }
  )

  for (f in numeric_feats) {
    v <- if (f %in% names(records)) {
      suppressWarnings(as.numeric(records[[f]]))
    } else {
      rep(NA_real_, n)
    }
    v[!is.finite(v)] <- 0
    out[[f]] <- v
  }

  # Optional categorical extras, kept raw (the recipe dummy-encodes them). NA
  # levels are left as NA for the recipe's step_unknown() to absorb. `country`
  # needs nothing here — it is already present as a character key column.
  if ("native_type" %in% include) {
    out$native_type <- if ("native_type" %in% names(records)) {
      as.character(records$native_type)
    } else {
      rep(NA_character_, n)
    }
  }

  attr(out, "feature_names") <- feature_names
  out
}
