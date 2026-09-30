# Configuration-aware evaluation helpers for curation selection.

curation_metrics_abort <- function(message) {
  curation_abort(message, class = "planscanR_error_curation_model")
}

curation_metrics_normalize_reviews <- function(reviews, config = NULL) {
  if (is.null(reviews)) {
    return(NULL)
  }
  tryCatch(
    curation_model_normalize_reviews(reviews, config),
    planscanR_error_curation_model = function(e) {
      curation_metrics_abort(conditionMessage(e))
    }
  )
}

curation_metrics_selected <- function(data, selection_col) {
  if (!selection_col %in% names(data)) {
    curation_metrics_abort(
      paste0("{.arg records} must include {.field ", selection_col, "} for evaluation.")
    )
  }
  selected <- data[[selection_col]] %in% TRUE
  tibble::tibble(
    .pred_keep = as.numeric(selected),
    truth = factor(as.character(data$decision), levels = c("keep", "drop"))
  )
}

curation_metrics_one <- function(data, selection_col) {
  if (nrow(data) == 0L) {
    return(NULL)
  }
  selection_metrics_from_oof(
    curation_metrics_selected(data, selection_col),
    threshold = 0.5
  )
}

#' Compare configured selection output with unanimous human decisions.
#'
#' Evaluation filters the requested review source before applying the existing
#' unanimous [consensus_reviews()] rule. The default `eval_source = "random"`
#' preserves the legacy unbiased evaluation path.
#'
#' @param records Records containing `country`, `document_id`, and the selected
#'   logical column.
#' @param reviews Review rows with canonical or legacy reviewer identity.
#' @param config Optional configuration used to scope explicit project/corpus
#'   columns.
#' @param eval_source Review source(s) to evaluate, defaulting to `"random"`;
#'   `NULL` evaluates all sources.
#' @param by_country If `TRUE`, prepend `country` with an `all` row.
#' @param selection_col Name of the logical selection column, default
#'   `"selected"`.
#' @return One metrics row, country metrics, or `NULL` when no agreed labels
#'   match the records.
#' @export
curation_selection_vs_human <- function(
  records,
  reviews,
  config = NULL,
  eval_source = "random",
  by_country = FALSE,
  selection_col = "selected"
) {
  if (!is.data.frame(records)) {
    curation_metrics_abort("{.arg records} must be a data frame.")
  }
  if (!all(c("country", "document_id") %in% names(records))) {
    curation_metrics_abort("{.arg records} must include country and document_id columns.")
  }
  if (!is.null(config) && !inherits(config, "planscanR_curation_config")) {
    curation_metrics_abort("{.arg config} must be a planscanR_curation_config.")
  }
  rows <- curation_metrics_normalize_reviews(reviews, config)
  if (is.null(rows) || nrow(rows) == 0L) {
    return(NULL)
  }
  if (!is.null(eval_source)) {
    if (!is.character(eval_source) || length(eval_source) == 0L || anyNA(eval_source)) {
      curation_metrics_abort("{.arg eval_source} must be NULL or a non-empty character vector.")
    }
    rows <- rows[rows$source %in% eval_source, , drop = FALSE]
  }
  if (nrow(rows) == 0L) {
    return(NULL)
  }
  decided <- consensus_reviews(rows)
  if (nrow(decided) == 0L) {
    return(NULL)
  }
  if (!selection_col %in% names(records)) {
    curation_metrics_abort(
      paste0("{.arg records} must include {.field ", selection_col, "} for evaluation.")
    )
  }
  selected <- records[, c("document_id", "country", selection_col), drop = FALSE]
  names(selected)[names(selected) == selection_col] <- ".selected"
  d <- merge(
    selected,
    decided[, c("document_id", "country", "decision"), drop = FALSE],
    by = c("document_id", "country")
  )
  if (nrow(d) == 0L) {
    return(NULL)
  }
  d$selected <- d$.selected
  one <- function(part) curation_metrics_one(part, "selected")
  if (!isTRUE(by_country)) {
    return(one(d))
  }
  groups <- split(d, d$country)
  rows_out <- lapply(groups, function(part) {
    value <- one(part)
    value$country <- part$country[[1L]]
    value
  })
  all_value <- one(d)
  all_value$country <- "all"
  out <- dplyr::bind_rows(c(list(all_value), rows_out))
  out[, c("country", setdiff(names(out), "country")), drop = FALSE]
}

#' Summarise unanimous agreement across reviewers.
#'
#' Agreement uses the legacy rule over keep/drop/unsure rows: a record counts
#' only when at least two distinct reviewers have decisions, and agrees when
#' all those decisions are identical. Explicit project/corpus columns are
#' filtered by `config` when supplied.
#'
#' @param reviews Review rows with canonical or legacy reviewer identity.
#' @param config Optional configuration used to scope explicit project/corpus
#'   columns.
#' @param decisions Decisions included in the agreement summary.
#' @return A one-row tibble with `n_multi`, `n_agree`, and `agreement_rate`, or
#'   `NULL` when no multi-reviewed record exists.
#' @export
curation_inter_reviewer_summary <- function(
  reviews,
  config = NULL,
  decisions = c("keep", "drop", "unsure")
) {
  rows <- curation_metrics_normalize_reviews(reviews, config)
  if (is.null(rows) || nrow(rows) == 0L) {
    return(NULL)
  }
  if (!is.character(decisions) || length(decisions) == 0L || anyNA(decisions)) {
    curation_metrics_abort("{.arg decisions} must be a non-empty character vector.")
  }
  decided <- rows[rows$decision %in% decisions, , drop = FALSE]
  if (nrow(decided) == 0L) {
    return(NULL)
  }
  per_record <- dplyr::summarise(
    dplyr::group_by(decided, .data$country, .data$document_id),
    n_rev = dplyr::n_distinct(.data$reviewer_id),
    n_dec = dplyr::n_distinct(.data$decision),
    .groups = "drop"
  )
  multi <- per_record[per_record$n_rev >= 2L, , drop = FALSE]
  if (nrow(multi) == 0L) {
    return(NULL)
  }
  n_agree <- sum(multi$n_dec == 1L)
  tibble::tibble(
    n_multi = nrow(multi),
    n_agree = n_agree,
    agreement_rate = n_agree / nrow(multi)
  )
}
