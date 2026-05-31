# Learning-curve tests for the learned selection model. Gated on the tidymodels
# glue (Suggests). Reuses a minimal synthetic generator (cleanly separable so any
# learner trains fast and stably).

# Local topic/label fixtures (the framework is config-agnostic; the BIOGAIN
# sets live in planscanR.biogain). Cardinality mirrors the BIOGAIN sets.
lc_topics <- c(
  wind = "wind", solar = "solar", power_grid = "grid",
  other_renewable = "other_renewable", energy_strategy = "strategy",
  renewable_zoning = "zoning"
)
lc_labels <- c(
  wind = "wind", solar = "solar", power_grid = "grid",
  other_renewable = "other_renewable", energy_strategy = "strategy",
  renewable_zoning = "zoning", fossil_power = "fossil",
  oil_gas_extraction = "oilgas", nuclear = "nuclear", water = "water",
  land_use = "land", transport = "transport", other = "other"
)

lc_synth_records <- function(n = 160, seed = 1) {
  set.seed(seed)
  topics <- names(lc_topics)
  labels <- names(lc_labels)
  keep <- rep(c(TRUE, FALSE), length.out = n)
  df <- tibble::tibble(
    document_id = as.character(seq_len(n)),
    country = rep(c("nl", "de", "at"), length.out = n)
  )
  for (s in topics) {
    base <- ifelse(keep, 0.65, 0.2)
    df[[paste0("relevance_score_", s)]] <- pmin(1, pmax(0, base + stats::rnorm(n, 0, 0.08)))
  }
  for (l in labels) {
    base <- if (l == "wind") ifelse(keep, 0.6, 0.05) else stats::runif(n, 0, 0.1)
    df[[paste0("class_score_", l)]] <- pmin(1, pmax(0, base))
  }
  df$kw_total <- ifelse(keep, 3L, 0L)
  attr(df, "keep") <- keep
  df
}

lc_synth_reviews <- function(records) {
  keep <- attr(records, "keep")
  tibble::tibble(
    document_id = records$document_id,
    country = records$country,
    decision = ifelse(keep, "keep", "drop"),
    source = "random",
    reviewer = "tester",
    note = NA_character_,
    reviewed_at = "2026-05-01T12:00:00",
    sidecar_path = NA_character_
  )
}

test_that("selection_learning_curve returns the expected long shape over sizes", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("rsample")
  skip_if_not_installed("workflows")

  recs <- lc_synth_records()
  rev <- lc_synth_reviews(recs)

  curve <- selection_learning_curve(
    recs,
    rev,
    lc_topics,
    lc_labels,
    sizes = c(30, 60, 90),
    repeats = 3,
    seed = 42
  )

  expect_named(
    curve,
    c("size", "n_train_used", "rep", "n_test", "precision", "recall", "f1")
  )
  expect_gt(dplyr::n_distinct(curve$size), 1L)
  expect_true(all(curve$precision >= 0 & curve$precision <= 1, na.rm = TRUE))
  expect_true(all(curve$recall >= 0 & curve$recall <= 1, na.rm = TRUE))
  expect_true(all(curve$f1 >= 0 & curve$f1 <= 1, na.rm = TRUE))
  expect_true(all(curve$rep %in% 1:3))

  summ <- learning_curve_summary(curve)
  expect_equal(nrow(summ), dplyr::n_distinct(curve$size))
  expect_named(
    summ,
    c(
      "size",
      "n_train_used",
      "n",
      "f1_mean",
      "f1_sd",
      "precision_mean",
      "precision_sd",
      "recall_mean",
      "recall_sd"
    )
  )
  expect_true(all(summ$f1_mean >= 0 & summ$f1_mean <= 1, na.rm = TRUE))
})

test_that("selection_learning_curve(by_country=TRUE) returns per-country + 'all' rows", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("rsample")
  skip_if_not_installed("workflows")

  recs <- lc_synth_records()
  rev <- lc_synth_reviews(recs)

  curve <- selection_learning_curve(
    recs,
    rev,
    lc_topics,
    lc_labels,
    sizes = c(30, 60, 90),
    repeats = 2,
    seed = 42,
    by_country = TRUE
  )

  expect_true("country" %in% names(curve))
  expect_equal(names(curve)[1], "country")
  expect_true("all" %in% curve$country)
  expect_true(all(c("nl", "de", "at") %in% curve$country))
  expect_true(all(curve$f1 >= 0 & curve$f1 <= 1, na.rm = TRUE))

  summ <- learning_curve_summary(curve)
  expect_true("country" %in% names(summ))
  # One row per (country, size) that actually had data.
  expect_equal(
    nrow(summ),
    nrow(unique(curve[, c("country", "size")]))
  )
})

test_that("selection_learning_curve errors without both classes", {
  skip_if_not_installed("parsnip")
  recs <- lc_synth_records(40)
  rev <- lc_synth_reviews(recs)
  rev$decision <- "keep"
  expect_error(
    selection_learning_curve(recs, rev, lc_topics, lc_labels, repeats = 2),
    class = "planscanR_error_bad_input"
  )
})
