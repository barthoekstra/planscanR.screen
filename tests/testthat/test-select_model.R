# Synthetic-data tests for the learned selection model. No HTTP, no portal.
# The full train/predict path is gated on the tidymodels glue being installed
# (Suggests); featurization is pure base R and always runs.

# Local topic/label fixtures so these tests exercise the framework without any
# project-specific config (the framework is config-agnostic). A representative
# cardinality (6 topics, 13 labels) including a "wind" slug, which synth_records
# keys on.
test_topics <- c(
  wind = "wind", solar = "solar", power_grid = "grid",
  other_renewable = "other_renewable", energy_strategy = "strategy",
  renewable_zoning = "zoning"
)
test_labels <- c(
  wind = "wind", solar = "solar", power_grid = "grid",
  other_renewable = "other_renewable", energy_strategy = "strategy",
  renewable_zoning = "zoning", fossil_power = "fossil",
  oil_gas_extraction = "oilgas", nuclear = "nuclear", water = "water",
  land_use = "land", transport = "transport", other = "other"
)

# A synthetic scored+classified corpus where "keep" records carry a high wind
# cosine + classifier score, so any reasonable learner should separate them.
synth_records <- function(n = 120, seed = 1) {
  set.seed(seed)
  topics <- names(test_topics)
  labels <- names(test_labels)
  keep <- rep(c(TRUE, FALSE), length.out = n)
  df <- tibble::tibble(
    document_id = as.character(seq_len(n)),
    country = rep(c("nl", "de", "at"), length.out = n)
  )
  for (s in topics) {
    base <- ifelse(keep, 0.65, 0.2)
    df[[paste0("relevance_score_", s)]] <- pmin(
      1,
      pmax(0, base + stats::rnorm(n, 0, 0.08))
    )
  }
  for (l in labels) {
    base <- if (l == "wind") ifelse(keep, 0.6, 0.05) else stats::runif(n, 0, 0.1)
    df[[paste0("class_score_", l)]] <- pmin(1, pmax(0, base))
  }
  df$kw_total <- ifelse(keep, 3L, 0L)
  attr(df, "keep") <- keep
  df
}

synth_reviews <- function(records) {
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

test_that("selection_feature_names is stable and country-agnostic by default", {
  fn <- selection_feature_names(test_topics, test_labels)
  expect_true("kw_total" %in% fn)
  expect_true(any(grepl("^relevance_score_", fn)))
  expect_true(any(grepl("^class_score_", fn)))
  expect_false("country" %in% fn)
  expect_false("native_type" %in% fn)
  expect_true(
    "country" %in% selection_feature_names(test_topics, test_labels, include = "country")
  )
})

test_that("selection_features fills missing/NA numerics with 0 and carries keys", {
  recs <- tibble::tibble(
    document_id = c("a", "b"),
    country = c("nl", "de"),
    relevance_score_wind = c(0.4, NA_real_)
    # all other feature columns intentionally absent
  )
  X <- selection_features(recs, test_topics, test_labels)
  expect_equal(X$document_id, c("a", "b"))
  expect_equal(X$relevance_score_wind, c(0.4, 0))
  expect_true(all(X$kw_total == 0))
  expect_setequal(
    attr(X, "feature_names"),
    selection_feature_names(test_topics, test_labels)
  )
})

test_that("train_selection_model learns, predicts, and round-trips on disk", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("rsample")
  skip_if_not_installed("workflows")

  recs <- synth_records()
  rev <- synth_reviews(recs)

  m <- train_selection_model(recs, rev, test_topics, test_labels, v = 5L, seed = 42)
  expect_s3_class(m, "planscanR_selection_model")
  expect_equal(m$n_train, nrow(recs))
  # Cleanly separable synthetic data -> strong out-of-fold metrics.
  expect_gt(m$cv$f1, 0.9)

  pred <- predict_selection(m, recs)
  expect_true(all(c("select_prob", "selected_model") %in% names(pred)))
  expect_true(is.numeric(pred$select_prob))
  # Threshold sweep without retraining.
  lo <- selection_cv_metrics(m, threshold = 0.1)
  hi <- selection_cv_metrics(m, threshold = 0.9)
  expect_gte(lo$tp + lo$fp, hi$tp + hi$fp) # lower threshold keeps at least as many

  path <- withr::local_tempfile(fileext = ".rds")
  save_selection_model(m, path)
  m2 <- load_selection_model(path)
  expect_s3_class(m2, "planscanR_selection_model")
  expect_equal(m2$cv$f1, m$cv$f1)
})

test_that("selection_cv_metrics by_country returns per-country plus an all row", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("recipes")
  skip_if_not_installed("rsample")
  skip_if_not_installed("workflows")

  recs <- synth_records()
  rev <- synth_reviews(recs)
  m <- train_selection_model(recs, rev, test_topics, test_labels, v = 5L, seed = 42)

  bc <- selection_cv_metrics(m, by_country = TRUE)
  expect_true("country" %in% names(bc))
  expect_true("all" %in% bc$country)
  expect_setequal(setdiff(bc$country, "all"), c("nl", "de", "at"))
  # the all-row count equals the sum over countries (each OOF record once)
  expect_equal(
    bc$n_reviewed[bc$country == "all"],
    sum(bc$n_reviewed[bc$country != "all"])
  )
  expect_true(all(bc$f1 >= 0 & bc$f1 <= 1, na.rm = TRUE))
})

test_that("consensus_reviews keeps agreements, drops conflicts, latest-per-reviewer", {
  rev <- tibble::tibble(
    document_id = c("1", "1", "2", "2", "3", "3", "4", "4"),
    country = c("nl", "nl", "nl", "nl", "de", "de", "de", "de"),
    decision = c(
      "keep",
      "keep", # rec 1: two reviewers agree -> keep
      "keep",
      "drop", # rec 2: two reviewers disagree -> dropped
      "drop",
      "keep", # rec 3: same reviewer changed mind -> latest (keep)
      "keep",
      "unsure" # rec 4: one keep + one unsure -> keep (unsure ignored)
    ),
    reviewer = c("A", "B", "A", "B", "A", "A", "A", "B"),
    reviewed_at = c(
      "2026-01-01T10:00:00",
      "2026-01-01T11:00:00",
      "2026-01-01T10:00:00",
      "2026-01-01T11:00:00",
      "2026-01-01T10:00:00",
      "2026-01-02T10:00:00", # rec 3 latest is keep
      "2026-01-01T10:00:00",
      "2026-01-01T11:00:00"
    )
  )
  out <- consensus_reviews(rev)
  expect_setequal(out$document_id, c("1", "3", "4"))
  expect_false("2" %in% out$document_id) # conflict excluded
  expect_equal(out$decision[out$document_id == "1"], "keep")
  expect_equal(out$decision[out$document_id == "3"], "keep") # latest verdict
  expect_equal(out$decision[out$document_id == "4"], "keep")
  expect_equal(out$n_reviewers[out$document_id == "1"], 2L)
})

test_that("train_selection_model errors without both classes", {
  skip_if_not_installed("parsnip")
  recs <- synth_records(20)
  rev <- synth_reviews(recs)
  rev$decision <- "keep" # single class
  expect_error(
    train_selection_model(recs, rev, test_topics, test_labels),
    class = "planscanR_error_bad_input"
  )
})
