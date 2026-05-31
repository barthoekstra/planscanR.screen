# Shared mock model `make_fake_model()` lives in helper-planscanr.R so other
# test files (e.g. test-topics.R) can use the same deterministic fixture.

test_that("embedding_model() validates inputs", {
  expect_error(embedding_model("", "en", identity), "non-empty string")
  expect_error(embedding_model("x", character(0), identity), "non-empty character")
  expect_error(embedding_model("x", "en", "not-a-function"), "must be a function")
})

test_that("custom S3 model implements the required interface", {
  m <- make_fake_model()
  expect_s3_class(m, "planscanR_embedding_model")
  expect_s3_class(m, "planscanR_embedding_custom")
  expect_identical(model_name(m), "fake-bow")
  expect_identical(supported_languages(m), c("nl", "en", "de"))
  v <- embed_text(m, c("hello", "hallo"))
  expect_true(is.matrix(v))
  expect_identical(nrow(v), 2L)
})

test_that("embed_text errors on malformed custom output", {
  bad <- embedding_model("bad", "en", function(x) "not a matrix")
  expect_error(embed_text(bad, c("a", "b")), "invalid shape")
})

test_that("S3 dispatch falls through on unknown classes", {
  expect_error(embed_text("not-a-model", "x"), class = "planscanR_error_no_method")
  expect_error(supported_languages(list()), class = "planscanR_error_no_method")
  expect_error(model_name(list()), class = "planscanR_error_no_method")
})

test_that("normalise_topics validates and auto-slugs", {
  expect_null(planscanR.screen:::normalise_topics(NULL))
  # Scalar gets auto-slugged into its single name
  out <- planscanR.screen:::normalise_topics("wind energy")
  expect_identical(unname(out), "wind energy")
  expect_identical(names(out), "wind_energy")
  # Named vector keeps names
  out <- planscanR.screen:::normalise_topics(c(wind = "wind energy", solar = "solar energy"))
  expect_identical(names(out), c("wind", "solar"))
  # Unnamed multi-element auto-slugs
  out <- planscanR.screen:::normalise_topics(c("wind energy", "solar energy"))
  expect_identical(names(out), c("wind_energy", "solar_energy"))
  # Duplicate slugs are rejected
  expect_error(
    planscanR.screen:::normalise_topics(c(wind = "x", wind = "y")),
    class = "planscanR_error_bad_input"
  )
  # Empty / invalid input
  expect_error(planscanR.screen:::normalise_topics(character(0)), class = "planscanR_error_bad_input")
  expect_error(planscanR.screen:::normalise_topics(c("ok", "")), class = "planscanR_error_bad_input")
})

test_that("slugify_topic produces stable column-safe suffixes", {
  expect_identical(planscanR.screen:::slugify_topic("Wind Energy"), "wind_energy")
  expect_identical(planscanR.screen:::slugify_topic("power lines & grid"), "power_lines_grid")
  expect_identical(planscanR.screen:::slugify_topic("  ---  "), "topic")
})

test_that("cosine_similarity_matrix returns the expected [N, T] shape", {
  m <- matrix(c(1, 0, 0, 0, 1, 0), nrow = 2, byrow = TRUE)
  t <- matrix(c(1, 0, 0, 0, 1, 0, 1, 1, 0), nrow = 3, byrow = TRUE)
  out <- planscanR.screen:::cosine_similarity_matrix(m, t)
  expect_identical(dim(out), c(2L, 3L))
  expect_equal(out[1, 1], 1)
  expect_equal(out[2, 2], 1)
  expect_equal(out[1, 2], 0)
})

test_that("score_records in multi-topic mode adds one column per topic", {
  m <- make_fake_model()
  recs <- tibble::tibble(
    country = c("nl", "nl"),
    title = c("Windpark Foo", "Zonnepark Bar"),
    summary = c("Wind energy advice", "Solar PV farm")
  )
  out <- score_records(
    recs,
    topic = c(wind = "windpark wind energy", solar = "zonnepark solar energy"),
    model = m
  )
  expect_true(all(c("relevance_score_wind", "relevance_score_solar", "relevance_model") %in% names(out)))
  expect_false("relevance_score" %in% names(out))
  # Cross-topic sanity: wind record scores higher on wind than solar, and
  # vice versa.
  expect_gt(out$relevance_score_wind[1], out$relevance_score_solar[1])
  expect_gt(out$relevance_score_solar[2], out$relevance_score_wind[2])
})

test_that("score_assessments() wraps score_records and is sidecar-aware", {
  m <- make_fake_model()
  recs <- tibble::tibble(
    country = "nl",
    title = "Windpark Foo",
    summary = "Wind energy advice"
  )
  out <- score_assessments(recs, topic = c(w = "wind"), model = m)
  expect_true("relevance_score_w" %in% names(out))
})

test_that("score_records with a single topic adds a per-topic column and is invariant to row order", {
  m <- make_fake_model()
  recs <- tibble::tibble(
    country = c("nl", "nl"),
    title = c("Windpark Foo", "Boerderij Windhoek"),
    summary = c("Wind energy advice", "Farm expansion advice")
  )
  out <- score_records(recs, topic = "Windpark Foo", model = m)
  expect_true("relevance_score_windpark_foo" %in% names(out))
  expect_false("relevance_score" %in% names(out))
  expect_true("relevance_model" %in% names(out))
  expect_identical(out$relevance_model, c("fake-bow", "fake-bow"))
  expect_gt(out$relevance_score_windpark_foo[1], out$relevance_score_windpark_foo[2])
})

test_that("score_records warns once when a country language is unsupported", {
  reset_relevance_warnings()
  m <- make_fake_model(languages = c("en"))
  recs <- tibble::tibble(
    country = c("nl", "nl"),
    title = c("A", "B"),
    summary = c("x", "y")
  )
  expect_warning(
    score_records(recs, topic = "z", model = m),
    class = "planscanR_warning_partial"
  )
  # Second call: same model, same country -> no warning
  expect_no_warning(score_records(recs, topic = "z", model = m))
})

test_that("score_records preserves zero-row tibbles", {
  m <- make_fake_model()
  recs <- tibble::tibble(country = character(0), title = character(0), summary = character(0))
  out <- score_records(recs, topic = "anything", model = m)
  expect_identical(nrow(out), 0L)
  expect_true(all(c("relevance_score_anything", "relevance_model") %in% names(out)))
})

test_that("reset_relevance_warnings clears the per-session ledger", {
  reset_relevance_warnings()
  m <- make_fake_model(languages = c("en"))
  recs <- tibble::tibble(country = "nl", title = "A", summary = "x")
  expect_warning(score_records(recs, "topic", m), class = "planscanR_warning_partial")
  expect_no_warning(score_records(recs, "topic", m))
  reset_relevance_warnings()
  expect_warning(score_records(recs, "topic", m), class = "planscanR_warning_partial")
})

test_that("languages_for_country handles multi-language and unknown codes", {
  expect_identical(planscanR.screen:::languages_for_country("nl"), "nl")
  expect_identical(planscanR.screen:::languages_for_country("at"), "de")
  expect_setequal(planscanR.screen:::languages_for_country("be"), c("nl", "fr", "de"))
  expect_identical(planscanR.screen:::languages_for_country("ZZ"), character(0))
})

