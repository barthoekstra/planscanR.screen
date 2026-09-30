# ACQ-C1c: generic record enrichment, sampling, queues, and progress.
#
# All inputs are synthetic.  The tests intentionally exercise the same scalar
# fields and sample ordering as the legacy review app without importing BIOGAIN.

curation_records_test_config <- function(
  config_id = "fixture.v1",
  topics = c(wind = "wind topic", solar = "solar topic"),
  lexicon = list(wind = c("wind", "turbine"), solar = c("solar", "pv"))
) {
  labels <- c(keep = "keep", drop = "drop")
  attr(labels, "relevant") <- "keep"
  curation_config(
    project_id = "project-a",
    corpus_id = "corpus-a",
    config_id = config_id,
    topics = topics,
    labels = labels,
    lexicon = lexicon,
    select_fn = function(records, controls) {
      records$selected <- rep(FALSE, nrow(records))
      records
    },
    selection_controls = list(),
    funnel_stages = list(
      indexed = list(
        key = "indexed",
        label = "Indexed",
        fn = function(records, context) rep(TRUE, nrow(records))
      )
    ),
    branding = list(title = "Fixture", primary = "#123456"),
    artifact_paths = list(root = "/tmp/fixture")
  )
}

curation_records_test_frame <- function() {
  tibble::tibble(
    country = c("nl", "de", "nl", "at"),
    document_id = c("shared-001", "shared-001", "nl-002", "at-001"),
    title = c("Wind turbine", "Solar park", "Wind and solar", "Other plan"),
    summary = c("wind", "solar PV", "turbine solar", "unrelated"),
    relevance_score_wind = c(0.8, 0.1, 0.2, 0.0),
    relevance_score_solar = c(0.1, 0.7, 0.6, 0.0),
    class_label = c("keep", "keep", "drop", "drop"),
    class_relevant = c(TRUE, TRUE, FALSE, FALSE),
    class_score = c(0.9, 0.8, 0.7, 0.1),
    attachment_urls = list(
      c("https://fixture/one.pdf", ""),
      character(0),
      c(NA_character_, "https://fixture/two.pdf"),
      character(0)
    ),
    download_status = list(
      tibble::tibble(status = c("downloaded", "failed")),
      tibble::tibble(status = "cached"),
      tibble::tibble(status = "failed"),
      NULL
    ),
    discovery_log = list(NULL, NULL, NULL, NULL),
    local_path = list(NULL, NULL, NULL, NULL)
  )
}

test_that("record enrichment is explicit, scalar, and config driven", {
  config <- curation_records_test_config()
  out <- curation_enrich_records(curation_records_test_frame(), config)

  expect_equal(out$cosine_max, c(0.8, 0.7, 0.6, 0))
  expect_equal(out$n_attachments, c(1L, 0L, 1L, 0L))
  expect_equal(out$has_attachments, c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(out$n_downloaded, c(1L, 1L, 0L, 0L))
  expect_equal(out$has_downloaded, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(out$kw_total, c(3L, 3L, 4L, 0L))
  expect_true(all(c("class_label", "class_score", "class_relevant") %in% names(out)))
  expect_false(any(c("attachment_urls", "download_status", "discovery_log", "local_path") %in% names(out)))
  expect_equal(out[c("country", "document_id")], curation_records_test_frame()[c("country", "document_id")])
})

test_that("changing topics and lexicon rebuilds derived signals", {
  raw <- curation_records_test_frame()
  first <- curation_enrich_records(raw, curation_records_test_config())
  second_config <- curation_records_test_config(
    config_id = "fixture.v2",
    topics = c(solar = "solar topic"),
    lexicon = list(solar = "solar")
  )
  second <- curation_enrich_records(first, second_config)

  expect_true("relevance_score_solar" %in% names(second))
  expect_false("relevance_score_wind" %in% names(second))
  expect_false("kw_wind" %in% names(second))
  expect_equal(second$kw_total, c(0L, 2L, 2L, 0L))
  expect_equal(second$cosine_max, c(0.1, 0.7, 0.6, 0))
})

test_that("snapshot adapter uses explicit corpus and exported index seam", {
  calls <- new.env(parent = emptyenv())
  calls$cache <- character(0)
  provider <- curation_records_provider_cache(
    cache_dir = "/fixture/cache",
    index_fn = function(cache_dir, country) {
      calls$cache <- c(calls$cache, cache_dir)
      curation_records_test_frame()[
        curation_records_test_frame()$country == country,
        ,
        drop = FALSE
      ]
    }
  )
  snapshot <- curation_records_snapshot(
    provider,
    curation_records_test_config(),
    countries = c("nl", "de")
  )

  expect_equal(snapshot$country, c("nl", "nl", "de"))
  expect_equal(snapshot$document_id, c("shared-001", "nl-002", "shared-001"))
  expect_true(all(calls$cache == "/fixture/cache"))
})

test_that("random samples preserve legacy stratification and RNG state", {
  records <- curation_records_test_frame()
  set.seed(101)
  before <- .Random.seed
  first <- curation_draw_random_sample(
    records,
    countries = c("nl", "de", "at"),
    n_per_country = 1L,
    seed = 19L
  )
  expect_identical(.Random.seed, before)
  expect_equal(nrow(first), 3L)
  expect_equal(sort(first$country), c("at", "de", "nl"))
  expect_length(unique(paste(first$country, first$document_id, sep = "::")), 3L)

  rm(".Random.seed", envir = .GlobalEnv)
  invisible(curation_draw_random_sample(records, countries = "nl", n_per_country = 1L, seed = 19L))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  set.seed(101)
})

test_that("review queues filter scope and prioritize another reviewer's work", {
  records <- curation_records_test_frame()
  scope <- curation_scope("project-a", "corpus-a", "fixture.v1")
  reviews <- tibble::tibble(
    project_id = c("project-a", "project-a", "project-b"),
    corpus_id = c("corpus-a", "corpus-a", "corpus-a"),
    country = c("nl", "de", "nl"),
    document_id = c("shared-001", "shared-001", "nl-002"),
    reviewer_id = c("reviewer-2", "reviewer-1", "reviewer-2"),
    decision = c("keep", "keep", "keep"),
    deleted = c(FALSE, FALSE, FALSE),
    source = "random"
  )

  queue <- curation_build_review_queue(
    records,
    reviews,
    reviewer_id = "reviewer-1",
    scope = scope,
    countries = c("nl", "de"),
    n_per_country = 1L,
    seed = 5L
  )
  expect_identical(attr(queue, "mode"), "validate")
  expect_equal(
    sort(paste(queue$country, queue$document_id, sep = "::")),
    "nl::shared-001"
  )

  progress <- curation_country_label_counts(
    records,
    reviews,
    countries = c("nl", "de"),
    reviewer_id = "reviewer-1",
    scope = scope
  )
  expect_equal(unname(progress$labelled), c(1L, 1L))
  expect_equal(unname(progress$yours), c(0L, 1L))
  expect_equal(unname(progress$records), c(2L, 1L))
})

test_that("tombstones keep scoped review rows aligned and unseeded draws advance RNG", {
  records <- curation_records_test_frame()
  scope <- curation_scope("project-a", "corpus-a", "fixture.v1")
  reviews <- tibble::tibble(
    project_id = c("project-a", "project-b"),
    corpus_id = c("corpus-a", "corpus-a"),
    country = c("nl", "nl"),
    document_id = c("shared-001", "shared-001"),
    reviewer_id = c("reviewer-old", "reviewer-other"),
    decision = c("drop", "keep"),
    deleted = c(TRUE, FALSE),
    source = "random"
  )

  progress <- curation_country_label_counts(
    records,
    reviews,
    countries = "nl",
    scope = scope
  )
  expect_identical(progress$labelled, 0L)
  queue <- curation_build_review_queue(
    records,
    reviews,
    reviewer_id = "reviewer-new",
    countries = "nl",
    scope = scope,
    n_per_country = 1L,
    seed = 5L
  )
  expect_identical(attr(queue, "mode"), "fresh")

  set.seed(77)
  before <- .Random.seed
  on.exit(assign(".Random.seed", before, envir = .GlobalEnv), add = TRUE)
  invisible(curation_draw_random_sample(records, countries = "nl", n_per_country = 1L))
  expect_false(identical(.Random.seed, before))
})

test_that("sampling and queue helpers reject malformed inputs", {
  expect_error(
    curation_draw_random_sample(curation_records_test_frame(), n_per_country = 0),
    class = "planscanR_error_curation_sampling"
  )
  expect_error(
    curation_build_review_queue(
      curation_records_test_frame(),
      NULL,
      reviewer_id = "",
      countries = "nl",
      n_per_country = 1L
    ),
    class = "planscanR_error_curation_sampling"
  )
  expect_error(
    curation_country_label_counts(
      curation_records_test_frame(),
      tibble::tibble(country = "nl"),
      countries = "nl"
    ),
    class = "planscanR_error_curation_sampling"
  )
})
