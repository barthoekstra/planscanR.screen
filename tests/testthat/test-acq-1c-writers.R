# ACQ-1c: downstream writers must update only the fields they own.

acq_1c_download_status <- function(url, status, root) {
  path <- file.path(root, "files", "nl", "writer-1", "report.pdf")
  tibble::tibble(
    url = url,
    local_path = path,
    status = status,
    size_bytes = if (identical(status, "downloaded")) 11 else NA_real_,
    sha256 = if (identical(status, "downloaded")) paste(rep("a", 64), collapse = "") else NA_character_,
    reason = NA_character_
  )
}

acq_1c_record <- function(root, url, downloads, relevance = NULL, classification = NULL, translation = NULL) {
  record <- tibble::tibble(
    country = "nl",
    source_portal = "fixture",
    document_id = "writer-1",
    url = "https://fixture/records/writer-1",
    retrieved_at = as.POSIXct("2026-10-03 12:00:00", tz = "UTC"),
    attachment_urls = list(url),
    local_path = list(downloads$local_path),
    title = "Wind project",
    summary = "Wind energy",
    download_status = list(downloads)
  )
  if (!is.null(relevance)) {
    record$relevance_score_wind <- relevance
    record$relevance_model <- "fresh-relevance"
  }
  if (!is.null(classification)) {
    record$class_label <- classification$label
    record$class_score <- classification$score
    record$class_relevant <- classification$relevant
    record$class_model <- classification$model
    record$class_score_wind <- classification$score
    record$class_score_other <- 1 - classification$score
  }
  if (!is.null(translation)) {
    record$translation_title_en <- translation$title
    record$translation_summary_en <- translation$summary
    record$translation_src_lang <- "nld_Latn"
    record$translation_service <- "fixture-translation"
  }
  record
}

test_that("relevance writer preserves fresh class, translation, and download outcomes", {
  withr::with_tempdir({
    cache <- file.path(getwd(), "cache")
    options(planscanR.cache_dir = cache)
    on.exit(options(planscanR.cache_dir = NULL), add = TRUE)
    url <- "https://fixture/files/writer-1/report.pdf"
    fresh_download <- acq_1c_download_status(url, "downloaded", cache)
    fresh <- acq_1c_record(
      cache,
      url,
      fresh_download,
      relevance = 0.25,
      classification = list(label = "fresh", score = 0.91, relevant = TRUE, model = "fresh-classifier"),
      translation = list(title = "Fresh translated title", summary = "Fresh translated summary")
    )
    fresh$relevance_score_solar <- 0.88
    planscanR::write_record_sidecar(fresh, downloads = fresh_download, root = cache)

    stale <- acq_1c_record(
      cache,
      url,
      acq_1c_download_status(url, "pending", cache),
      classification = list(label = "stale", score = 0.11, relevant = FALSE, model = "stale-classifier"),
      translation = list(title = "Stale translated title", summary = "Stale translated summary")
    )
    stale$relevance_score_solar <- 0.12
    scored <- score_assessments(
      stale,
      topic = c(wind = "wind energy"),
      model = make_fake_model(),
      write_sidecar = TRUE
    )

    path <- file.path(cache, "files", "nl", "writer-1", "writer-1.meta.json")
    out <- planscanR::read_record_sidecar(path)
    expect_identical(out$class_label, "fresh")
    expect_identical(out$translation_title_en, "Fresh translated title")
    expect_identical(out$download_status[[1]]$status, "downloaded")
    expect_equal(out$relevance_score_wind, scored$relevance_score_wind, tolerance = 1e-3)
    expect_identical(out$relevance_score_solar, 0.88)
  })
})

test_that("classification writer preserves fresh relevance, translation, and download outcomes", {
  withr::with_tempdir({
    cache <- file.path(getwd(), "cache")
    options(planscanR.cache_dir = cache)
    on.exit(options(planscanR.cache_dir = NULL), add = TRUE)
    url <- "https://fixture/files/writer-1/report.pdf"
    fresh_download <- acq_1c_download_status(url, "downloaded", cache)
    fresh <- acq_1c_record(
      cache,
      url,
      fresh_download,
      relevance = 0.88,
      classification = list(label = "old", score = 0.62, relevant = TRUE, model = "old-classifier"),
      translation = list(title = "Fresh translated title", summary = "Fresh translated summary")
    )
    planscanR::write_record_sidecar(fresh, downloads = fresh_download, root = cache)

    stale <- acq_1c_record(
      cache,
      url,
      acq_1c_download_status(url, "pending", cache),
      relevance = 0.12,
      translation = list(title = "Stale translated title", summary = "Stale translated summary")
    )
    labels <- c(wind = "wind", other = "other")
    attr(labels, "relevant") <- "wind"
    classifier_fixture <- classifier(
      "fixture-classifier",
      function(x, labels, multi_label) {
        matrix(
          rep(c(0.93, 0.07), length.out = length(x) * length(labels)),
          nrow = length(x),
          ncol = length(labels),
          byrow = TRUE,
          dimnames = list(NULL, names(labels))
        )
      }
    )
    classified <- classify_assessments(
      stale,
      classifier = classifier_fixture,
      labels = labels,
      write_sidecar = TRUE
    )

    path <- file.path(cache, "files", "nl", "writer-1", "writer-1.meta.json")
    out <- planscanR::read_record_sidecar(path)
    expect_identical(out$relevance_score_wind, 0.88)
    expect_identical(out$translation_title_en, "Fresh translated title")
    expect_identical(out$download_status[[1]]$status, "downloaded")
    expect_identical(out$class_label, classified$class_label)
  })
})

test_that("a missing sidecar is created from source metadata plus owned scores", {
  withr::with_tempdir({
    cache <- file.path(getwd(), "cache")
    options(planscanR.cache_dir = cache)
    on.exit(options(planscanR.cache_dir = NULL), add = TRUE)
    url <- "https://fixture/files/writer-1/report.pdf"
    record <- acq_1c_record(
      cache,
      url,
      acq_1c_download_status(url, "downloaded", cache),
      classification = list(label = "stale", score = 0.11, relevant = FALSE, model = "stale-classifier"),
      translation = list(title = "Stale translated title", summary = "Stale translated summary")
    )
    record$relevance_score_solar <- 0.12
    score_assessments(
      record,
      topic = c(wind = "wind energy"),
      model = make_fake_model(),
      write_sidecar = TRUE
    )
    path <- file.path(cache, "files", "nl", "writer-1", "writer-1.meta.json")
    out <- planscanR::read_record_sidecar(path)
    expect_true("relevance_score_wind" %in% names(out))
    expect_false("relevance_score_solar" %in% names(out))
    expect_false("class_label" %in% names(out))
    expect_false("translation_title_en" %in% names(out))
    expect_identical(out$download_status[[1]]$status, "downloaded")
  })
})
