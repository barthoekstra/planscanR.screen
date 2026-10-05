test_that("historical review rows retain source tags and expose safe linkage states", {
  reviews <- data.frame(
    country = c("nl", "nl", "de"),
    document_id = c("one", "two", "three"),
    reviewer_id = "stable-a",
    decision = c("keep", "drop", "keep"),
    source = c("random", "random", "browse"),
    import_row_id = c("nl::one::stable-a", "nl::two::stable-a", "de::three::stable-a"),
    stringsAsFactors = FALSE
  )
  historical <- data.frame(
    import_row_id = reviews$import_row_id,
    country = reviews$country,
    document_id = reviews$document_id,
    legacy_reviewer = "Reviewer A",
    original_source = c("random", "browse", "browse"),
    imported_source = reviews$source,
    historical_linkage = c("matched", "pending", "not_seen_in_returned_metadata"),
    current_linkage = c("current_unique", "historical_only", "ambiguous"),
    current_match_count = c(1L, 0L, 2L),
    mutation_allowed = c(TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  out <- curation_app_historical_review_rows(reviews, historical)
  expect_identical(out$original_source, historical$original_source)
  expect_identical(out$imported_source, historical$imported_source)
  expect_identical(out$linkage_status, c("current_unique", "historical_only", "ambiguous"))
  expect_identical(out$mutation_allowed, c(TRUE, FALSE, FALSE))
  expect_true(curation_app_historical_mutation_allowed(historical, "nl", "one"))
  expect_false(curation_app_historical_mutation_allowed(historical, "nl", "two"))
  expect_false(curation_app_historical_mutation_allowed(historical, "de", "three"))

  unrelated <- reviews[1L, , drop = FALSE]
  unrelated$country <- "fr"
  unrelated$document_id <- "outside-cohort"
  unrelated$import_row_id <- NA_character_
  out_with_unrelated <- curation_app_historical_review_rows(
    rbind(reviews, unrelated),
    historical
  )
  expect_true(out_with_unrelated$historical_mutation_allowed[[4L]])
})

test_that("historical badges distinguish corrected source tags and unresolved linkage", {
  skip_if_not_installed("htmltools")
  corrected <- data.frame(
    original_source = "browse",
    imported_source = "random",
    linkage_status = "current_unique",
    mutation_allowed = TRUE,
    stringsAsFactors = FALSE
  )
  unresolved <- data.frame(
    original_source = "browse",
    imported_source = "browse",
    linkage_status = "historical_only",
    mutation_allowed = FALSE,
    stringsAsFactors = FALSE
  )
  corrected_html <- as.character(curation_app_historical_badge(corrected))
  unresolved_html <- as.character(curation_app_historical_badge(unresolved))
  expect_match(corrected_html, "original browse", fixed = TRUE)
  expect_match(corrected_html, "imported random", fixed = TRUE)
  expect_match(corrected_html, "current match", fixed = TRUE)
  expect_match(unresolved_html, "historical only", fixed = TRUE)
  expect_match(unresolved_html, "read-only", fixed = TRUE)
})

test_that("ordinary tables retain their columns without historical context", {
  rows <- curation_app_historical_record_rows(
    data.frame(country = "nl", document_id = "one", stringsAsFactors = FALSE),
    NULL
  )
  expect_true("historical_badge" %in% names(rows))
  expect_true("historical_mutation_allowed" %in% names(rows))
  expect_true(rows$historical_mutation_allowed[[1L]])
  expect_identical(rows$historical_badge[[1L]], "")
})

test_that("historical-only labels remain visible as read-only audit rows", {
  links <- data.frame(
    country = c("nl", "de"),
    document_id = c("one", "two"),
    original_source = c("browse", "random"),
    imported_source = c("random", "random"),
    historical_reviewer = c("Reviewer A", "Reviewer B"),
    historical_linkage = c("matched", "pending"),
    current_linkage = c("current_unique", "historical_only"),
    current_match_count = c(1L, 0L),
    mutation_allowed = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  current <- data.frame(country = "nl", document_id = "one", stringsAsFactors = FALSE)
  rows <- curation_app_historical_only_records(links, current)
  expect_identical(rows$document_id, "two")
  expect_false(rows$historical_mutation_allowed[[1L]])
  expect_match(rows$historical_badge[[1L]], "Reviewer B", fixed = TRUE)

  reviews <- data.frame(
    country = "de",
    document_id = "two",
    reviewer_id = "stable-b",
    decision = "keep",
    source = "legacy-import",
    note = "retained operator note",
    reviewed_at = "2026-10-05T10:00:00Z",
    import_row_id = "de::two",
    stringsAsFactors = FALSE
  )
  rows_with_review <- curation_app_historical_only_records(links, current, reviews)
  expect_identical(rows_with_review$decision[[1L]], "keep")
  expect_identical(rows_with_review$note[[1L]], "retained operator note")
  expect_identical(rows_with_review$reviewed_at[[1L]], "2026-10-05T10:00:00Z")
})

test_that("decision cells carry an immutable linkage guard", {
  skip_if_not_installed("reactable")
  js <- as.character(curation_app_decision_cell("random", "stable-a"))
  expect_match(js, "historical_mutation_allowed", fixed = TRUE)
  expect_match(js, "data-mutation-allowed", fixed = TRUE)
  expect_match(js, "Historical linkage is unresolved", fixed = TRUE)
})
