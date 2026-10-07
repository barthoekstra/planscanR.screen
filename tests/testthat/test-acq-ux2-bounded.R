# ACQ-UX2: complete-result queries with bounded browser pages.

testthat::test_that("large curation results page without a hidden 5000-row cap", {
  records <- data.frame(
    country = rep(c("nl", "de", "be", "fr"), length.out = 128000L),
    document_id = paste0("document-", seq_len(128000L)),
    title = paste0("Project ", seq_len(128000L)),
    portal = rep(c("portal-a", "portal-b"), length.out = 128000L),
    stringsAsFactors = FALSE
  )
  page <- curation_app_page_rows(records, page = 512L, page_size = 250L)
  testthat::expect_lte(nrow(page), 250L)
  testthat::expect_identical(attr(page, "total_count"), 128000L)
  testthat::expect_identical(page$document_id[[1L]], "document-127751")

  found <- curation_app_query_rows(records, search = "DOCUMENT-120000")
  testthat::expect_identical(nrow(found), 1L)
  testthat::expect_identical(found$document_id[[1L]], "document-120000")

  sorted <- curation_app_query_rows(records[1:3, , drop = FALSE], sort_by = "title", sort_desc = TRUE)
  testthat::expect_identical(sorted$title[[1L]], "Project 3")
})

testthat::test_that("curation browser rows are compact and identity-safe", {
  records <- data.frame(
    country = c("nl", "de", "nl"),
    document_id = c("same", "same", "other"),
    portal = c("portal-a", "portal-b", "portal-a"),
    source_ref_id = c("source-a", "source-b", "source-c"),
    title = c("A", "B", "C"),
    summary = "hidden summary",
    url = "https://fixture.invalid/detail",
    internal_note = "hidden note",
    source_payload = "hidden payload",
    cosine_max = c(.9, .8, .7),
    selected = TRUE,
    stringsAsFactors = FALSE
  )
  compact <- curation_app_compact_display(records)
  testthat::expect_false(any(c("summary", "url", "internal_note", "source_payload") %in% names(compact)))
  testthat::expect_true(all(c("country", "portal", "document_id", "title") %in% names(compact)))
  blind <- curation_app_compact_display(records, blind = TRUE)
  testthat::expect_false("cosine_max" %in% names(blind))

  hit <- curation_app_record_from_identity(records, "de", "same", portal = "portal-b")
  testthat::expect_identical(hit$source_ref_id[[1L]], "source-b")
  testthat::expect_null(curation_app_record_from_identity(
    rbind(records[1L, , drop = FALSE], records[1L, , drop = FALSE]), "nl", "same"
  ))
})

testthat::test_that("reactable receives one page without hidden detail fields", {
  testthat::skip_if_not_installed("reactable")
  records <- data.frame(
    country = rep("nl", 40L),
    document_id = paste0("doc-", seq_len(40L)),
    portal = rep("portal-a", 40L),
    source_ref_id = paste0("source-", seq_len(40L)),
    title = paste0("Title ", seq_len(40L)),
    summary = "private summary",
    url = "https://fixture.invalid/private",
    internal_note = "private note",
    source_payload = "private payload",
    selected = TRUE,
    stringsAsFactors = FALSE
  )
  widget <- curation_app_table(
    records,
    reviews = data.frame(),
    config = list(translator = NULL),
    reviewer_id = "fixture-reviewer",
    page = 2L,
    page_size = 25L
  )
  payload <- jsonlite::fromJSON(widget$x$tag$attribs$data)
  testthat::expect_lte(length(payload[[1L]]), 25L)
  testthat::expect_false(any(c("summary", "url", "internal_note", "source_payload") %in% names(payload)))
  testthat::expect_true(all(c("country", "portal", "document_id", "title") %in% names(payload)))

  sorted_page <- curation_app_page_rows(
    curation_app_query_rows(records, sort_by = "country", sort_desc = TRUE),
    page = 2L, page_size = 25L
  )
  sorted_widget <- curation_app_table(
    records,
    reviews = data.frame(),
    config = list(translator = NULL),
    reviewer_id = "fixture-reviewer",
    page = 2L,
    page_size = 25L,
    sort_by = "country",
    sort_desc = TRUE
  )
  sorted_payload <- jsonlite::fromJSON(sorted_widget$x$tag$attribs$data)
  testthat::expect_identical(as.character(sorted_payload$document_id), as.character(sorted_page$document_id))

  random_widget <- curation_app_table(
    records,
    reviews = data.frame(),
    config = list(translator = NULL),
    source = "random",
    reviewer_id = "fixture-reviewer",
    page = 2L,
    page_size = 25L,
    sort_by = "country",
    sort_desc = TRUE,
    detail_input_id = "random_detail"
  )
  random_payload <- jsonlite::fromJSON(random_widget$x$tag$attribs$data)
  testthat::expect_identical(as.character(random_payload$document_id), as.character(sorted_page$document_id))
  testthat::expect_match(as.character(random_widget$x$tag$attribs$onClick), "random_detail", fixed = TRUE)

  links <- data.frame(
    import_row_id = "legacy-1", country = "nl", document_id = "doc-1",
    original_source = "archive-a", imported_source = "portal-a",
    historical_reviewer = "reviewer-a", historical_linkage = "historical-only",
    current_linkage = "current-match", current_match_count = 1L,
    linkage_status = "current_unique", mutation_allowed = FALSE,
    stringsAsFactors = FALSE
  )
  badge_widget <- curation_app_table(
    records[1L, , drop = FALSE],
    reviews = data.frame(),
    config = list(translator = NULL),
    reviewer_id = "fixture-reviewer",
    historical_links = links
  )
  badge_payload <- jsonlite::fromJSON(badge_widget$x$tag$attribs$data)
  testthat::expect_match(as.character(badge_payload$historical_badge[[1L]]), "archive-a", fixed = TRUE)
})

testthat::test_that("historical and admin widgets receive bounded visible rows", {
  testthat::skip_if_not_installed("reactable")
  n <- 300L
  historical <- data.frame(
    country = rep(c("nl", "de"), length.out = n),
    document_id = paste0("historical-", seq_len(n)),
    historical_source = rep("legacy", n),
    current_match_count = 0L,
    stringsAsFactors = FALSE
  )
  historical_widget <- curation_app_historical_only_table(historical, page = 2L, page_size = 250L)
  historical_payload <- jsonlite::fromJSON(historical_widget$x$tag$attribs$data)
  testthat::expect_lte(length(historical_payload[[1L]]), 250L)
  testthat::expect_false("summary" %in% names(historical_payload))
  testthat::expect_equal(
    nrow(curation_app_query_rows(historical, search = "legacy")),
    n
  )

  reviews <- data.frame(
    country = rep(c("nl", "de"), length.out = n),
    document_id = paste0("review-", seq_len(n)),
    reviewer_id = rep("reviewer-a", n),
    decision = rep("keep", n),
    source = rep("browse", n),
    note = rep("private note", n),
    reviewed_at = as.POSIXct("2026-01-01", tz = "UTC"),
    sidecar_path = rep("private-sidecar", n),
    revision = seq_len(n),
    deleted = FALSE,
    stringsAsFactors = FALSE
  )
  admin_widget <- curation_app_admin_table(reviews, page = 2L, page_size = 250L)
  admin_payload <- jsonlite::fromJSON(admin_widget$x$tag$attribs$data)
  testthat::expect_lte(length(admin_payload[[1L]]), 250L)
  testthat::expect_false(any(c("note", "sidecar_path") %in% names(admin_payload)))
  testthat::expect_equal(nrow(curation_app_query_rows(reviews, search = "reviewer-a")), n)
})

testthat::test_that("historical linkage keeps duplicate identities ambiguous", {
  links <- data.frame(
    import_row_id = c("legacy-a", "legacy-b", "legacy-c"),
    country = c("nl", "nl", "de"),
    document_id = c("duplicate", "duplicate", "unique"),
    current_match_count = c(1L, 1L, 0L),
    stringsAsFactors = FALSE
  )
  records <- data.frame(
    country = c("nl", "de"), document_id = c("duplicate", "unique"),
    stringsAsFactors = FALSE
  )
  enriched <- curation_app_historical_record_rows(records, links)
  testthat::expect_false(enriched$historical_mutation_allowed[[1L]])
  testthat::expect_false(enriched$historical_mutation_allowed[[2L]])

  reviews <- data.frame(
    country = c("nl", "nl"), document_id = c("duplicate", "duplicate"),
    reviewer_id = c("reviewer-a", "reviewer-a"), decision = "keep", source = "legacy",
    import_row_id = c("legacy-a", NA_character_), revision = c(1L, 1L), deleted = FALSE,
    stringsAsFactors = FALSE
  )
  review_rows <- curation_app_historical_review_rows(reviews, links)
  testthat::expect_true(review_rows$historical_mutation_allowed[[1L]])
  testthat::expect_false(review_rows$historical_mutation_allowed[[2L]])
})

testthat::test_that("acquisition preview item pages retain title and portal identity", {
  preview <- list(items = data.frame(
    title = c("Zulu", "Alpha", "Alpha"),
    country = c("nl", "nl", "de"),
    portal = c("portal-a", "portal-a", "portal-b"),
    native_project_id = c("p-1", "p-2", "p-3"),
    source_ref_id = c("s-1", "s-2", "s-3"),
    probability = c(.2, .9, .8),
    selected = c(FALSE, TRUE, TRUE),
    energy_kind = c("wind", "solar", "wind"),
    priority = c(1L, 10L, 5L),
    priority_rule_id = c("fallback", "solar", "wind"),
    priority_explanation = c("fallback", "solar", "wind"),
    exclusion_reason = c("below threshold", NA, NA),
    stringsAsFactors = FALSE
  ))
  page <- acquisition_campaign_preview_items_page(preview, sort_by = "title", page = 1L, page_size = 2L)
  testthat::expect_lte(nrow(page), 2L)
  testthat::expect_identical(attr(page, "total_count"), 3L)
  testthat::expect_identical(page$title[[1L]], "Alpha")
  testthat::expect_true(all(c("title", "country", "portal", "source_ref_id") %in% names(page)))

  large_n <- 128000L
  large_preview <- list(items = data.frame(
    title = paste0("Project ", seq_len(large_n)),
    country = rep("nl", large_n),
    portal = rep("portal-a", large_n),
    native_project_id = paste0("p-", seq_len(large_n)),
    source_ref_id = paste0("s-", seq_len(large_n)),
    probability = rep(.8, large_n),
    selected = rep(TRUE, large_n),
    stringsAsFactors = FALSE
  ))
  large_page <- acquisition_campaign_preview_items_page(large_preview, page = 512L, page_size = 250L)
  testthat::expect_lte(nrow(large_page), 250L)
  testthat::expect_identical(attr(large_page, "total_count"), large_n)
  testthat::expect_identical(large_page$source_ref_id[[1L]], "s-127751")
})
