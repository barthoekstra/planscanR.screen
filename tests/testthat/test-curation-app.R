test_that("curation_app exposes an injected app factory", {
  expect_true(exists("curation_app", asNamespace("planscanR.screen"), inherits = FALSE))
})

test_that("headless detail render works with translation disabled and sparse records", {
  skip_if_not_installed("htmltools")
  cfg <- curation_config(
    project_id = "detail-project",
    corpus_id = "detail-corpus",
    config_id = "detail.v1",
    topics = c(topic = "topic"),
    labels = c(keep = "Keep", drop = "Drop"),
    lexicon = list(topic = "topic"),
    select_fn = function(records, controls) {
      records$selected <- rep(FALSE, nrow(records))
      records
    },
    selection_controls = list(),
    funnel_stages = list(list(
      key = "indexed", label = "Indexed",
      fn = function(records, context) rep(TRUE, nrow(records))
    )),
    branding = list(title = "Detail fixture", primary = "#123456"),
    artifact_paths = list(root = tempfile("curation-detail-")),
    translator = NULL
  )
  row <- data.frame(
    country = "nl",
    document_id = "same-native-id",
    title = "A sparse record",
    summary = "Original summary",
    stringsAsFactors = FALSE
  )
  expect_no_error(curation_app_record_detail(row, cfg))
  if (requireNamespace("shiny", quietly = TRUE)) {
    expect_no_error(curation_app_single_record_ui(row, cfg))
  }
})

test_that("sparse review tables tolerate no matching reviews", {
  for (package in c("reactable", "htmltools")) {
    skip_if_not_installed(package)
  }
  cfg <- curation_config(
    project_id = "table-project",
    corpus_id = "table-corpus",
    config_id = "table.v1",
    topics = c(topic = "topic"),
    labels = c(keep = "Keep", drop = "Drop"),
    lexicon = list(topic = "topic"),
    select_fn = function(records, controls) {
      records$selected <- rep(FALSE, nrow(records))
      records
    },
    selection_controls = list(),
    funnel_stages = list(list(
      key = "indexed", label = "Indexed",
      fn = function(records, context) rep(TRUE, nrow(records))
    )),
    branding = list(title = "Table fixture", primary = "#123456"),
    artifact_paths = list(root = tempfile("curation-table-")),
    translator = NULL
  )
  sparse <- data.frame(
    country = "nl",
    document_id = "sparse-record",
    title = "Sparse record",
    stringsAsFactors = FALSE
  )
  other_reviewer <- data.frame(
    country = "de",
    document_id = "other-record",
    reviewer_id = "bob",
    decision = "keep",
    source = "browse",
    stringsAsFactors = FALSE
  )
  expect_no_error(
    curation_app_table(
      sparse,
      curation_app_empty_reviews(),
      cfg,
      reviewer_id = "alice"
    )
  )
  expect_no_error(curation_app_table(sparse, other_reviewer, cfg, reviewer_id = "alice"))
})

test_that("decision cells retain provenance and encode dynamic HTML values", {
  skip_if_not_installed("reactable")
  js <- as.character(curation_app_decision_cell("random\nrow", "fixture'reviewer"))
  expect_match(js, "var src = 'random\\nrow';", fixed = TRUE)
  expect_match(js, "var rid = 'fixture\\'reviewer';", fixed = TRUE)
  expect_match(js, "data-record-id", fixed = TRUE)
  expect_match(js, "data-revision", fixed = TRUE)
  expect_match(js, "var revision", fixed = TRUE)
  expect_match(js, "replace(/&/g", fixed = TRUE)
  expect_false(grepl("onclick=", js, fixed = TRUE))
})

test_that("display rows carry the immutable review revision", {
  rows <- data.frame(
    country = c("nl", "de"),
    document_id = c("one", "two"),
    title = c("One", "Two"),
    stringsAsFactors = FALSE
  )
  reviews <- data.frame(
    country = "nl", document_id = "one", reviewer_id = "alice",
    decision = "keep", source = "browse", revision = 4L,
    deleted = FALSE, stringsAsFactors = FALSE
  )
  displayed <- curation_app_review_display_rows(rows, reviews, "alice")
  expect_identical(vapply(displayed, function(row) row$index, integer(1)), c(0L, 1L))
  expect_identical(vapply(displayed, function(row) row$revision, integer(1)), c(4L, 0L))
})

test_that("the UI keeps browse, random, funnel, model, and stepper controls", {
  for (package in c("shiny", "bslib", "reactable", "plotly", "htmltools")) {
    skip_if_not_installed(package)
  }
  stages <- list(list(
    key = "indexed", label = "Indexed",
    fn = function(records, context) rep(TRUE, nrow(records))
  ))
  cfg <- curation_config(
    project_id = "ui-project", corpus_id = "ui-corpus", config_id = "ui.v1",
    topics = c(topic = "topic"), labels = c(keep = "Keep", drop = "Drop"),
    lexicon = list(topic = "topic"),
    select_fn = function(records, controls) {
      records$selected <- rep(FALSE, nrow(records))
      records
    },
    selection_controls = list(
      relevance_threshold = 0.7, kw_min = 3L,
      use_classifier = FALSE, use_trim = FALSE,
      nonrenewable = character(0), nonrenewable_score = 0.5
    ),
    funnel_stages = stages,
    branding = list(title = "UI fixture", primary = "#123456"),
    artifact_paths = list(root = tempfile("curation-ui-")),
    translator = NULL
  )
  ui <- curation_app_ui(cfg)
  expect_s3_class(ui, "shiny.tag.list")
  # Serializing the tag tree avoids forcing bslib's Sass compiler (and its
  # user-level cache) during an offline unit test.
  html <- paste(rawToChar(serialize(ui, NULL, ascii = TRUE), multiple = TRUE), collapse = "")
  for (id in c(
    "review_tbl", "random_tbl", "funnel_plot", "funnel_table",
    "train_model", "compute_lc", "rebuild", "sr_prev", "sr_next",
    "mark_keep", "mark_clear", "rnd_draw", "rnd_blind"
  )) {
    expect_match(html, id, fixed = TRUE)
  }
  expect_match(html, "sr_decision", fixed = TRUE)
  expect_match(html, "sr_displayed_document_id", fixed = TRUE)
  expect_match(html, "admin_review_ui", fixed = TRUE)
  expect_match(html, "curation-review-card", fixed = TRUE)
  expect_match(html, "curation-admin-card", fixed = TRUE)
})

test_that("a deployment supplied logout control is a visible POST form", {
  for (package in c("shiny", "bslib", "reactable", "plotly", "htmltools")) {
    skip_if_not_installed(package)
  }
  cfg <- curation_config(
    project_id = "logout-project", corpus_id = "logout-corpus", config_id = "logout.v1",
    topics = c(topic = "topic"), labels = c(keep = "Keep", drop = "Drop"),
    lexicon = list(topic = "topic"),
    select_fn = function(records, controls) {
      records$selected <- rep(FALSE, nrow(records))
      records
    },
    selection_controls = list(),
    funnel_stages = list(list(
      key = "indexed", label = "Indexed",
      fn = function(records, context) rep(TRUE, nrow(records))
    )),
    branding = list(title = "Logout fixture", primary = "#123456"),
    artifact_paths = list(root = tempfile("curation-logout-")),
    translator = NULL
  )
  render <- function(logout_path = NULL) {
    htmltools::renderTags(curation_app_ui(cfg, logout_path = logout_path))$html
  }
  expect_false(grepl("Log out", render(), fixed = TRUE))
  logout_html <- render("/app/logout")
  expect_match(logout_html, "action=\"/app/logout\"", fixed = TRUE)
  expect_match(logout_html, "method=\"post\"", fixed = TRUE)
  expect_match(logout_html, "Log out", fixed = TRUE)
})
