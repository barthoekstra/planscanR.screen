if (!exists("acq_ux1_fixture_records", mode = "function")) {
  runner_path <- testthat::test_path(
    "..",
    "..",
    "inst",
    "curation",
    "acq_ux1_characterization.R"
  )
  if (!file.exists(runner_path)) {
    runner_path <- file.path(getwd(), "inst", "curation", "acq_ux1_characterization.R")
  }
  source(normalizePath(runner_path, mustWork = TRUE), local = FALSE)
}

testthat::test_that("ACQ-UX1 fixture and current widget contract are measurable", {
  records <- acq_ux1_fixture_records()
  testthat::expect_equal(nrow(records), 128000L)
  testthat::expect_equal(length(unique(paste(records$country, records$document_id, sep = "::"))), 128000L)
  testthat::expect_equal(sum(duplicated(records$document_id)), 96000L)

  metrics <- acq_ux1_widget_metrics(
    records[seq_len(25L), , drop = FALSE],
    acq_ux1_fixture_config()
  )
  testthat::expect_equal(metrics$payload_rows, 25L)
  testthat::expect_equal(metrics$page_size, 25)
  testthat::expect_equal(metrics$page_size_options, list(25, 50, 100, 250))
  testthat::expect_true(all(c("internal_note", "source_payload") %in% metrics$hidden_fixture_fields_in_payload))
})
