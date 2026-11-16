if (!exists("acq_ux1_fixture_records", mode = "function")) {
  source_root <- screen_test_source_root()
  runner_path <- if (identical(source_root, "__installed_namespace__")) {
    system.file("curation", "acq_ux1_characterization.R", package = "planscanR.screen")
  } else {
    file.path(source_root, "inst", "curation", "acq_ux1_characterization.R")
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
  # The server pages rows before rendering and strips source-only fields from
  # the browser payload. Client-side page-size controls are intentionally off.
  testthat::expect_null(metrics$page_size_options)
  testthat::expect_length(metrics$hidden_fixture_fields_in_payload, 0L)
})
