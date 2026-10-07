testthat::test_that("future server retains rows beyond the display cap", {
  for (package in c("shiny", "bslib", "reactable", "plotly", "htmltools")) {
    testthat::skip_if_not_installed(package)
  }
  records <- acq_ux1_fixture_records(5004L)
  context <- acq_ux1_app_context(records)
  shiny::testServer(acq_ux1_app_server(context), {
    session$setInputs(
      countries = sort(unique(records$country)),
      stage_filter = "all"
    )
    session$flushReact()
    testthat::expect_identical(output$review_info, "5004 rows")
  })
})

testthat::test_that("display identity preserves country, document, and revision dimensions", {
  records <- acq_ux1_fixture_records(8L)
  records$document_id[c(1L, 3L, 5L)] <- "collision"
  reviews <- acq_ux1_empty_reviews()
  reviews <- rbind(
    reviews,
    data.frame(
      country = "be",
      document_id = "collision",
      reviewer_id = "fixture-reviewer",
      decision = "relevant",
      source = "browse",
      note = NA_character_,
      reviewed_at = as.POSIXct(0, origin = "1970-01-01", tz = "UTC"),
      sidecar_path = NA_character_,
      revision = 2L,
      deleted = FALSE,
      stringsAsFactors = FALSE
    )
  )
  rows <- acq_ux1_ns("curation_app_review_display_rows")(
    records,
    reviews,
    reviewer_id = "fixture-reviewer"
  )
  expect_true(all(c("country", "document_id", "revision") %in% names(rows[[1L]])))
  expect_equal(
    length(unique(vapply(rows, function(row) {
      paste(row$country, row$document_id, row$revision, sep = "::")
    }, character(1)))),
    length(rows)
  )
  expect_identical(rows[[1L]]$revision, 2L)
})

testthat::test_that("future bounded page map sends only the selected page", {
  records <- acq_ux1_fixture_records(5004L)
  metrics <- acq_ux1_widget_metrics(records, acq_ux1_fixture_config())
  testthat::expect_lte(as.integer(metrics$payload_rows), 25L)
})

testthat::test_that("future widget payload excludes hidden fields", {
  records <- acq_ux1_fixture_records(8L)
  metrics <- acq_ux1_widget_metrics(records, acq_ux1_fixture_config())
  testthat::expect_length(metrics$hidden_fixture_fields_in_payload, 0L)
})

testthat::test_that("future polling token stays stable when the source is unchanged", {
  poll_token <- acq_ux1_ns("acquisition_dashboard_poll_token")
  first <- poll_token(NULL)
  Sys.sleep(0.01)
  second <- poll_token(NULL)
  # This is deliberately a token contract characterization. It does not claim
  # to count database reads or establish that a dashboard refresh queried SQL.
  testthat::expect_identical(first, second)
})

testthat::test_that("unchanged dashboard summaries retain bounded attempts and parity", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  probe <- acq_ux1_dashboard_read_probe()
  testthat::expect_identical(probe$synthetic_attempt_rows, 1000L)
  testthat::expect_identical(probe$first_observed_completed_bytes, 500500)
  testthat::expect_identical(probe$second_observed_completed_bytes, 500500)
  testthat::expect_true(probe$repeated_summary_parity)
  testthat::expect_true(probe$query_count_within_summary_bound)
  testthat::expect_identical(probe$unaggregated_attempt_rows_materialized, 0L)
})
