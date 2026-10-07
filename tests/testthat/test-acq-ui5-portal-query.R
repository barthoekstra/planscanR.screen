testthat::test_that("portal-aware record lookup uses the composite source index", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("openssl")
  catalog <- acquisition_catalog_open(file.path(withr::local_tempdir(), "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  acquisition_catalog_register_source(catalog, "nl", "portal-a", "project-1")
  records <- data.frame(
    country = "nl",
    portal = "portal-a",
    document_id = "project-1",
    select_prob = 0.9,
    stringsAsFactors = FALSE
  )
  trace_env <- new.env(parent = emptyenv())
  trace_env$queries <- character()
  assign("acq_ui5_query_trace_env", trace_env, envir = .GlobalEnv)
  on.exit(rm("acq_ui5_query_trace_env", envir = .GlobalEnv), add = TRUE)
  trace(
    DBI::dbSendQuery,
    tracer = quote({
      trace_env <- get("acq_ui5_query_trace_env", envir = .GlobalEnv)
      trace_env$queries <- c(trace_env$queries, statement)
    }),
    print = FALSE
  )
  on.exit(untrace("dbSendQuery", where = getNamespace("DBI")), add = TRUE)

  resolved <- acquisition_campaign_selection_from_records(catalog, records, 0.5)
  query <- trace_env$queries[grepl("FROM acquisition_sources", trace_env$queries, fixed = TRUE)]
  testthat::expect_length(query, 1L)
  testthat::expect_match(
    query,
    "country = \\? AND portal = \\? AND native_project_id = \\?"
  )
  plan <- DBI::dbGetQuery(
    catalog$conn,
    paste("EXPLAIN QUERY PLAN", query),
    params = list("nl", "portal-a", "project-1")
  )
  testthat::expect_true(
    any(grepl("USING INDEX .*\\(country=\\?.*portal=\\?.*native_project_id=\\?\\)", plan$detail))
  )
  testthat::expect_identical(resolved$portal, "portal-a")
})
