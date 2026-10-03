skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

test_that("worker error metadata redacts credential-bearing values", {
  catalog <- acquisition_catalog_open(withr::local_tempfile(fileext = ".sqlite"))
  source <- acquisition_catalog_register_source(
    catalog,
    country = "nl",
    portal = "fixture.example",
    native_project_id = "supervision-secret",
    source_url = "https://fixture.example/supervision-secret"
  )
  job <- acquisition_queue_enqueue(catalog, "discover", "source", source$source_ref_id)

  acquisition_worker_run(
    catalog,
    worker_id = "redaction-fixture",
    max_jobs = 1L,
    handlers = list(discover = function(job, catalog) {
      list(
        status = "retryable",
        error_category = "fixture",
        error_message = paste(
          "GET https://user:password@fixture.example/path?access_token=secret-value",
          "Authorization: Basic dXNlcjpwYXNz",
          "Cookie: a=x; biogain_session=session-secret"
        )
      )
    })
  )

  attempts <- acquisition_queue_attempts(catalog, job$job_id)
  expect_length(attempts$error_message, 1L)
  expect_false(grepl("password|secret-value|dXNlcjpwYXNz|session-secret", attempts$error_message, ignore.case = TRUE))
  expect_match(attempts$error_message, "REDACTED", fixed = TRUE)
  catalog$close()
})
