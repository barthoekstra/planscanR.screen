worker_startup_helper <- function() {
  path <- testthat::test_path("..", "..", "inst", "worker", "acquisition_worker.R")
  expressions <- parse(path)
  helper <- Filter(function(expr) {
    is.call(expr) && identical(expr[[1L]], as.name("<-")) &&
      identical(expr[[2L]], as.name("acquisition_worker_open_catalog"))
  }, as.list(expressions))
  environment <- new.env(parent = baseenv())
  eval(helper[[1L]], environment)
  environment$acquisition_worker_open_catalog
}

test_that("idle workers retry typed busy failures and return the recovered catalog", {
  open_catalog <- worker_startup_helper()
  attempts <- 0L
  waits <- numeric()
  busy <- structure(list(message = "database is locked", call = NULL),
                    class = c("planscanR_error_acquisition_busy", "error", "condition"))
  opener <- function(path, busy_timeout) {
    expect_identical(path, "fixture.sqlite")
    expect_identical(busy_timeout, 30000L)
    attempts <<- attempts + 1L
    if (attempts < 3L) stop(busy)
    list(ready = TRUE)
  }
  result <- suppressMessages(open_catalog(
    "fixture.sqlite", retry = TRUE, opener = opener,
    pause = function(seconds) waits <<- c(waits, seconds)
  ))
  expect_identical(result, list(ready = TRUE))
  expect_identical(attempts, 3L)
  expect_identical(waits, c(15, 15))
})

test_that("one-shot workers and non-busy startup errors still fail", {
  open_catalog <- worker_startup_helper()
  busy <- structure(list(message = "database is locked", call = NULL),
                    class = c("planscanR_error_acquisition_busy", "error", "condition"))
  pause <- function(seconds) stop("must not sleep")
  expect_error(open_catalog("fixture", opener = function(...) stop(busy), pause = pause),
               class = "planscanR_error_acquisition_busy")
  expect_error(open_catalog("fixture", retry = TRUE,
                           opener = function(...) stop("invalid schema"), pause = pause),
               "invalid schema")
})
