test_that("the acquisition dashboard has a namespaced module UI", {
  testthat::skip_if_not_installed("shiny")
  ui <- acquisition_dashboard_ui("acquisition")
  html <- htmltools::renderTags(ui)$html

  testthat::expect_match(html, "acquisition-dashboard-actor", fixed = TRUE)
  testthat::expect_match(html, "acquisition-dashboard-queue", fixed = TRUE)
  testthat::expect_false(grepl('id="dashboard-actor"', html, fixed = TRUE))
})

test_that("curation exposes acquisition only through optional service injection", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not_installed("reactable")
  testthat::skip_if_not_installed("plotly")
  testthat::skip_if_not_installed("htmltools")

  config <- curation_config(
    project_id = "ui2-project",
    corpus_id = "ui2-corpus",
    config_id = "ui2.v1",
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
    branding = list(title = "UI2 fixture", primary = "#123456"),
    artifact_paths = list(root = tempfile("curation-ui2-")),
    translator = NULL
  )

  without <- htmltools::renderTags(curation_app_ui(config))$html
  with <- htmltools::renderTags(
    curation_app_ui(config, acquisition = list(available = FALSE, reason = "fixture unavailable"))
  )$html
  testthat::expect_false(grepl("Acquisition", without, fixed = TRUE))
  testthat::expect_match(with, "Acquisition", fixed = TRUE)
  testthat::expect_match(with, "fixture unavailable", fixed = TRUE)
  testthat::expect_true("acquisition" %in% names(formals(curation_app)))
  testthat::expect_true("acquisition" %in% names(formals(run_curation)))
})

test_that("the dashboard server is a reusable module entry point", {
  testthat::skip_if_not_installed("shiny")
  testthat::expect_true(is.function(acquisition_dashboard_server))
  testthat::expect_true("id" %in% names(formals(acquisition_dashboard_server)))
  testthat::expect_true("catalog" %in% names(formals(acquisition_dashboard_server)))
})

test_that("standalone and embedded dashboard controls share one namespace contract", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("bslib")
  testthat::skip_if_not_installed("reactable")
  testthat::skip_if_not_installed("plotly")
  testthat::skip_if_not_installed("htmltools")

  config <- curation_config(
    project_id = "ui2-parity-project",
    corpus_id = "ui2-parity-corpus",
    config_id = "ui2.v1",
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
    branding = list(title = "UI2 parity", primary = "#123456"),
    artifact_paths = list(root = tempfile("curation-ui2-parity-")),
    translator = NULL
  )
  catalog <- structure(list(), class = "planscanR_acquisition_catalog")
  standalone <- htmltools::renderTags(acquisition_dashboard_ui("standalone"))$html
  embedded <- htmltools::renderTags(
    curation_app_ui(config, acquisition = catalog)
  )$html

  controls <- c(
    "dashboard-actor", "dashboard-progress", "dashboard-project-counts",
    "dashboard-file-counts", "dashboard-stage-counts", "dashboard-workers",
    "dashboard-unknown", "dashboard-filter-campaign", "dashboard-filter-country",
    "dashboard-filter-energy", "dashboard-filter-state", "dashboard-filter-portal",
    "dashboard-filter-search", "dashboard-page", "dashboard-page-size",
    "dashboard-page-prev", "dashboard-page-next", "dashboard-queue",
    "dashboard-action", "dashboard-target-type", "dashboard-target-id",
    "dashboard-reason", "dashboard-apply", "dashboard-action-result"
  )
  for (control in controls) {
    testthat::expect_match(standalone, paste0("standalone-", control), fixed = TRUE)
    testthat::expect_match(embedded, paste0("acquisition-", control), fixed = TRUE)
  }
  testthat::expect_match(embedded, "Acquisition", fixed = TRUE)
  testthat::expect_false(grepl('id="dashboard-actor"', embedded, fixed = TRUE))
})

test_that("supplied acquisition catalogs remain caller-owned", {
  testthat::skip_if_not_installed("shiny")
  catalog <- structure(list(), class = "planscanR_acquisition_catalog")
  service <- planscanR.screen:::curation_app_acquisition_service(catalog)

  testthat::expect_true(isTRUE(service$available))
  testthat::expect_identical(service$catalog, catalog)
  testthat::expect_false("close" %in% names(service))
})

test_that("module requests unwrap the Shiny session proxy for cookie auth", {
  testthat::skip_if_not_installed("shiny")
  token <- paste(rep("a", 64L), collapse = "")
  root <- new.env(parent = emptyenv())
  root$request <- list(HEADERS = c(HTTP_COOKIE = paste0("biogain_session=", token)))
  proxy <- shiny:::createSessionProxy(root)

  root_request <- acquisition_dashboard_request(NULL, root)
  proxy_request <- acquisition_dashboard_request(NULL, proxy)
  header <- function(value) planscanR.screen:::curation_auth_request_header(value, "HTTP_COOKIE")

  testthat::expect_identical(header(root_request), paste0("biogain_session=", token))
  testthat::expect_identical(header(proxy_request), paste0("biogain_session=", token))
})
