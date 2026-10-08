# ACQ-UX6: each new campaign can include an explicit country scope.

testthat::test_that("country scope defaults to all available countries and keeps an empty scope explicit", {
  selection <- data.frame(
    country = c("NL", "dk", "de", "fr", NA_character_),
    selected = TRUE,
    stringsAsFactors = FALSE
  )

  available <- acquisition_campaign_scope_countries(selection)

  testthat::expect_identical(available, c("de", "dk", "fr", "nl"))
  testthat::expect_identical(
    acquisition_campaign_scope_input(NULL, available),
    available
  )
  testthat::expect_identical(
    acquisition_campaign_scope_input(character(0), available),
    character(0)
  )
  testthat::expect_identical(
    acquisition_campaign_scope_input(c("NL", "missing", "nl"), available),
    "nl"
  )

  thresholded <- selection
  thresholded$selected <- c(TRUE, TRUE, TRUE, TRUE, TRUE)
  scoped <- acquisition_campaign_apply_country_scope(thresholded, c("nl", "dk", "de"))
  testthat::expect_identical(scoped$selected, c(TRUE, TRUE, TRUE, FALSE, FALSE))
  testthat::expect_identical(scoped$manual_override, c(NA_character_, NA_character_, NA_character_, "exclude", "exclude"))
  probability_only <- data.frame(
    country = c("nl", "fr"),
    probability = c(0.4, 0.9),
    stringsAsFactors = FALSE
  )
  probability_scope <- acquisition_campaign_apply_country_scope(probability_only, c("nl", "fr"))
  testthat::expect_false("selected" %in% names(probability_scope))
  alias_override <- data.frame(
    country = c("nl", "fr"),
    selected = TRUE,
    selection_override = c("keep", "drop"),
    stringsAsFactors = FALSE
  )
  alias_scope <- acquisition_campaign_apply_country_scope(alias_override, "nl")
  testthat::expect_false("manual_override" %in% names(alias_scope))
  testthat::expect_identical(alias_scope$selection_override, c("keep", "exclude"))
  testthat::expect_identical(
    acquisition_campaign_country_scope_metadata(available, c("nl", "dk", "de")),
    list(
      mode = "include",
      available_countries = c("de", "dk", "fr", "nl"),
      included_countries = c("de", "dk", "nl")
    )
  )
})

testthat::test_that("country scope is applied after thresholding and preview remains read-only", {
  for (package in c("DBI", "RSQLite", "openssl")) {
    testthat::skip_if_not_installed(package)
  }

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  artifact <- file.path(root, "model.rds")
  saveRDS(list(model = "ux6-country-scope"), artifact)
  model <- acquisition_catalog_register_model(
    catalog, "model-ux6-country-scope", artifact, "policy-ux6", "snapshot-ux6"
  )
  sources <- lapply(c("nl", "dk", "de", "fr"), function(country) {
    acquisition_catalog_register_source(
      catalog, country, "portal-ux6", paste0("project-", country),
      source_url = paste0("https://portal.invalid/", country)
    )
  })
  selection <- data.frame(
    source_ref_id = vapply(sources, `[[`, character(1), "source_ref_id"),
    country = c("nl", "dk", "de", "fr"),
    probability = c(0.99, 0.98, 0.97, 0.99),
    selected = c(TRUE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
  thresholded_selection <- selection
  thresholded_selection$selected <- thresholded_selection$probability >= 0.95
  scoped_selection <- acquisition_campaign_apply_country_scope(thresholded_selection, c("nl", "dk", "de"))
  probability_selection <- selection[, c("source_ref_id", "country", "probability"), drop = FALSE]
  probability_selection$probability <- c(0.4, 0.98, 0.97, 0.99)
  probability_scope <- acquisition_campaign_apply_country_scope(probability_selection, c("nl", "dk", "de"))
  probability_preview <- acquisition_campaign_preview(
    catalog,
    probability_scope,
    model$model_id,
    "snapshot-ux6",
    threshold = 0.95,
    policy_version = "policy-ux6",
    priority_rules = list(list(rule_id = "fallback", priority = 1L))
  )
  testthat::expect_identical(probability_preview$items$selected, c(FALSE, TRUE, TRUE, FALSE))
  before_jobs <- acquisition_queue_jobs(catalog)
  preview <- acquisition_campaign_preview(
    catalog,
    scoped_selection,
    model$model_id,
    "snapshot-ux6",
    threshold = 0.95,
    policy_version = "policy-ux6",
    priority_rules = list(list(rule_id = "fallback", priority = 1L))
  )
  after_jobs <- acquisition_queue_jobs(catalog)

  testthat::expect_identical(preview$counts$selected_projects, 3L)
  testthat::expect_identical(
    preview$items$selected,
    c(TRUE, TRUE, TRUE, FALSE)
  )
  testthat::expect_true(is.na(preview$items$exclusion_reason[1L]))
  testthat::expect_identical(preview$items$exclusion_reason[4L], "manual exclusion")
  testthat::expect_identical(preview$counts$jobs, 0L)
  testthat::expect_equal(nrow(before_jobs), nrow(after_jobs))
})

testthat::test_that("pilot and backfill scopes preserve campaigns and reuse queue work", {
  for (package in c("DBI", "RSQLite", "openssl")) {
    testthat::skip_if_not_installed(package)
  }

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  artifact <- file.path(root, "model.rds")
  saveRDS(list(model = "ux6-country-scope"), artifact)
  model <- acquisition_catalog_register_model(
    catalog, "model-ux6-country-scope", artifact, "policy-ux6", "snapshot-ux6"
  )
  countries <- c("nl", "dk", "de", "fr")
  sources <- lapply(countries, function(country) {
    acquisition_catalog_register_source(
      catalog, country, "portal-ux6", paste0("project-", country)
    )
  })
  selection <- data.frame(
    source_ref_id = vapply(sources, `[[`, character(1), "source_ref_id"),
    country = countries,
    probability = rep(0.9, length(countries)),
    selected = TRUE,
    stringsAsFactors = FALSE
  )
  rules <- list(list(rule_id = "fallback", priority = 1L))
  pilot_scope <- c("nl", "dk", "de")
  pilot_selection <- acquisition_campaign_apply_country_scope(selection, pilot_scope)
  pilot <- acquisition_campaign_submit(
    catalog,
    pilot_selection,
    model$model_id,
    "snapshot-ux6",
    0.5,
    "policy-ux6",
    rules,
    submitted_by = "admin-ux6",
    campaign_id = "campaign-ux6-pilot",
    metadata = list(country_scope = acquisition_campaign_country_scope_metadata(countries, pilot_scope))
  )
  pilot_items_before <- acquisition_campaign_items(catalog, pilot$campaign_id)

  backfill <- acquisition_campaign_submit(
    catalog,
    selection,
    model$model_id,
    "snapshot-ux6",
    0.5,
    "policy-ux6",
    rules,
    submitted_by = "admin-ux6",
    campaign_id = "campaign-ux6-backfill",
    metadata = list(country_scope = acquisition_campaign_country_scope_metadata(countries, countries))
  )
  pilot_items_after <- acquisition_campaign_items(catalog, pilot$campaign_id)
  backfill_items <- acquisition_campaign_items(catalog, backfill$campaign_id)
  campaigns <- acquisition_campaigns(catalog)
  jobs <- acquisition_queue_jobs(catalog)
  pilot_metadata <- acquisition_campaign_unblob(
    campaigns$metadata_blob[campaigns$campaign_id == pilot$campaign_id]
  )

  testthat::expect_identical(pilot$status, "inserted")
  testthat::expect_identical(backfill$status, "inserted")
  testthat::expect_equal(nrow(campaigns), 2L)
  testthat::expect_equal(nrow(jobs), 4L)
  testthat::expect_identical(
    pilot_items_before[, c("source_ref_id", "selected", "work_status")],
    pilot_items_after[, c("source_ref_id", "selected", "work_status")]
  )
  pilot_source_ids <- vapply(sources[seq_along(pilot_scope)], `[[`, character(1), "source_ref_id")
  testthat::expect_true(all(pilot_items_after$selected[pilot_items_after$source_ref_id %in% pilot_source_ids] == 1L))
  fr_source <- sources[[which(countries == "fr")]]$source_ref_id
  testthat::expect_identical(pilot_items_after$selected[pilot_items_after$source_ref_id == fr_source], 0L)
  testthat::expect_true(all(backfill_items$selected == 1L))
  testthat::expect_equal(sum(backfill_items$work_status == "reused"), 3L)
  testthat::expect_equal(sum(backfill_items$work_status == "enqueued"), 1L)
  testthat::expect_identical(pilot_metadata$country_scope$included_countries, sort(pilot_scope))
})

testthat::test_that("country scope controls are visible in the campaign panel", {
  testthat::skip_if_not_installed("shiny")

  html <- htmltools::renderTags(acquisition_campaign_panel_ui("ux6"))$html

  testthat::expect_match(html, "campaign-country-scope", fixed = TRUE)
  testthat::expect_match(html, "Countries included in this campaign", fixed = TRUE)
  testthat::expect_match(html, "Unticked countries are excluded", fixed = TRUE)
})

testthat::test_that("panel snapshots an explicitly empty source-only country scope", {
  for (package in c("DBI", "RSQLite", "openssl", "shiny")) {
    testthat::skip_if_not_installed(package)
  }

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  artifact <- file.path(root, "model.rds")
  saveRDS(list(model = "ux6-country-scope"), artifact)
  model <- acquisition_catalog_register_model(
    catalog, "model-ux6-panel", artifact, "policy-ux6", "snapshot-ux6"
  )
  sources <- lapply(c("nl", "dk", "de"), function(country) {
    acquisition_catalog_register_source(catalog, country, "portal-ux6", paste0("project-", country))
  })
  captured <- NULL
  submit_fn <- function(...) {
    captured <<- list(...)
    list(campaign_id = "campaign-ux6-panel", status = "inserted", items = list())
  }
  preview <- list(
    selection = data.frame(
      source_ref_id = vapply(sources, `[[`, character(1), "source_ref_id"),
      probability = 0.9,
      selected = TRUE,
      stringsAsFactors = FALSE
    ),
    model_id = model$model_id,
    input_snapshot_id = "snapshot-ux6",
    threshold = 0.5,
    policy_version = "policy-ux6",
    priority_rules = list(list(rule_id = "fallback", priority = 1L))
  )
  admin <- curation_actor_resolver(function(request) curation_actor("admin-ux6", roles = "admin"))

  shiny::testServer(
    acquisition_campaign_panel_server,
    args = list(
      id = "campaign",
      catalog = catalog,
      preview = preview,
      actor_resolver = admin,
      submit_fn = submit_fn
    ),
    {
      session$setInputs(`campaign-threshold` = 0.5)
      session$flushReact()
      session$setInputs(`campaign-countries` = character(0))
      session$flushReact()
      session$setInputs(`campaign-submit` = 1L)
      session$flushReact()
    }
  )

  testthat::expect_true(is.list(captured))
  testthat::expect_true(all(!captured$selection$selected))
  testthat::expect_identical(
    captured$metadata$country_scope$included_countries,
    character(0)
  )
  testthat::expect_identical(
    captured$metadata$country_scope$available_countries,
    c("de", "dk", "nl")
  )
})
