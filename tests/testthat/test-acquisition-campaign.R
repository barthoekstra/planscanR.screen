testthat::test_that("campaign submissions are immutable and reuse durable work", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("openssl")

  root <- withr::local_tempdir()
  artifact <- file.path(root, "model.rds")
  saveRDS(list(model = "fixture"), artifact)
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  sources <- lapply(
    list(
      list(country = "nl", portal = "portal-a", native_project_id = "wind-1"),
      list(country = "de", portal = "portal-a", native_project_id = "mixed-1"),
      list(country = "nl", portal = "portal-a", native_project_id = "unknown-1")
    ),
    function(x) {
      acquisition_catalog_register_source(
        catalog,
        country = x$country,
        portal = x$portal,
        native_project_id = x$native_project_id,
        source_url = paste0("https://", x$portal, "/", x$native_project_id)
      )
    }
  )
  model <- acquisition_catalog_register_model(
    catalog,
    model_id = "model-campaign-1",
    artifact_path = artifact,
    config_id = "policy.v1",
    input_snapshot_id = "snapshot-1"
  )
  selection <- data.frame(
    source_ref_id = vapply(sources, `[[`, character(1), "source_ref_id"),
    selected = TRUE,
    probability = c(0.91, 0.82, 0.31),
    energy_tags = I(list("wind", c("wind", "solar"), character())),
    energy_evidence = I(list("title", "title and summary", NA_character_)),
    energy_confidence = c(0.9, 0.7, NA_real_),
    manual_energy_tag = c(NA_character_, NA_character_, NA_character_),
    stringsAsFactors = FALSE
  )
  rules <- list(
    list(rule_id = "nl-wind", countries = "nl", energy = "wind", priority = 90, explanation = "NL wind first"),
    list(rule_id = "solar-any", countries = "*", energy = "solar", priority = 70, explanation = "Solar next"),
    list(rule_id = "unknown-any", countries = "*", energy = "unknown", priority = 10, explanation = "Unknown last")
  )

  first <- acquisition_campaign_submit(
    catalog,
    selection = selection,
    model_id = model$model_id,
    input_snapshot_id = "snapshot-1",
    threshold = 0.5,
    policy_version = "policy.v1",
    priority_rules = rules,
    submitted_by = "reviewer-a"
  )
  replay <- acquisition_campaign_submit(
    catalog,
    selection = selection,
    model_id = model$model_id,
    input_snapshot_id = "snapshot-1",
    threshold = 0.5,
    policy_version = "policy.v1",
    priority_rules = rules,
    submitted_by = "reviewer-a"
  )
  equivalent <- acquisition_campaign_submit(
    catalog,
    selection = selection,
    model_id = model$model_id,
    input_snapshot_id = "snapshot-1",
    threshold = 0.5,
    policy_version = "policy.v1",
    priority_rules = rules,
    submitted_by = "reviewer-b",
    campaign_id = "campaign-equivalent"
  )

  testthat::expect_identical(first$status, "inserted")
  testthat::expect_identical(replay$status, "replayed")
  testthat::expect_identical(equivalent$status, "inserted")
  testthat::expect_true(all(equivalent$items[[1L]]$work_status == "reused"))
  testthat::expect_equal(nrow(acquisition_campaigns(catalog)), 2L)
  testthat::expect_equal(nrow(acquisition_campaign_items(catalog, first$campaign_id)), 3L)
  testthat::expect_equal(nrow(acquisition_queue_jobs(catalog)), 3L)
  testthat::expect_true(all(replay$items$work_status[replay$items$selected == 1L] == "reused"))
  items <- acquisition_campaign_items(catalog, first$campaign_id)
  testthat::expect_identical(items$energy_kind[items$source_ref_id == sources[[2L]]$source_ref_id], "mixed")
  testthat::expect_identical(items$energy_kind[items$source_ref_id == sources[[3L]]$source_ref_id], "unknown")
  testthat::expect_true(all(nzchar(items$priority_explanation)))
  testthat::expect_error(
    acquisition_campaign_submit(
      catalog,
      selection = transform(selection, probability = 0.01),
      model_id = model$model_id,
      input_snapshot_id = "snapshot-1",
      threshold = 0.5,
      policy_version = "policy.v1",
      priority_rules = rules,
      submitted_by = "reviewer-a",
      campaign_id = first$campaign_id
    ),
    class = "planscanR_error_acquisition_immutable"
  )
  catalog$close()
})

testthat::test_that("reprioritization records a revision and leaves active work intact", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("openssl")

  root <- withr::local_tempdir()
  artifact <- file.path(root, "model.rds")
  saveRDS(list(model = "fixture"), artifact)
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  sources <- lapply(c("wind-project", "solar-project"), function(id) {
    acquisition_catalog_register_source(
      catalog,
      country = "nl",
      portal = "portal.example",
      native_project_id = id,
      source_url = paste0("https://portal.example/", id)
    )
  })
  model <- acquisition_catalog_register_model(
    catalog,
    model_id = "model-campaign-2",
    artifact_path = artifact,
    config_id = "policy.v1",
    input_snapshot_id = "snapshot-2"
  )
  selection <- data.frame(
    source_ref_id = vapply(sources, `[[`, character(1), "source_ref_id"),
    selected = TRUE,
    probability = c(0.8, 0.8),
    energy_tags = I(list("wind", "solar")),
    stringsAsFactors = FALSE
  )
  first <- acquisition_campaign_submit(
    catalog,
    selection = selection,
    model_id = model$model_id,
    input_snapshot_id = "snapshot-2",
    threshold = 0.5,
    policy_version = "policy.v1",
    priority_rules = list(
      list(rule_id = "wind-first", countries = "*", energy = "wind", priority = 90),
      list(rule_id = "solar-second", countries = "*", energy = "solar", priority = 10)
    ),
    submitted_by = "reviewer-a"
  )
  running <- acquisition_queue_claim(catalog, "worker-a", lease_seconds = 60L)
  before <- acquisition_queue_jobs(catalog)
  changed <- acquisition_campaign_reprioritize(
    catalog,
    campaign_id = first$campaign_id,
    priority_rules = list(
      list(rule_id = "solar-first", countries = "*", energy = "solar", priority = 100),
      list(rule_id = "wind-second", countries = "*", energy = "wind", priority = 20)
    ),
    changed_by = "admin"
  )
  after <- acquisition_queue_jobs(catalog)

  testthat::expect_identical(changed$status, "updated")
  testthat::expect_equal(changed$revision, 1L)
  testthat::expect_identical(
    after$priority[after$job_id == running$job_id],
    before$priority[before$job_id == running$job_id]
  )
  pending <- after[after$state == "queued", , drop = FALSE]
  testthat::expect_equal(nrow(pending), 1L)
  testthat::expect_identical(pending$priority, 100L)
  testthat::expect_identical(pending$priority_rule_id, "solar-first")
  explanation <- acquisition_campaign_explain(catalog, first$campaign_id)
  testthat::expect_true(any(explanation$queue_state == "running"))
  testthat::expect_true(any(explanation$queue_state == "queued"))
  testthat::expect_equal(
    length(unique(acquisition_campaign_priority_history(catalog, first$campaign_id)$revision)),
    2L
  )
  catalog$close()
})

testthat::test_that("discovered downloads inherit the campaign priority evidence", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("openssl")

  root <- withr::local_tempdir()
  artifact <- file.path(root, "model.rds")
  saveRDS(list(model = "fixture"), artifact)
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  source <- acquisition_catalog_register_source(
    catalog,
    country = "se",
    portal = "portal.example",
    native_project_id = "wind-project",
    source_url = "https://portal.example/wind-project"
  )
  model <- acquisition_catalog_register_model(
    catalog,
    model_id = "model-campaign-3",
    artifact_path = artifact,
    config_id = "policy.v1",
    input_snapshot_id = "snapshot-3"
  )
  campaign <- acquisition_campaign_submit(
    catalog,
    selection = data.frame(
      source_ref_id = source$source_ref_id,
      selected = TRUE,
      energy_tags = I(list("wind")),
      stringsAsFactors = FALSE
    ),
    model_id = model$model_id,
    input_snapshot_id = "snapshot-3",
    threshold = 0.5,
    policy_version = "policy.v1",
    priority_rules = list(list(rule_id = "wind", countries = "se", energy = "wind", priority = 42)),
    submitted_by = "reviewer-a"
  )
  claim <- acquisition_queue_claim(catalog, "worker-a", lease_seconds = 60L)
  testthat::expect_identical(claim$job_id, campaign$items[[1L]]$job_id)
  done <- acquisition_queue_complete(
    catalog,
    claim$job_id,
    claim$lease_token,
    status = "succeeded",
    result = list(
      status = "succeeded",
      source_ref_id = source$source_ref_id,
      documents = list(list(
        native_attachment_id = "report-1",
        attachment_url = "https://cdn.example/report-1.pdf",
        ordinal = 1L
      ))
    )
  )
  testthat::expect_true(done$ok)
  jobs <- acquisition_queue_jobs(catalog)
  download <- jobs[jobs$kind == "download", , drop = FALSE]
  testthat::expect_equal(nrow(download), 1L)
  testthat::expect_identical(download$campaign_id, campaign$campaign_id)
  testthat::expect_identical(download$priority, 42L)
  testthat::expect_identical(download$priority_rule_id, "wind")
  catalog$close()
})

testthat::test_that("reprioritization reaches pending downloads and preserves active work", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("openssl")

  root <- withr::local_tempdir()
  artifact <- file.path(root, "model.rds")
  saveRDS(list(model = "fixture"), artifact)
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  sources <- lapply(seq_len(3L), function(i) {
    acquisition_catalog_register_source(
      catalog,
      country = "nl",
      portal = "portal.example",
      native_project_id = paste0("campaign-project-", i),
      source_url = paste0("https://portal.example/campaign-project-", i)
    )
  })
  model <- acquisition_catalog_register_model(
    catalog,
    model_id = "model-campaign-pending-download",
    artifact_path = artifact,
    config_id = "policy.v1",
    input_snapshot_id = "snapshot-pending-download"
  )
  campaign <- acquisition_campaign_submit(
    catalog,
    selection = data.frame(
      source_ref_id = vapply(sources, `[[`, character(1), "source_ref_id"),
      selected = TRUE,
      energy_tags = I(list("wind", "solar", character())),
      stringsAsFactors = FALSE
    ),
    model_id = model$model_id,
    input_snapshot_id = "snapshot-pending-download",
    threshold = 0.5,
    policy_version = "policy.v1",
    priority_rules = list(
      list(rule_id = "solar-first", energy = "solar", priority = 50),
      list(rule_id = "wind-next", energy = "wind", priority = 40),
      list(rule_id = "mixed-last", energy = "mixed", priority = 5)
    ),
    submitted_by = "reviewer-a",
    campaign_id = "campaign-pending-download"
  )
  job_for_source <- function(source_ref_id) {
    items <- campaign$items[vapply(campaign$items, function(item) {
      identical(item$source_ref_id, source_ref_id)
    }, logical(1))]
    items[[1L]]$job_id
  }

  first_discovery <- acquisition_queue_claim(catalog, "worker-a", lease_seconds = 60L)
  testthat::expect_identical(first_discovery$job_id, job_for_source(sources[[2L]]$source_ref_id))
  acquisition_queue_complete(
    catalog,
    first_discovery$job_id,
    first_discovery$lease_token,
    status = "succeeded",
    result = list(
      source_ref_id = sources[[2L]]$source_ref_id,
      documents = list(list(
        native_attachment_id = "pending-download-2",
        attachment_url = "https://cdn.example/pending-download-2.pdf",
        ordinal = 1L
      ))
    )
  )
  active_download <- acquisition_queue_claim(catalog, "worker-download", lease_seconds = 60L)
  testthat::expect_identical(active_download$kind, "download")

  second_discovery <- acquisition_queue_claim(catalog, "worker-b", lease_seconds = 60L)
  testthat::expect_identical(second_discovery$job_id, job_for_source(sources[[1L]]$source_ref_id))
  acquisition_queue_complete(
    catalog,
    second_discovery$job_id,
    second_discovery$lease_token,
    status = "succeeded",
    result = list(
      source_ref_id = sources[[1L]]$source_ref_id,
      documents = list(list(
        native_attachment_id = "pending-download-1",
        attachment_url = "https://cdn.example/pending-download-1.pdf",
        ordinal = 1L
      ))
    )
  )
  before <- acquisition_queue_jobs(catalog)
  pending_download <- before[before$kind == "download" & before$state == "queued", , drop = FALSE]
  testthat::expect_equal(nrow(pending_download), 1L)
  testthat::expect_identical(pending_download$priority, 40L)

  changed <- acquisition_campaign_reprioritize(
    catalog,
    campaign_id = campaign$campaign_id,
    priority_rules = list(
      list(rule_id = "solar-new", energy = "solar", priority = 90),
      list(rule_id = "wind-new", energy = "wind", priority = 100),
      list(rule_id = "unknown-new", energy = "unknown", priority = 80)
    ),
    changed_by = "admin"
  )
  after <- acquisition_queue_jobs(catalog)
  active_after <- after[after$job_id == active_download$job_id, , drop = FALSE]
  pending_after <- after[after$job_id == pending_download$job_id, , drop = FALSE]
  third_after <- after[after$target_ref_id == sources[[3L]]$source_ref_id, , drop = FALSE]
  testthat::expect_identical(changed$updated, 2L)
  testthat::expect_identical(active_after$state, "running")
  testthat::expect_identical(active_after$priority, 50L)
  testthat::expect_identical(pending_after$priority, 100L)
  testthat::expect_identical(pending_after$priority_rule_id, "wind-new")
  testthat::expect_identical(third_after$priority, 80L)
  catalog$close()
})

testthat::test_that("reprioritized running discovery gives new downloads the latest revision", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("openssl")

  root <- withr::local_tempdir()
  artifact <- file.path(root, "model.rds")
  saveRDS(list(model = "fixture"), artifact)
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  source <- acquisition_catalog_register_source(
    catalog,
    country = "nl",
    portal = "portal.example",
    native_project_id = "running-discovery-project",
    source_url = "https://portal.example/running-discovery-project"
  )
  model <- acquisition_catalog_register_model(
    catalog,
    model_id = "model-campaign-running-discovery",
    artifact_path = artifact,
    config_id = "policy.v1",
    input_snapshot_id = "snapshot-running-discovery"
  )
  campaign <- acquisition_campaign_submit(
    catalog,
    selection = data.frame(
      source_ref_id = source$source_ref_id,
      selected = TRUE,
      energy_tags = I(list("wind")),
      stringsAsFactors = FALSE
    ),
    model_id = model$model_id,
    input_snapshot_id = "snapshot-running-discovery",
    threshold = 0.5,
    policy_version = "policy.v1",
    priority_rules = list(list(rule_id = "initial", energy = "wind", priority = 10)),
    campaign_id = "campaign-running-discovery"
  )
  running <- acquisition_queue_claim(catalog, "worker-a", lease_seconds = 60L)
  changed <- acquisition_campaign_reprioritize(
    catalog,
    campaign_id = campaign$campaign_id,
    priority_rules = list(list(rule_id = "latest", energy = "wind", priority = 80)),
    changed_by = "admin"
  )
  testthat::expect_identical(changed$active, 1L)
  done <- acquisition_queue_complete(
    catalog,
    running$job_id,
    running$lease_token,
    status = "succeeded",
    result = list(
      source_ref_id = source$source_ref_id,
      documents = list(list(
        native_attachment_id = "latest-download",
        attachment_url = "https://cdn.example/latest-download.pdf",
        ordinal = 1L
      ))
    )
  )
  testthat::expect_true(done$ok)
  jobs <- acquisition_queue_jobs(catalog)
  discovery <- jobs[jobs$job_id == running$job_id, , drop = FALSE]
  download <- jobs[jobs$kind == "download", , drop = FALSE]
  testthat::expect_identical(discovery$priority, 10L)
  testthat::expect_identical(download$priority, 80L)
  testthat::expect_identical(download$priority_rule_id, "latest")
  catalog$close()
})

testthat::test_that("campaign adoption applies pending priority and leaves active downloads untouched", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("openssl")

  root <- withr::local_tempdir()
  artifact <- file.path(root, "model.rds")
  saveRDS(list(model = "fixture"), artifact)
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  queued_source <- acquisition_catalog_register_source(
    catalog,
    country = "nl",
    portal = "portal.example",
    native_project_id = "adopted-discovery-project",
    source_url = "https://portal.example/adopted-discovery-project"
  )
  queued_discovery <- acquisition_queue_enqueue(
    catalog,
    kind = "discover",
    target_type = "source",
    target_ref_id = queued_source$source_ref_id,
    priority = 3L
  )
  active_source <- acquisition_catalog_register_source(
    catalog,
    country = "nl",
    portal = "portal.example",
    native_project_id = "adopted-download-project",
    source_url = "https://portal.example/adopted-download-project"
  )
  active_document <- acquisition_catalog_register_document(
    catalog,
    source_ref_id = active_source$source_ref_id,
    native_attachment_id = "adopted-download",
    attachment_url = "https://cdn.example/adopted-download.pdf"
  )
  active_download <- acquisition_queue_enqueue(
    catalog,
    kind = "download",
    target_type = "document",
    target_ref_id = active_document$document_ref_id,
    priority = 7L
  )
  active_claim <- acquisition_queue_claim(catalog, "download-worker", lease_seconds = 60L)
  testthat::expect_identical(active_claim$job_id, active_download$job_id)
  model <- acquisition_catalog_register_model(
    catalog,
    model_id = "model-campaign-adoption",
    artifact_path = artifact,
    config_id = "policy.v1",
    input_snapshot_id = "snapshot-adoption"
  )
  queued_campaign <- acquisition_campaign_submit(
    catalog,
    selection = data.frame(
      source_ref_id = queued_source$source_ref_id,
      selected = TRUE,
      energy_tags = I(list("wind")),
      stringsAsFactors = FALSE
    ),
    model_id = model$model_id,
    input_snapshot_id = "snapshot-adoption",
    threshold = 0.5,
    policy_version = "policy.v1",
    priority_rules = list(list(rule_id = "queued-rule", energy = "wind", priority = 77)),
    campaign_id = "campaign-adopted-discovery"
  )
  adopted_discovery <- acquisition_queue_jobs(catalog)
  adopted_discovery <- adopted_discovery[adopted_discovery$job_id == queued_discovery$job_id, , drop = FALSE]
  testthat::expect_identical(queued_campaign$items[[1L]]$job_id, queued_discovery$job_id)
  testthat::expect_identical(adopted_discovery$priority, 77L)
  testthat::expect_identical(adopted_discovery$campaign_id, queued_campaign$campaign_id)

  active_campaign <- acquisition_campaign_submit(
    catalog,
    selection = data.frame(
      source_ref_id = active_source$source_ref_id,
      selected = TRUE,
      energy_tags = I(list("wind")),
      stringsAsFactors = FALSE
    ),
    model_id = model$model_id,
    input_snapshot_id = "snapshot-adoption-active",
    threshold = 0.5,
    policy_version = "policy.v1",
    priority_rules = list(list(rule_id = "active-rule", energy = "wind", priority = 88)),
    campaign_id = "campaign-adopted-download"
  )
  discovery_claim <- acquisition_queue_claim(catalog, "discovery-worker", lease_seconds = 60L)
  testthat::expect_identical(discovery_claim$job_id, active_campaign$items[[1L]]$job_id)
  acquisition_queue_complete(
    catalog,
    discovery_claim$job_id,
    discovery_claim$lease_token,
    status = "succeeded",
    result = list(
      source_ref_id = active_source$source_ref_id,
      documents = list(list(
        native_attachment_id = "adopted-download",
        attachment_url = "https://cdn.example/adopted-download.pdf",
        ordinal = 1L
      ))
    )
  )
  jobs <- acquisition_queue_jobs(catalog)
  adopted_download <- jobs[jobs$job_id == active_download$job_id, , drop = FALSE]
  testthat::expect_identical(adopted_download$state, "running")
  testthat::expect_identical(adopted_download$priority, 7L)
  testthat::expect_true(is.na(adopted_download$campaign_id))
  catalog$close()
})
