# ACQ-UX5: acquisition preview -> queue -> monitor workflow.

testthat::test_that("acquisition workflow UI names preview, queue, monitor and cancellation confirmation", {
  testthat::skip_if_not_installed("shiny")

  preview_html <- htmltools::renderTags(acquisition_campaign_panel_ui("ux5"))$html
  dashboard_html <- htmltools::renderTags(acquisition_dashboard_ui("ux5"))$html

  testthat::expect_match(preview_html, "Preview is read-only", fixed = TRUE)
  testthat::expect_match(preview_html, "Queue selected acquisition", fixed = TRUE)
  testthat::expect_match(preview_html, "independent workers", fixed = TRUE)
  testthat::expect_match(dashboard_html, "Monitor queued acquisition", fixed = TRUE)
  testthat::expect_match(dashboard_html, "dashboard-cancel-confirm", fixed = TRUE)
  testthat::expect_match(dashboard_html, "Choose an explicit target", fixed = TRUE)
  testthat::expect_true(length(regmatches(dashboard_html, gregexpr("<details", dashboard_html, fixed = TRUE))[[1L]]) >= 2L)
})

testthat::test_that("control guidance states the actual bounded effect for every action", {
  for (action in c("pause", "resume", "retry", "cancel")) {
    guidance <- acquisition_dashboard_action_guidance(action, "campaign", "campaign-ux5")
    testthat::expect_match(guidance, "campaign-ux5", fixed = TRUE)
    testthat::expect_match(guidance, "Campaign", fixed = TRUE)
    testthat::expect_true(nchar(guidance) > 40L)
  }

  testthat::expect_match(
    acquisition_dashboard_action_guidance("cancel", "job", "job-ux5"),
    "running workers receive a safe-stop request",
    fixed = TRUE
  )
  testthat::expect_match(
    acquisition_dashboard_action_guidance("pause", "job", "job-ux5"),
    "queued and waiting-to-retry work",
    fixed = TRUE
  )
  testthat::expect_match(
    acquisition_dashboard_action_guidance("resume", "job", "job-ux5"),
    "paused work",
    fixed = TRUE
  )
  testthat::expect_match(
    acquisition_dashboard_action_guidance("retry", "job", "job-ux5"),
    "failed, blocked, or cancelled work",
    fixed = TRUE
  )
  testthat::expect_match(
    acquisition_dashboard_action_guidance("cancel", "campaign", "campaign-ux5"),
    "jobs owned by this campaign",
    fixed = TRUE
  )
  testthat::expect_match(
    acquisition_dashboard_action_guidance("cancel", "campaign", "campaign-ux5"),
    "shared or reused jobs owned by another campaign remain unchanged",
    fixed = TRUE
  )
  testthat::expect_match(
    acquisition_dashboard_control_result_text(list(
      action = "cancel", target_type = "campaign", target_id = "campaign-ux5",
      affected = 1L, running = 0L, audit_id = "audit-ux5"
    )),
    "shared or reused jobs owned by another campaign remain unchanged",
    fixed = TRUE
  )
})

testthat::test_that("submission acknowledgement distinguishes queued work from independent workers", {
  result <- list(
    campaign_id = "campaign-ux5",
    status = "inserted",
    submitted_by = "admin-ux5",
    items = list(
      list(job_id = "job-1", work_status = "enqueued"),
      list(job_id = "job-2", work_status = "reused"),
      list(job_id = NA_character_, work_status = "not_selected")
    )
  )
  acknowledgement <- acquisition_campaign_submission_acknowledgement(result)
  testthat::expect_match(acknowledgement, "campaign-ux5", fixed = TRUE)
  testthat::expect_match(acknowledgement, "2 queue jobs", fixed = TRUE)
  testthat::expect_match(acknowledgement, "1 new", fixed = TRUE)
  testthat::expect_match(acknowledgement, "1 reused", fixed = TRUE)
  testthat::expect_match(acknowledgement, "independent workers", fixed = TRUE)
  testthat::expect_match(acknowledgement, "does not run discovery", fixed = TRUE)

  replay <- acquisition_campaign_submission_acknowledgement(utils::modifyList(result, list(status = "replayed")))
  testthat::expect_match(replay, "unchanged", fixed = TRUE)
})

testthat::test_that("control target choices stay bounded and retain source context", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  sources <- lapply(seq_len(3L), function(index) {
    acquisition_catalog_register_source(
      catalog,
      "nl",
      "portal-ux5",
      paste0("project-ux5-", index),
      source_url = paste0("https://fixture.invalid/ux5-", index)
    )
  })
  jobs <- lapply(seq_len(3L), function(index) {
    acquisition_queue_enqueue(
      catalog,
      "discover",
      "source",
      sources[[index]]$source_ref_id,
      idempotency_key = paste0("ux5-target-", index)
    )
  })

  choices <- acquisition_dashboard_target_choices(catalog, "job", limit = 2L)
  testthat::expect_length(choices, 2L)
  testthat::expect_true(all(unname(choices) %in% vapply(jobs, `[[`, character(1), "job_id")))
  testthat::expect_true(all(grepl("portal-ux5|project-ux5", names(choices))))
  testthat::expect_false(any(names(choices) == unname(choices)))
})

testthat::test_that("preview remains read-only while attachment totals stay unknown", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("openssl")

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  artifact <- file.path(root, "model.rds")
  saveRDS(list(model = "ux5"), artifact)
  model <- acquisition_catalog_register_model(
    catalog, "model-ux5", artifact, "policy-ux5", "snapshot-ux5"
  )
  source <- acquisition_catalog_register_source(catalog, "nl", "portal-ux5", "project-ux5")
  selection <- data.frame(
    source_ref_id = source$source_ref_id,
    title = "UX5 title",
    probability = 0.9,
    selected = TRUE,
    stringsAsFactors = FALSE
  )
  before_jobs <- acquisition_queue_jobs(catalog)
  preview <- acquisition_campaign_preview(
    catalog,
    selection,
    model$model_id,
    "snapshot-ux5",
    threshold = 0.5,
    policy_version = "policy-ux5",
    priority_rules = list(list(rule_id = "fallback", priority = 1L))
  )
  after_jobs <- acquisition_queue_jobs(catalog)

  testthat::expect_identical(preview$counts$jobs, 0L)
  testthat::expect_true(isTRUE(preview$attachments$unknown))
  testthat::expect_true(is.na(preview$attachments$files))
  testthat::expect_identical(nrow(before_jobs), nrow(after_jobs))
})

testthat::test_that("dashboard requires an explicit target and cancel confirmation before writing", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  source <- acquisition_catalog_register_source(catalog, "nl", "portal-ux5", "project-ux5")
  job <- acquisition_queue_enqueue(catalog, "discover", "source", source$source_ref_id)
  admin <- curation_actor_resolver(function(request) curation_actor("admin-ux5", roles = "admin"))

  shiny::testServer(
    acquisition_dashboard_server,
    args = list(
      id = "dashboard", catalog = catalog, actor_resolver = admin, refresh_ms = 250L
    ),
    {
      session$flushReact()
      testthat::expect_match(
        as.character(output$`dashboard-action-guidance`),
        "Choose an explicit target",
        fixed = TRUE
      )

      session$setInputs(`dashboard-apply` = 1L)
      session$flushReact()
      testthat::expect_match(as.character(output$`dashboard-action-result`), "No action was sent", fixed = TRUE)
      testthat::expect_identical(acquisition_queue_jobs(catalog)$state, "queued")
      testthat::expect_equal(nrow(acquisition_control_audit(catalog, target_id = job$job_id)), 0L)

      session$setInputs(
        `dashboard-action` = "cancel",
        `dashboard-target-type` = "job",
        `dashboard-target-id` = job$job_id,
        `dashboard-cancel-confirm` = FALSE,
        `dashboard-apply` = 2L
      )
      session$flushReact()
      testthat::expect_match(as.character(output$`dashboard-action-result`), "Confirm the cancellation", fixed = TRUE)
      testthat::expect_identical(acquisition_queue_jobs(catalog)$state, "queued")
      testthat::expect_equal(nrow(acquisition_control_audit(catalog, target_id = job$job_id)), 0L)

      session$setInputs(`dashboard-cancel-confirm` = TRUE)
      session$flushReact()
      # A browser may briefly retain the checked value while a target change
      # is travelling to the server. The server-side tuple must still expire
      # the confirmation before accepting a different action.
      session$setInputs(`dashboard-action` = "pause")
      session$flushReact()
      session$setInputs(`dashboard-action` = "cancel")
      session$flushReact()
      session$setInputs(`dashboard-apply` = 3L)
      session$flushReact()
      testthat::expect_match(as.character(output$`dashboard-action-result`), "No action was sent", fixed = TRUE)
      testthat::expect_identical(acquisition_queue_jobs(catalog)$state, "queued")
      testthat::expect_equal(nrow(acquisition_control_audit(catalog, target_id = job$job_id)), 0L)

      session$setInputs(`dashboard-cancel-confirm` = FALSE)
      session$flushReact()
      session$setInputs(`dashboard-cancel-confirm` = TRUE)
      session$flushReact()
      session$setInputs(`dashboard-apply` = 4L)
      session$flushReact()
      testthat::expect_match(as.character(output$`dashboard-action-result`), job$job_id, fixed = TRUE)
      testthat::expect_match(as.character(output$`dashboard-action-result`), "safe-stop", fixed = TRUE)
      testthat::expect_identical(acquisition_queue_jobs(catalog)$state, "cancelled")
      testthat::expect_equal(nrow(acquisition_control_audit(catalog, target_id = job$job_id)), 1L)
    }
  )
})

testthat::test_that("forged admin request fields do not authorize reviewer controls", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  source <- acquisition_catalog_register_source(catalog, "nl", "portal-ux5", "project-ux5")
  job <- acquisition_queue_enqueue(catalog, "discover", "source", source$source_ref_id)
  reviewer <- curation_actor_resolver(function(request) curation_actor("reviewer-ux5", roles = "reviewer"))

  shiny::testServer(
    acquisition_dashboard_server,
    args = list(
      id = "dashboard", catalog = catalog, actor_resolver = reviewer,
      request = list(actor_id = "forged-admin", roles = "admin"), refresh_ms = 250L
    ),
    {
      session$flushReact()
      session$setInputs(
        `dashboard-action` = "pause",
        `dashboard-target-type` = "job",
        `dashboard-target-id` = job$job_id,
        `dashboard-apply` = 1L
      )
      session$flushReact()
      testthat::expect_match(as.character(output$`dashboard-action-result`), "admin role", fixed = TRUE)
      testthat::expect_identical(acquisition_queue_jobs(catalog)$state, "queued")
      testthat::expect_equal(nrow(acquisition_control_audit(catalog, target_id = job$job_id)), 0L)
    }
  )
})

testthat::test_that("an expired session cannot write dashboard controls", {
  testthat::skip_if_not_installed("shiny")
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  source <- acquisition_catalog_register_source(catalog, "nl", "portal-ux5", "project-ux5")
  job <- acquisition_queue_enqueue(catalog, "discover", "source", source$source_ref_id)
  calls <- 0L
  expiring_admin <- curation_actor_resolver(function(request) {
    calls <<- calls + 1L
    if (calls == 1L) {
      return(curation_actor("admin-ux5", roles = "admin"))
    }
    rlang::abort("The authenticated session has expired.", class = "planscanR_error_curation_authentication")
  })

  shiny::testServer(
    acquisition_dashboard_server,
    args = list(
      id = "dashboard", catalog = catalog, actor_resolver = expiring_admin, refresh_ms = 250L
    ),
    {
      session$flushReact()
      before_jobs <- acquisition_queue_jobs(catalog)
      before_audit <- acquisition_control_audit(catalog, target_id = job$job_id)
      session$setInputs(
        `dashboard-action` = "pause",
        `dashboard-target-type` = "job",
        `dashboard-target-id` = job$job_id,
        `dashboard-apply` = 1L
      )
      session$flushReact()
      testthat::expect_match(as.character(output$`dashboard-action-result`), "session has expired", fixed = TRUE)
      testthat::expect_identical(acquisition_queue_jobs(catalog)$state, before_jobs$state)
      testthat::expect_equal(
        nrow(acquisition_control_audit(catalog, target_id = job$job_id)),
        nrow(before_audit)
      )
    }
  )
})

testthat::test_that("an expired session cannot submit a campaign or create queue work", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  testthat::skip_if_not_installed("openssl")

  root <- withr::local_tempdir()
  catalog <- acquisition_catalog_open(file.path(root, "catalog.sqlite"))
  on.exit(acquisition_catalog_close(catalog), add = TRUE)
  artifact <- file.path(root, "model.rds")
  saveRDS(list(model = "ux5"), artifact)
  model <- acquisition_catalog_register_model(
    catalog, "model-ux5-expired", artifact, "policy-ux5", "snapshot-ux5"
  )
  source <- acquisition_catalog_register_source(catalog, "nl", "portal-ux5", "project-ux5")
  selection <- data.frame(
    source_ref_id = source$source_ref_id,
    country = "nl",
    portal = "portal-ux5",
    native_project_id = "project-ux5",
    probability = 0.9,
    selected = TRUE,
    stringsAsFactors = FALSE
  )
  expired <- curation_actor_resolver(function(request) {
    rlang::abort("The authenticated session has expired.", class = "planscanR_error_curation_authentication")
  })
  before_campaigns <- acquisition_campaigns(catalog)
  before_jobs <- acquisition_queue_jobs(catalog)

  testthat::expect_error(
    acquisition_campaign_submit_authenticated(
      catalog = catalog,
      selection = selection,
      model_id = model$model_id,
      input_snapshot_id = "snapshot-ux5",
      threshold = 0.5,
      policy_version = "policy-ux5",
      priority_rules = list(list(rule_id = "fallback", priority = 1L)),
      actor_resolver = expired,
      campaign_id = "campaign-ux5-expired"
    ),
    class = "planscanR_error_curation_authentication",
    regexp = "expired"
  )
  testthat::expect_equal(nrow(acquisition_campaigns(catalog)), nrow(before_campaigns))
  testthat::expect_equal(nrow(acquisition_queue_jobs(catalog)), nrow(before_jobs))
})
