# ACQ-UX7 disposable end-to-end fixture.
#
# This launcher combines the already-tested 128k curation fixture with the
# real acquisition preview, storage-settings, queue and control modules. It
# creates a temporary SQLite catalog and temporary roots only. It never reads
# the BIOGAIN cache, credentials, or service state and never starts a worker.
# The companion Playwright script records timings and browser payload evidence.

args <- commandArgs(trailingOnly = TRUE)
startup_started <- Sys.time()
package_root <- normalizePath(
  Sys.getenv("ACQ_UX7_PACKAGE_ROOT", if (length(args) > 0L) args[[1L]] else "."),
  mustWork = TRUE
)
port <- suppressWarnings(as.integer(
  Sys.getenv("ACQ_UX7_PORT", if (length(args) > 1L) args[[2L]] else "43867")
))
if (length(port) != 1L || is.na(port) || port < 1024L || port > 65535L) {
  stop("ACQ_UX7_PORT must be a valid local TCP port.", call. = FALSE)
}

if (identical(Sys.getenv("ACQ_UX7_INSTALLED"), "true")) {
  library(planscanR.screen)
} else {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("pkgload is required for the source-checkout fixture.", call. = FALSE)
  }
  pkgload::load_all(package_root, quiet = TRUE)
}

source(file.path(package_root, "inst", "curation", "acq_ux1_characterization.R"), local = FALSE)
ns <- asNamespace("planscanR.screen")
f <- function(name) get(name, envir = ns, inherits = FALSE)

root <- tempfile("acq-ux7-fixture-")
downloads <- file.path(root, "downloads")
cache <- file.path(root, "source-cache")
state <- file.path(root, "review-state")
dir.create(downloads, recursive = TRUE, showWarnings = FALSE)
dir.create(cache, recursive = TRUE, showWarnings = FALSE)
dir.create(state, recursive = TRUE, showWarnings = FALSE)

catalog_path <- file.path(root, "acquisition.sqlite")
catalog <- f("acquisition_catalog_open")(catalog_path)
on.exit(f("acquisition_catalog_close")(catalog), add = TRUE)

admin <- f("curation_actor")("fixture-admin", roles = "admin", display_name = "Fixture administrator")
f("acquisition_storage_register_root")(catalog, "downloads", downloads, admin)
f("acquisition_storage_set")(
  catalog,
  "default_estimate_bytes",
  max(f("acquisition_storage_transfer_cap_bytes")() * 2, 1024^2),
  admin
)

artifact <- file.path(root, "model.rds")
saveRDS(list(model = "acq-ux7-disposable-model"), artifact)
model <- f("acquisition_catalog_register_model")(
  catalog,
  "model-acq-ux7-fixture",
  artifact,
  "policy-acq-ux7.v1",
  "snapshot-acq-ux7.v1"
)

countries <- c("nl", "dk", "de", "be")
sources <- lapply(countries, function(country) {
  f("acquisition_catalog_register_source")(
    catalog,
    country,
    paste0("portal-", country),
    paste0("project-acq-ux7-", country),
    source_url = paste0("https://fixture.invalid/acq-ux7/", country)
  )
})
selection <- data.frame(
  source_ref_id = vapply(sources, `[[`, character(1), "source_ref_id"),
  country = countries,
  portal = paste0("portal-", countries),
  native_project_id = paste0("project-acq-ux7-", countries),
  title = paste("Fixture", toupper(countries), "project"),
  probability = c(0.96, 0.91, 0.87, 0.83),
  selected = TRUE,
  energy_kind = c("wind", "solar", "wind", "solar"),
  stringsAsFactors = FALSE
)
fixture_actor <- match.arg(
  Sys.getenv("ACQ_UX7_ACTOR", unset = "admin"),
  c("admin", "reviewer")
)
fixture_counts <- new.env(parent = emptyenv())
fixture_counts$actor_resolutions <- 0L
fixture_counts$state_reads <- 0L
fixture_counts$metrics_reads <- 0L
fixture_counts$submission_calls <- 0L
fixture_process_metric <- function(name) {
  lines <- tryCatch(readLines(sprintf("/proc/%s/status", Sys.getpid())), error = function(error) character())
  hit <- lines[startsWith(lines, paste0(name, ":"))]
  if (length(hit) == 0L) {
    return(NA_real_)
  }
  value <- suppressWarnings(as.numeric(sub("^\\s*[^:]+:\\s*([0-9.]+).*", "\\1", hit[[1L]])))
  if (!is.finite(value)) {
    return(NA_real_)
  }
  value * 1024
}
fixture_metrics <- function() {
  list(
    pid = Sys.getpid(),
    server_rss_bytes = fixture_process_metric("VmRSS"),
    server_shared_bytes = fixture_process_metric("RssShmem"),
    snapshot_bytes = as.numeric(object.size(context$app_state$snapshot$records)),
    calls = list(
      actor_resolutions = fixture_counts$actor_resolutions,
      state_reads = fixture_counts$state_reads,
      metrics_reads = fixture_counts$metrics_reads,
      submission_calls = fixture_counts$submission_calls
    )
  )
}
seed_reviewer_target <- identical(
  tolower(Sys.getenv("ACQ_UX7_SEED_TARGET", unset = "false")),
  "true"
)
if (seed_reviewer_target) {
  # The reviewer-only server gets one disposable queued target so its control
  # denial can be exercised without changing the administrator measurement
  # fixture or touching a production catalog.
  f("acquisition_campaign_submit")(
    catalog = catalog,
    selection = selection,
    model_id = model$model_id,
    input_snapshot_id = "snapshot-acq-ux7.v1",
    threshold = 0.5,
    policy_version = "policy-acq-ux7.v1",
    priority_rules = list(list(rule_id = "fixture-fallback", priority = 1L)),
    submitted_by = "fixture-admin",
    campaign_id = "campaign-acq-ux7-reviewer-seed"
  )
}
preview_args <- list(
  selection = selection,
  model_id = model$model_id,
  input_snapshot_id = "snapshot-acq-ux7.v1",
  threshold = 0.5,
  policy_version = "policy-acq-ux7.v1",
  priority_rules = list(list(rule_id = "fixture-fallback", priority = 1L))
)

# The browser submits five fresh deterministic campaigns while measuring
# acknowledgement. Each remains disposable and the bounded state view proves
# that earlier campaigns were not overwritten.
submission_count <- 0L
submit_fn <- function(...) {
  submission_count <<- submission_count + 1L
  fixture_counts$submission_calls <- submission_count
  call <- list(...)
  call$campaign_id <- sprintf("campaign-acq-ux7-fixture-%02d", submission_count)
  do.call(f("acquisition_campaign_submit_authenticated"), call)
}

storage_context <- list(
  cache_root = cache,
  state_root = state,
  catalog_path = catalog_path,
  config_id = "acq-ux7-fixture.v1"
)
acquisition <- list(
  catalog = catalog,
  available = TRUE,
  preview = preview_args,
  campaign_submit_fn = submit_fn,
  storage_context = storage_context
)

config <- acq_ux1_fixture_config(root = file.path(root, "curation-artifacts"))
records <- acq_ux1_fixture_records(128000L)
context <- acq_ux1_app_context(records, config)
fixture_review <- data.frame(
  country = "nl",
  document_id = "native-000001",
  reviewer_id = "fixture-reviewer",
  decision = "relevant",
  source = "fixture",
  note = "disposable acceptance row",
  reviewed_at = as.POSIXct(0, origin = "1970-01-01", tz = "UTC"),
  sidecar_path = NA_character_,
  revision = 1L,
  deleted = FALSE,
  stringsAsFactors = FALSE
)
context$review_repository <- f("curation_review_repository")(
  read_fn = function(scope, key, actor) fixture_review,
  list_fn = function(scope, filter, actor, limit, offset) fixture_review,
  upsert_fn = function(scope, decision, actor, expected_revision) {
    list(ok = TRUE, revision = as.integer(expected_revision) + 1L)
  },
  clear_fn = function(scope, key, actor, expected_revision) {
    list(ok = TRUE, revision = as.integer(expected_revision) + 1L)
  },
  bulk_upsert_fn = function(scope, decisions, actor, expected_revision) {
    list(ok = TRUE, revision = as.integer(expected_revision) + 1L)
  },
  import_fn = function(scope, rows, actor, expected_revision) {
    list(ok = TRUE, revision = as.integer(expected_revision) + 1L)
  },
  export_fn = function(scope, actor, format) fixture_review
)
context$historical_reviews <- data.frame(
  import_row_id = "history-acq-ux7",
  country = "nl",
  document_id = "history-acq-ux7",
  original_source = "fixture-archive",
  imported_source = "fixture-archive",
  historical_reviewer = "fixture-archive-reviewer",
  historical_linkage = "legacy_exact",
  current_match_count = 0L,
  current_linkage = "historical_only",
  linkage_status = "historical_only",
  mutation_allowed = FALSE,
  stringsAsFactors = FALSE
)
context$actor_resolver <- f("curation_actor_resolver")(function(request) {
  fixture_counts$actor_resolutions <- fixture_counts$actor_resolutions + 1L
  if (identical(fixture_actor, "reviewer")) {
    return(f("curation_actor")("fixture-reviewer", roles = "reviewer", display_name = "Fixture reviewer"))
  }
  f("curation_actor")("fixture-admin", roles = "admin", display_name = "Fixture administrator")
})
context$actor_probe <- context$actor_resolver
context$acquisition <- acquisition

ui <- shiny::tagList(
  shiny::tags$div(
    id = "acq-ux7-fixture-banner",
    shiny::tags$strong("ACQ-UX7 disposable acceptance fixture"),
    shiny::helpText(
      "Loopback-only synthetic corpus and temporary SQLite catalog; no production state, credentials, workers, downloads, or live cache are used."
    ),
    shiny::selectInput(
      "fixture-actor",
      "Fixture actor (fixed at launch)",
      choices = c("Administrator" = "admin", "Reviewer" = "reviewer"),
      selected = fixture_actor,
      selectize = FALSE
    ),
    shiny::textOutput("fixture-ready"),
    shiny::verbatimTextOutput("fixture-state"),
    shiny::verbatimTextOutput("fixture-metrics")
  ),
  f("curation_app_ui")(config, acquisition = acquisition)
)

server <- function(input, output, session) {
  output$`fixture-ready` <- shiny::renderText({
    # Keep this reactive so readiness is emitted in the same session as the
    # fixed actor input; changing the input cannot change authenticated role.
    input$`fixture-actor`
    paste("ready", nrow(context$app_state$snapshot$records), "synthetic records; actor", fixture_actor)
  })
  output$`fixture-state` <- shiny::renderText({
    # These inputs make the state view refresh after each isolated action while
    # keeping it independent of a worker or periodic production poll.
    input$`fixture-actor`
    input$`acquisition-campaign-campaign-submit`
    input$`acquisition-storage-save`
    input$`acquisition-dashboard-apply`
    fixture_counts$state_reads <- fixture_counts$state_reads + 1L
    jobs <- f("acquisition_queue_jobs")(catalog)
    campaigns <- f("acquisition_campaigns")(catalog)
    settings <- f("acquisition_storage_settings")(catalog, "default_estimate_bytes")
    value <- if (nrow(settings) == 0L) "none" else as.character(settings$value[[nrow(settings)]])
    paste(
      "fixture campaigns", nrow(campaigns),
      "jobs", nrow(jobs),
      "states", paste(sort(unique(as.character(jobs$state))), collapse = ","),
      "campaign_ids", paste(sort(as.character(campaigns$campaign_id)), collapse = ","),
      "estimate", value
    )
  })
  output$`fixture-metrics` <- shiny::renderText({
    input$`fixture-actor`
    input$`acquisition-campaign-campaign-submit`
    input$`acquisition-storage-save`
    input$`acquisition-dashboard-apply`
    fixture_counts$metrics_reads <- fixture_counts$metrics_reads + 1L
    jsonlite::toJSON(fixture_metrics(), auto_unbox = TRUE)
  })
  f("curation_app_server")(input, output, session, context)
}

startup_marker <- Sys.getenv("ACQ_UX7_STARTUP_MARKER", unset = "")
if (nzchar(startup_marker)) {
  dir.create(dirname(startup_marker), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    list(
      schema = "planscanR.acq-ux7-fixture-startup.v1",
      started_utc = format(startup_started, tz = "UTC", usetz = TRUE),
      prepared_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      preparation_seconds = as.numeric(difftime(Sys.time(), startup_started, units = "secs")),
      synthetic_rows = nrow(records),
      loopback_only = TRUE
    ),
    startup_marker,
    auto_unbox = TRUE,
    pretty = TRUE
  )
}

shiny::runApp(
  shiny::shinyApp(ui = ui, server = server),
  host = "127.0.0.1",
  port = port,
  launch.browser = FALSE
)
