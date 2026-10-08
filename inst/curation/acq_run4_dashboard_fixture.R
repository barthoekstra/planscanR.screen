# ACQ-RUN4 disposable dashboard/browser fixture.
#
# This process creates one temporary acquisition catalog, one queued download,
# and a synthetic verified file. It never reads production state, credentials,
# or a portal. The companion Playwright runner drives only the loopback app.

script_file <- tryCatch(sys.frame(1)$ofile, error = function(error) NULL)
if (is.null(script_file) || !nzchar(script_file)) {
  script_file <- "inst/curation/acq_run4_dashboard_fixture.R"
}
package_root <- normalizePath(file.path(dirname(script_file), "../.."), mustWork = FALSE)
if (file.exists(file.path(package_root, "DESCRIPTION"))) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Install pkgload to run the ACQ-RUN4 fixture.", call. = FALSE)
  }
  pkgload::load_all(package_root, quiet = TRUE)
} else {
  if (!requireNamespace("planscanR.screen", quietly = TRUE)) {
    stop("Install planscanR.screen to run the ACQ-RUN4 fixture.", call. = FALSE)
  }
  library(planscanR.screen)
}

args <- commandArgs(trailingOnly = TRUE)
port <- 43874L
port_arg <- grep("^--port=", args, value = TRUE)
if (length(port_arg) > 0L) {
  port <- suppressWarnings(as.integer(sub("^--port=", "", port_arg[[1L]])))
}
if (length(port) != 1L || is.na(port) || port < 1024L || port > 65535L) {
  stop("--port must be a TCP port between 1024 and 65535.", call. = FALSE)
}

fixture_root <- file.path(tempdir(), paste0("planscanR-acq-run4-", Sys.getpid()))
if (dir.exists(fixture_root)) {
  unlink(fixture_root, recursive = TRUE, force = TRUE)
}
dir.create(fixture_root, recursive = TRUE, showWarnings = FALSE)
catalog <- acquisition_catalog_open(file.path(fixture_root, "catalog.sqlite"))
on.exit(acquisition_catalog_close(catalog), add = TRUE)
source <- acquisition_catalog_register_source(
  catalog,
  country = "nl",
  portal = "fixture.example",
  native_project_id = "run4-project",
  source_url = "https://fixture.example/run4"
)
document <- acquisition_catalog_register_document(
  catalog,
  source$source_ref_id,
  native_attachment_id = "run4-document",
  attachment_url = "https://fixture.example/run4.pdf"
)
acquisition_queue_enqueue(catalog, "download", "document", document$document_ref_id)

worker_id <- "run4-worker"
configure_worker <- function(id = worker_id) {
  acquisition_worker_health_configure(catalog, id)
  invisible(id)
}
configure_worker()
acquisition_worker_health_start(catalog, worker_id)

set_worker_mode <- function(mode) {
  if (identical(mode, "unknown")) {
    configure_worker("run4-unknown")
    return(invisible(TRUE))
  }
  configure_worker(worker_id)
  if (identical(mode, "idle")) {
    acquisition_worker_health_start(catalog, worker_id)
  } else if (identical(mode, "busy")) {
    acquisition_worker_health_start(catalog, worker_id)
    acquisition_worker_health_heartbeat(catalog, worker_id, state = "busy")
  } else if (identical(mode, "stale")) {
    acquisition_worker_health_start(catalog, worker_id, now = Sys.time() - 120)
  } else if (identical(mode, "offline")) {
    acquisition_worker_health_start(catalog, worker_id)
    acquisition_worker_health_stop(catalog, worker_id)
  }
  invisible(TRUE)
}

verified_path <- file.path(fixture_root, "run4.pdf")
complete_verified_download <- function() {
  configure_worker()
  acquisition_worker_health_start(catalog, worker_id)
  claim <- acquisition_queue_claim(catalog, worker_id)
  if (!isTRUE(claim$ok)) {
    return(list(ok = FALSE, status = claim$status %||% "not-claimed"))
  }
  writeBin(charToRaw("ACQ-RUN4 verified fixture bytes\n"), verified_path)
  size_bytes <- file.info(verified_path)$size
  sha256 <- paste0(openssl::sha256(readBin(verified_path, "raw", n = size_bytes)))
  acquisition_queue_complete(
    catalog,
    claim$job_id,
    claim$lease_token,
    status = "succeeded",
    result = list(
      status = "succeeded",
      document_ref_id = document$document_ref_id,
      root_id = "fixture",
      relative_path = "run4.pdf",
      local_path = verified_path,
      size_bytes = size_bytes,
      sha256 = sha256,
      verification_status = "verified",
      provenance = "acq-run4-fixture"
    )
  )
}

actor <- curation_actor("acq-run4-admin", roles = "admin", display_name = "ACQ-RUN4 fixture admin")
actor_resolver <- curation_actor_resolver(function(request) actor)
tick <- shiny::reactiveTimer(250)

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5L),
  shiny::tags$h1("ACQ-RUN4 dashboard fixture"),
  shiny::tags$p("Loopback-only disposable catalog; no production state or credentials."),
  shiny::actionButton("fixture-idle", "Idle"),
  shiny::actionButton("fixture-busy", "Busy"),
  shiny::actionButton("fixture-offline", "Offline"),
  shiny::actionButton("fixture-stale", "Stale"),
  shiny::actionButton("fixture-unknown", "Unknown"),
  shiny::actionButton("fixture-verified", "Complete verified download"),
  shiny::verbatimTextOutput("fixture-state"),
  acquisition_dashboard_ui("acquisition")
)

server <- function(input, output, session) {
  shiny::observeEvent(input$`fixture-idle`, set_worker_mode("idle"), ignoreInit = TRUE)
  shiny::observeEvent(input$`fixture-busy`, set_worker_mode("busy"), ignoreInit = TRUE)
  shiny::observeEvent(input$`fixture-offline`, set_worker_mode("offline"), ignoreInit = TRUE)
  shiny::observeEvent(input$`fixture-stale`, set_worker_mode("stale"), ignoreInit = TRUE)
  shiny::observeEvent(input$`fixture-unknown`, set_worker_mode("unknown"), ignoreInit = TRUE)
  shiny::observeEvent(input$`fixture-verified`, complete_verified_download(), ignoreInit = TRUE)
  output$`fixture-state` <- shiny::renderText({
    tick()
    current <- acquisition_dashboard_summary(catalog)
    paste(
      "health", current$execution_health$status,
      "pending", current$progress$pending_jobs,
      "running", current$progress$running_jobs,
      "verified files", current$progress$verified_file_count,
      "verified bytes", current$progress$verified_bytes,
      "latest success", current$progress$latest_success_at %||% "unknown",
      sep = " | "
    )
  })
  acquisition_dashboard_server(
    id = "acquisition",
    catalog = catalog,
    actor_resolver = actor_resolver,
    refresh_ms = 250L,
    page_size = 20L
  )
}

shiny::runApp(
  shiny::shinyApp(ui = ui, server = server),
  host = "127.0.0.1",
  port = port,
  launch.browser = FALSE
)
