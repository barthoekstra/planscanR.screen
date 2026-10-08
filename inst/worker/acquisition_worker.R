#!/usr/bin/env Rscript

# Minimal supervised entry point.  The supervisor owns restart policy and
# process limits; this script opens the durable catalog and runs queue claims
# outside Shiny. `--idle` keeps a worker process alive between empty claims so
# systemd can supervise a stable two-to-four-process pool.

main <- function() {
args <- commandArgs(trailingOnly = TRUE)

value_for <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit) == 0L) return(default)
  substring(hit[[1L]], nchar(prefix) + 1L)
}

has_flag <- function(name) paste0("--", name) %in% args

logical_value <- function(value, name) {
  value <- tolower(trimws(as.character(value[[1L]])))
  if (!value %in% c("true", "false", "1", "0", "yes", "no")) {
    stop("--", name, " must be true or false", call. = FALSE)
  }
  value %in% c("true", "1", "yes")
}

catalog_path <- value_for("catalog")
worker_id <- value_for("worker", paste0("worker-", Sys.getpid()))
expected_workers <- trimws(strsplit(value_for("expected-workers", worker_id), ",", fixed = TRUE)[[1L]])
max_jobs <- value_for("max-jobs", "1")
lease_seconds <- value_for("lease-seconds", "300")
poll_seconds <- suppressWarnings(as.numeric(value_for("poll-seconds", "5")))
idle <- has_flag("idle") || logical_value(value_for("idle", "false"), "idle")

if (is.null(catalog_path) || !nzchar(catalog_path)) {
  stop("Usage: acquisition_worker.R --catalog=/path/catalog.sqlite [--worker=ID] [--max-jobs=N] [--lease-seconds=N]", call. = FALSE)
}

acquisition_r_library <- Sys.getenv("CURATION_ACQUISITION_R_LIB", unset = "")
if (nzchar(acquisition_r_library)) {
  acquisition_r_library <- normalizePath(acquisition_r_library, mustWork = FALSE)
  if (!dir.exists(acquisition_r_library)) {
    stop("CURATION_ACQUISITION_R_LIB does not exist: ", acquisition_r_library, call. = FALSE)
  }
  .libPaths(unique(c(acquisition_r_library, .libPaths())))
}

if (!requireNamespace("planscanR.screen", quietly = TRUE)) {
  stop("Install planscanR.screen before running the acquisition worker.", call. = FALSE)
}

catalog <- planscanR.screen::acquisition_catalog_open(catalog_path)
health_started <- FALSE
health_failed <- FALSE
on.exit({
  if (isTRUE(health_started) && !isTRUE(health_failed)) {
    try(
      planscanR.screen::acquisition_worker_health_stop(catalog, worker_id),
      silent = TRUE
    )
  }
  try(planscanR.screen::acquisition_catalog_close(catalog), silent = TRUE)
}, add = TRUE)
max_jobs <- if (identical(max_jobs, "Inf")) Inf else as.integer(max_jobs)
lease_seconds <- as.integer(lease_seconds)
if (!is.finite(poll_seconds) || poll_seconds < 0 || poll_seconds > 3600) {
  stop("--poll-seconds must be between 0 and 3600", call. = FALSE)
}
if (length(expected_workers) < 1L || length(expected_workers) > 32L ||
  any(!nzchar(expected_workers)) || anyDuplicated(expected_workers) > 0L ||
  !worker_id %in% expected_workers) {
  stop("--expected-workers must contain one to 32 unique IDs including --worker", call. = FALSE)
}

tryCatch({
  planscanR.screen::acquisition_worker_health_configure(catalog, expected_workers)
  planscanR.screen::acquisition_worker_health_start(catalog, worker_id)
  health_started <- TRUE
  repeat {
    outcomes <- planscanR.screen::acquisition_worker_run(
      catalog,
      worker_id = worker_id,
      max_jobs = max_jobs,
      lease_seconds = lease_seconds
    )
    if (!idle) break
    if (length(outcomes) == 0L && poll_seconds > 0) {
      remaining <- poll_seconds
      while (remaining > 0) {
        interval <- min(remaining, 10)
        Sys.sleep(interval)
        remaining <- remaining - interval
        planscanR.screen::acquisition_worker_health_heartbeat(catalog, worker_id, state = "idle")
      }
    }
  }
}, error = function(error) {
  if (isTRUE(health_started)) {
    health_failed <<- TRUE
    try(
      planscanR.screen::acquisition_worker_health_stop(
        catalog,
        worker_id,
        state = "error",
        error_category = "worker-process",
        error_message = conditionMessage(error)
      ),
      silent = TRUE
    )
  }
  stop(error)
})
}

main()
