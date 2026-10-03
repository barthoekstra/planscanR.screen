#!/usr/bin/env Rscript

# Minimal supervised entry point.  The supervisor owns restart policy and
# process limits; this script opens the durable catalog and runs queue claims
# outside Shiny. `--idle` keeps a worker process alive between empty claims so
# systemd can supervise a stable two-to-four-process pool.

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
on.exit(planscanR.screen::acquisition_catalog_close(catalog), add = TRUE)
max_jobs <- if (identical(max_jobs, "Inf")) Inf else as.integer(max_jobs)
lease_seconds <- as.integer(lease_seconds)
if (!is.finite(poll_seconds) || poll_seconds < 0 || poll_seconds > 3600) {
  stop("--poll-seconds must be between 0 and 3600", call. = FALSE)
}

repeat {
  outcomes <- planscanR.screen::acquisition_worker_run(
    catalog,
    worker_id = worker_id,
    max_jobs = max_jobs,
    lease_seconds = lease_seconds
  )
  if (!idle) break
  if (length(outcomes) == 0L) Sys.sleep(poll_seconds)
}
