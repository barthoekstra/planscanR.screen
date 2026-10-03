#!/usr/bin/env Rscript

# Minimal supervised entry point.  The supervisor owns restart policy and
# process limits; this script only opens the durable catalog and runs a bounded
# number of queue claims outside Shiny.

args <- commandArgs(trailingOnly = TRUE)

value_for <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit) == 0L) return(default)
  substring(hit[[1L]], nchar(prefix) + 1L)
}

catalog_path <- value_for("catalog")
worker_id <- value_for("worker", paste0("worker-", Sys.getpid()))
max_jobs <- value_for("max-jobs", "1")
lease_seconds <- value_for("lease-seconds", "300")

if (is.null(catalog_path) || !nzchar(catalog_path)) {
  stop("Usage: acquisition_worker.R --catalog=/path/catalog.sqlite [--worker=ID] [--max-jobs=N] [--lease-seconds=N]", call. = FALSE)
}
if (!requireNamespace("planscanR.screen", quietly = TRUE)) {
  stop("Install planscanR.screen before running the acquisition worker.", call. = FALSE)
}

catalog <- planscanR.screen::acquisition_catalog_open(catalog_path)
on.exit(planscanR.screen::acquisition_catalog_close(catalog), add = TRUE)
max_jobs <- if (identical(max_jobs, "Inf")) Inf else as.integer(max_jobs)
lease_seconds <- as.integer(lease_seconds)
planscanR.screen::acquisition_worker_run(
  catalog,
  worker_id = worker_id,
  max_jobs = max_jobs,
  lease_seconds = lease_seconds
)
