# ACQ-RUN4 installed-package fixture smoke.
#
# Usage: ACQ_RUN4_RLIB=/path/to/.acquisitionRlib Rscript --vanilla \
#   scripts/acq-run4-installed-smoke.R

run_smoke <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  lib_arg <- grep("^--lib=", args, value = TRUE)
  installed_lib <- if (length(lib_arg) > 0L) {
    sub("^--lib=", "", lib_arg[[1L]])
  } else {
    Sys.getenv("ACQ_RUN4_RLIB", "")
  }
  if (!nzchar(installed_lib) || !dir.exists(installed_lib)) {
    stop("Set ACQ_RUN4_RLIB to the installed acquisition R library.", call. = FALSE)
  }
  old_flag <- Sys.getenv("ACQ_RUN4_USE_INSTALLED", unset = NA_character_)
  on.exit(
    if (is.na(old_flag)) Sys.unsetenv("ACQ_RUN4_USE_INSTALLED") else Sys.setenv(ACQ_RUN4_USE_INSTALLED = old_flag),
    add = TRUE
  )
  Sys.setenv(ACQ_RUN4_USE_INSTALLED = "true")
  .libPaths(c(normalizePath(installed_lib), .libPaths()))
  suppressPackageStartupMessages(
    library(planscanR.screen, lib.loc = normalizePath(installed_lib))
  )
  fixture_file <- system.file(
    "curation/acq_run4_dashboard_fixture.R",
    package = "planscanR.screen"
  )
  if (!nzchar(fixture_file) || !file.exists(fixture_file)) {
    stop("The installed package does not contain the ACQ-RUN4 fixture.", call. = FALSE)
  }
  fixture_env <- new.env(parent = globalenv())
  sys.source(fixture_file, envir = fixture_env)
  loaded_package <- normalizePath(find.package("planscanR.screen"), mustWork = TRUE)
  expected_package <- normalizePath(file.path(installed_lib, "planscanR.screen"), mustWork = TRUE)
  if (!identical(loaded_package, expected_package)) {
    stop("The installed smoke did not load planscanR.screen from ACQ_RUN4_RLIB.", call. = FALSE)
  }
  state <- fixture_env$main(run_app = FALSE)
  on.exit(state$cleanup(), add = TRUE)
  summary <- planscanR.screen::acquisition_dashboard_summary(state$catalog)
  if (!identical(summary$execution_health$status, "idle")) {
    stop("Installed fixture did not retain the open catalog lifecycle.", call. = FALSE)
  }
  if (!identical(as.integer(summary$progress$pending_jobs), 1L)) {
    stop("Installed fixture did not create its synthetic pending job.", call. = FALSE)
  }
  cat("ACQ-RUN4 installed fixture smoke passed\n")
  invisible(TRUE)
}

run_smoke()
