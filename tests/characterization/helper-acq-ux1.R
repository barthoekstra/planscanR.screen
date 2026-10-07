package_root <- normalizePath(getwd(), mustWork = TRUE)
runner_path <- file.path(package_root, "inst", "curation", "acq_ux1_characterization.R")
if (!file.exists(runner_path)) {
  package_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = TRUE)
  runner_path <- file.path(package_root, "inst", "curation", "acq_ux1_characterization.R")
}
source(runner_path, local = FALSE)
