#!/usr/bin/env Rscript
main <- function(args) {
  if (!length(args)) {
    stop("Usage: Rscript scripts/check-styling.R <file.R> [...]; pass explicit files", call. = FALSE)
  }

  config <- normalizePath(".styler.R", mustWork = TRUE)
  env <- new.env(parent = globalenv())
  sys.source(config, envir = env)
  if (!inherits(env$styler_style, "function")) {
    stop(".styler.R must define styler_style as a styler style function", call. = FALSE)
  }

  files <- normalizePath(args, mustWork = TRUE)
  tmp <- tempfile("styler-check-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  options("R.cache.rootPath" = file.path(tmp, "cache"))
  bad <- character()
  for (i in seq_along(files)) {
    if (!grepl("\\.[Rr]$", files[[i]])) stop("Only explicit R files are accepted", call. = FALSE)
    copy <- file.path(tmp, sprintf("%04d.R", i))
    file.copy(files[[i]], copy)
    withCallingHandlers(
      invisible(capture.output(styler::style_file(copy, style = env$styler_style))),
      warning = function(w) stop(conditionMessage(w), call. = FALSE)
    )
    if (!identical(
      readBin(files[[i]], "raw", n = file.info(files[[i]])$size),
      readBin(copy, "raw", n = file.info(copy)$size)
    )) {
      bad <- c(bad, args[[i]])
    }
  }
  if (length(bad)) {
    cat("Unformatted R files:\n", paste0("  ", bad, collapse = "\n"), "\n", sep = "")
    quit(status = 1L)
  }
  cat("Styling check passed for ", length(files), " file(s).\n", sep = "")
}

main(commandArgs(trailingOnly = TRUE))
