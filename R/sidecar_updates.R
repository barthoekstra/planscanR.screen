# Small downstream sidecar projection helper. The leaf package owns the
# read/merge/write lock; screen supplies only the derived fields it produced.

screen_update_sidecar_fields <- function(record, fields, root = NULL) {
  if (!is.data.frame(record) || nrow(record) != 1L) {
    cli::cli_abort("{.arg record} must be exactly one data-frame row.")
  }
  fields <- intersect(fields, names(record))
  update_fn <- function(current) {
    for (field in fields) {
      current[[field]] <- record[[field]]
    }
    current
  }
  updater <- tryCatch(
    getExportedValue("planscanR", "update_record_sidecar"),
    error = function(error) NULL
  )
  if (is.function(updater)) {
    updated <- tryCatch(
      {
        updater(
          country = as.character(record$country[[1L]]),
          document_id = as.character(record$document_id[[1L]]),
          update_fn = update_fn,
          root = root
        )
        TRUE
      },
      error = function(error) {
        if (inherits(error, "planscanR_error_sidecar_missing")) {
          FALSE
        } else {
          stop(error)
        }
      }
    )
    if (isTRUE(updated)) {
      return(invisible(TRUE))
    }
  }
  # A missing sidecar is the only creation path. Keep source metadata and
  # attachment provenance needed to create it, but do not publish another
  # writer's derived projection from a stale input row.
  derived <- grepl(
    "^(relevance_score_|relevance_model$|class_label$|class_score$|class_relevant$|class_model$|class_score_|translation_)",
    names(record)
  )
  creation <- record[, !derived, drop = FALSE]
  for (field in fields) {
    creation[[field]] <- record[[field]]
  }
  downloads <- creation[["download_status"]]
  downloads <- if (is.list(downloads) && length(downloads) >= 1L) downloads[[1L]] else NULL
  planscanR::write_record_sidecar(creation, downloads = downloads, root = root)
  invisible(TRUE)
}
