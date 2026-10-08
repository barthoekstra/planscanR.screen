# ACQ-UX6: a small read-mostly view over the acquisition storage catalog.
#
# The panel deliberately delegates writes and validation to the existing
# acquisition_storage_* APIs. It has no path editor, relocation action, or
# process-wide configuration resolver.

acquisition_storage_format_bytes <- function(value, unknown = "Unknown") {
  value <- suppressWarnings(as.numeric(value))
  if (length(value) != 1L || is.na(value) || !is.finite(value) || value < 0) {
    return(unknown)
  }
  units <- c("B", "KiB", "MiB", "GiB", "TiB", "PiB")
  index <- if (value < 1024) 1L else min(length(units), floor(log(value, base = 1024)) + 1L)
  scaled <- value / 1024^(index - 1L)
  if (index == 1L) sprintf("%.0f %s", scaled, units[[index]]) else sprintf("%.2f %s", scaled, units[[index]])
}

acquisition_storage_path_state <- function(path, missing_reason = "Unavailable") {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(trimws(as.character(path)))) {
    return(list(path = missing_reason, status = "Unavailable", writable = FALSE))
  }
  path <- normalizePath(as.character(path), winslash = "/", mustWork = FALSE)
  if (!dir.exists(path)) {
    return(list(path = path, status = "Unavailable", writable = FALSE))
  }
  writable <- isTRUE(file.access(path, mode = 2L) == 0L)
  list(
    path = path,
    status = if (writable) "Ready" else "Read-only",
    writable = writable
  )
}

acquisition_storage_effective <- function(catalog, storage_context = NULL, state_root = NULL) {
  if (is.null(storage_context)) storage_context <- list()
  if (!is.list(storage_context)) {
    acquisition_storage_abort("{.arg storage_context} must be a list or NULL.")
  }
  state_root <- state_root %||% storage_context$state_root
  cache_state <- acquisition_storage_path_state(storage_context$cache_root)
  catalog_path <- storage_context$catalog_path %||% catalog$path
  catalog_dir <- if (is.null(catalog_path) || length(catalog_path) != 1L || is.na(catalog_path)) {
    NULL
  } else {
    dirname(normalizePath(as.character(catalog_path), winslash = "/", mustWork = FALSE))
  }
  roots <- tryCatch(acquisition_storage_roots(catalog, active = TRUE), error = function(error) NULL)
  download <- if (!is.null(roots) && nrow(roots) > 0L) {
    preferred <- which(as.character(roots$purpose) == "downloads")
    roots[if (length(preferred) > 0L) preferred[[1L]] else 1L, , drop = FALSE]
  } else {
    NULL
  }
  download_path <- if (is.null(download)) NULL else as.character(download$root_path[[1L]])
  download_state <- acquisition_storage_path_state(download_path)
  capacity <- if (isTRUE(download_state$status == "Unavailable")) {
    NULL
  } else {
    tryCatch(acquisition_storage_capacity(download_state$path), error = function(error) NULL)
  }
  reserve <- if (is.null(download)) {
    "Unavailable"
  } else {
    paste0(
      acquisition_storage_format_bytes(download$reserve_bytes[[1L]]),
      " fixed; ",
      sprintf("%.1f%% of total capacity", 100 * as.numeric(download$reserve_fraction[[1L]]))
    )
  }
  destinations <- data.frame(
    destination = c("Download files", "Curation state", "Acquisition catalog", "Source cache"),
    purpose = c("downloads", "state", "catalog", "cache"),
    path = c(download_state$path, acquisition_storage_path_state(state_root)$path, acquisition_storage_path_state(catalog_dir)$path, cache_state$path),
    status = c(download_state$status, acquisition_storage_path_state(state_root)$status, acquisition_storage_path_state(catalog_dir)$status, cache_state$status),
    capacity = c(
      if (is.null(capacity)) {
        "Unknown"
      } else {
        paste0(
          acquisition_storage_format_bytes(capacity$free_bytes), " free of ",
          acquisition_storage_format_bytes(capacity$total_bytes)
        )
      },
      "Not measured",
      "Not measured",
      "Not measured"
    ),
    reserve = c(reserve, "Not applicable", "Not applicable", "Not applicable"),
    source = c(
      if (is.null(download)) "Fallback: no registered active root" else "Registered catalog root",
      if (is.null(state_root)) "Fallback: launcher state root unavailable" else "Explicit launcher state root",
      if (is.null(storage_context$catalog_path)) "Fallback: catalog directory (dashboard-only)" else "Explicit catalog path",
      if (is.null(storage_context$cache_root)) "Fallback: source cache unavailable" else "Explicit read-only cache root"
    ),
    stringsAsFactors = FALSE
  )

  settings <- tryCatch(
    acquisition_storage_settings(catalog, "default_estimate_bytes"),
    error = function(error) data.frame()
  )
  floor_value <- tryCatch(acquisition_storage_transfer_cap_bytes(), error = function(error) NA_real_)
  floor_display <- acquisition_storage_format_bytes(floor_value)
  if (nrow(settings) == 0L) {
    fallback <- tryCatch(acquisition_storage_transfer_cap_bytes(), error = function(error) NA_real_)
    effective_value <- if (is.finite(fallback)) max(fallback, floor_value) else fallback
    setting_view <- data.frame(
      setting = "Unknown-size download estimate",
      key = "default_estimate_bytes",
      value = fallback,
      display_value = acquisition_storage_format_bytes(fallback),
      effective_value = effective_value,
      effective_display_value = acquisition_storage_format_bytes(effective_value),
      floor_value = floor_value,
      floor_display_value = floor_display,
      units = "bytes",
      version = NA_integer_,
      changed_by = "Fallback",
      changed_at = NA_character_,
      status = "Fallback",
      effect = paste0("Used for future download reservations; effective reservation is max(configured estimate, ", floor_display, ")."),
      stringsAsFactors = FALSE
    )
  } else {
    latest <- settings[which.max(as.integer(settings$version)), , drop = FALSE]
    value <- suppressWarnings(as.numeric(latest$value[[1L]]))
    effective_value <- if (is.finite(value) && is.finite(floor_value)) max(value, floor_value) else value
    setting_view <- data.frame(
      setting = "Unknown-size download estimate",
      key = "default_estimate_bytes",
      value = value,
      display_value = acquisition_storage_format_bytes(value),
      effective_value = effective_value,
      effective_display_value = acquisition_storage_format_bytes(effective_value),
      floor_value = floor_value,
      floor_display_value = floor_display,
      units = "bytes",
      version = as.integer(latest$version[[1L]]),
      changed_by = as.character(latest$changed_by[[1L]]),
      changed_at = as.character(latest$changed_at[[1L]]),
      status = "Configured",
      effect = paste0("Applies to future download reservations; effective reservation is max(configured estimate, ", floor_display, "). Existing files and campaigns are unchanged."),
      stringsAsFactors = FALSE
    )
  }
  list(destinations = destinations, settings = setting_view)
}

acquisition_storage_settings_ui <- function(ns = identity) {
  shiny <- asNamespace("shiny")
  bslib <- asNamespace("bslib")
  bslib$card(
    id = ns("storage-settings-panel"),
    class = "storage-settings-panel",
    htmltools::tags$style(htmltools::HTML(
      ".storage-settings-panel{max-width:100%;}\n",
      ".storage-settings-panel .shiny-table-output{max-width:100%;overflow-x:auto;}\n",
      ".storage-settings-panel .shiny-table-output table{width:max-content;min-width:760px;table-layout:auto;}\n",
      ".storage-settings-panel td,.storage-settings-panel th{white-space:normal;overflow-wrap:anywhere;word-break:break-word;}"
    )),
    bslib$card_header(
      htmltools::div(
        class = "d-flex justify-content-between align-items-center",
        htmltools::span("Effective storage and settings"),
        curation_app_help(
          "Storage and settings",
          "These values come from the registered acquisition catalog and the explicit launcher state root. Paths are displayed for orientation; this panel cannot relocate files or edit paths."
        )
      )
    ),
    shiny$helpText(
      "Download destinations are registered catalog roots. Curation state is kept in the launcher's explicit writable state root. The source cache is read-only. The supported estimate affects future download jobs; a service restart is not required. Unknown values stay visible as unavailable."
    ),
    shiny$tableOutput(ns("storage-effective")),
    shiny$uiOutput(ns("storage-controls")),
    shiny$textOutput(ns("storage-result")),
    fill = FALSE
  )
}

acquisition_storage_settings_server <- function(
  input,
  output,
  session,
  catalog,
  actor_resolver,
  request = NULL,
  storage_context = NULL
) {
  refresh <- shiny::reactiveVal(0L)
  effective <- shiny::reactive({
    refresh()
    acquisition_storage_effective(catalog, storage_context = storage_context)
  })
  output$`storage-effective` <- shiny::renderTable(
    {
      value <- effective()
      destinations <- value$destinations
      settings <- value$settings[c("setting", "display_value", "effective_display_value", "floor_display_value", "units", "version", "changed_by", "status", "effect")]
      names(settings) <- c("Setting", "Configured value", "Effective reservation", "Minimum floor", "Units", "Version", "Changed by", "Status", "Effect")
      destinations <- destinations[c("destination", "purpose", "path", "status", "capacity", "reserve", "source")]
      names(destinations) <- c("Destination", "Purpose", "Path", "Status", "Capacity", "Reserve", "Source")
      rbind(
        destinations,
        data.frame(
          Destination = settings$Setting,
          Purpose = "versioned setting",
          Path = paste0(settings$`Configured value`, " ", settings$Units),
          Status = settings$Status,
          Capacity = paste0("Effective ", settings$`Effective reservation`, "; floor ", settings$`Minimum floor`, ". ", settings$Effect),
          Reserve = paste0("v", settings$Version),
          Source = settings$`Changed by`,
          stringsAsFactors = FALSE
        )
      )
    },
    striped = TRUE,
    bordered = TRUE,
    spacing = "s"
  )
  output$`storage-controls` <- shiny::renderUI({
    actor <- tryCatch(
      curation_resolve_actor(actor_resolver, acquisition_dashboard_request(request, session)),
      error = function(error) NULL
    )
    if (is.null(actor) || !any(tolower(as.character(actor$roles)) == "admin")) {
      return(shiny::helpText(
        "Storage settings are read-only for reviewers. An authenticated administrator is required to save the supported setting.",
        class = "storage-settings-read-only"
      ))
    }
    setting <- effective()$settings
    floor <- as.numeric(setting$floor_value[[1L]])
    floor_display <- setting$floor_display_value[[1L]]
    shiny::tags$div(
      class = "storage-settings-controls",
      shiny::helpText(
        paste0("Supported setting: the byte estimate reserved for an unknown-size download. The built-in reservation floor is ", floor_display, "; enter at least this value. It affects future queue claims; existing files and campaigns are unchanged and a service restart is not required.")
      ),
      shiny::numericInput(
        session$ns("storage-default-estimate"),
        "Unknown-size download estimate (bytes)",
        value = if (is.finite(setting$effective_value[[1L]])) setting$effective_value[[1L]] else 0,
        min = if (is.finite(floor)) floor else 0,
        max = 9e18,
        step = 1
      ),
      shiny::actionButton(session$ns("storage-save"), "Save for future download jobs", class = "btn-outline-primary btn-sm")
    )
  })
  result <- shiny::reactiveVal(
    "No storage setting changes submitted. Existing files and campaigns remain unchanged."
  )
  output$`storage-result` <- shiny::renderText(result())
  shiny::observeEvent(input$`storage-save`,
    {
      value <- suppressWarnings(as.numeric(input$`storage-default-estimate`))
      saved <- tryCatch(
        {
          actor <- curation_resolve_actor(actor_resolver, acquisition_dashboard_request(request, session))
          floor <- acquisition_storage_transfer_cap_bytes()
          if (length(value) != 1L || is.na(value) || !is.finite(value) || value < floor) {
            acquisition_storage_abort(paste0("The unknown-size download estimate must be at least ", acquisition_storage_format_bytes(floor), "."))
          }
          acquisition_storage_set(catalog, "default_estimate_bytes", value, actor)
        },
        error = identity
      )
      if (inherits(saved, "error")) {
        result(conditionMessage(saved))
        return()
      }
      refresh(refresh() + 1L)
      result(paste0(
        "Saved version ", saved$version,
        " for future download jobs. Existing files and campaigns are unchanged; service restart is not required."
      ))
    },
    ignoreInit = TRUE
  )
  invisible(list(effective = effective, result = result))
}
