# Configuration-driven curation app.
#
# The app is deliberately kept behind runtime dependency checks.  The screen
# package remains usable without Shiny, bslib, reactable, or plotly; callers
# opt into this UI by supplying a complete curation context.

curation_app_ui_packages <- function() {
  c("shiny", "bslib", "reactable", "plotly", "htmltools")
}

curation_app_require_ui <- function() {
  missing <- curation_app_ui_packages()[!vapply(
    curation_app_ui_packages(),
    requireNamespace,
    logical(1),
    quietly = TRUE
  )]
  if (length(missing) > 0L) {
    curation_abort(
      paste0(
        "The curation app needs optional package",
        if (length(missing) == 1L) "" else "s",
        ": ", paste(missing, collapse = ", "), "."
      ),
      class = "planscanR_error_curation_missing_dependency"
    )
  }
  invisible(TRUE)
}

curation_app_check_actor_resolver <- function(resolver) {
  if (!inherits(resolver, "planscanR_curation_actor_resolver")) {
    curation_abort(
      "{.arg actor_resolver} must be a trusted curation actor resolver.",
      class = "planscanR_error_curation_actor"
    )
  }
  invisible(resolver)
}

curation_app_empty_records <- function() {
  tibble::tibble(
    country = character(0),
    document_id = character(0),
    title = character(0),
    summary = character(0),
    cosine_max = numeric(0),
    class_label = character(0),
    class_score = numeric(0),
    class_relevant = logical(0),
    kw_total = integer(0),
    n_attachments = integer(0),
    has_attachments = logical(0),
    n_downloaded = integer(0),
    has_downloaded = logical(0),
    selected = logical(0)
  )
}

curation_app_empty_reviews <- function() {
  data.frame(
    country = character(0),
    document_id = character(0),
    reviewer_id = character(0),
    decision = character(0),
    source = character(0),
    note = character(0),
    reviewed_at = as.POSIXct(character(0), tz = "UTC"),
    sidecar_path = character(0),
    revision = integer(0),
    deleted = logical(0),
    stringsAsFactors = FALSE
  )
}

curation_app_normalize_reviews <- function(rows) {
  if (is.null(rows) || !is.data.frame(rows) || nrow(rows) == 0L) {
    return(curation_app_empty_reviews())
  }
  if (!"reviewer_id" %in% names(rows) && "reviewer" %in% names(rows)) {
    rows$reviewer_id <- as.character(rows$reviewer)
  }
  required <- c("country", "document_id", "reviewer_id", "decision", "source")
  missing <- setdiff(required, names(rows))
  if (length(missing) > 0L) {
    curation_abort(
      paste0("The review repository omitted required columns: ", paste(missing, collapse = ", "), "."),
      class = "planscanR_error_curation_repository"
    )
  }
  if (!"note" %in% names(rows)) rows$note <- NA_character_
  if (!"reviewed_at" %in% names(rows)) {
    rows$reviewed_at <- as.POSIXct(rep(NA_real_, nrow(rows)), origin = "1970-01-01", tz = "UTC")
  }
  if (!"sidecar_path" %in% names(rows)) rows$sidecar_path <- NA_character_
  if (!"revision" %in% names(rows)) rows$revision <- 0L
  if (!"deleted" %in% names(rows)) rows$deleted <- FALSE
  rows$country <- as.character(rows$country)
  rows$document_id <- as.character(rows$document_id)
  rows$reviewer_id <- as.character(rows$reviewer_id)
  rows$decision <- as.character(rows$decision)
  rows$source <- as.character(rows$source)
  rows$revision <- as.integer(rows$revision)
  rows$deleted <- as.logical(rows$deleted)
  rows[, c(
    "country", "document_id", "reviewer_id", "decision", "source", "note",
    "reviewed_at", "sidecar_path", "revision", "deleted",
    setdiff(names(rows), c(
      "country", "document_id", "reviewer_id", "decision", "source", "note",
      "reviewed_at", "sidecar_path", "revision", "deleted"
    ))
  ), drop = FALSE]
}

curation_app_active_reviews <- function(rows) {
  rows <- curation_app_normalize_reviews(rows)
  if (nrow(rows) == 0L) {
    return(rows)
  }
  rows[!rows$deleted & !is.na(rows$decision) & nzchar(rows$decision), , drop = FALSE]
}

curation_app_key_string <- function(country, document_id, reviewer_id = NULL) {
  if (length(country) == 0L || length(document_id) == 0L) {
    return(character(0))
  }
  key <- paste0(as.character(country), "::", as.character(document_id))
  if (!is.null(reviewer_id)) {
    key <- paste0(key, "::", as.character(reviewer_id))
  }
  key
}

curation_app_current_revision <- function(rows, country, document_id, reviewer_id) {
  rows <- curation_app_normalize_reviews(rows)
  if (nrow(rows) == 0L) {
    return(0L)
  }
  hit <- rows$country == as.character(country) &
    rows$document_id == as.character(document_id) &
    rows$reviewer_id == as.character(reviewer_id)
  if (!any(hit)) {
    return(0L)
  }
  as.integer(rows$revision[which(hit)[1L]])
}

curation_app_align_review_row <- function(rows, row, previous = NULL) {
  missing <- setdiff(names(rows), names(row))
  for (name in missing) {
    value <- if (!is.null(previous) && name %in% names(previous)) {
      previous[[name]][[1L]]
    } else {
      rows[[name]][NA_integer_]
    }
    row[[name]] <- value
  }
  additional <- setdiff(names(row), names(rows))
  for (name in additional) {
    rows[[name]] <- row[[name]][rep(NA_integer_, nrow(rows))]
  }
  row[, names(rows), drop = FALSE]
}

curation_app_replace_review_result <- function(rows, decision, result, actor) {
  rows <- curation_app_normalize_reviews(rows)
  if (is.null(result) || is.null(result$key)) {
    return(rows)
  }
  key <- result$key
  reviewer_id <- key$reviewer_id %||% actor$id
  hit <- rows$country == key$country &
    rows$document_id == key$document_id &
    rows$reviewer_id == reviewer_id
  previous <- if (any(hit)) rows[which(hit)[[1L]], , drop = FALSE] else NULL
  if (any(hit)) rows <- rows[!hit, , drop = FALSE]
  if (isTRUE(result$deleted)) {
    previous_source <- if (!is.null(previous)) as.character(previous$source[[1L]]) else NA_character_
    if (is.na(previous_source) || !nzchar(previous_source)) previous_source <- "clear"
    tombstone <- data.frame(
      country = key$country,
      document_id = key$document_id,
      reviewer_id = reviewer_id,
      decision = NA_character_,
      source = if (is.null(decision)) previous_source else decision$source[[1L]],
      note = if (is.null(decision) && !is.null(previous)) previous$note[[1L]] else if (is.null(decision)) NA_character_ else decision$note[[1L]],
      reviewed_at = if (is.null(decision) && !is.null(previous)) {
        previous$reviewed_at[[1L]]
      } else if (is.null(decision)) {
        as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
      } else {
        decision$reviewed_at[[1L]]
      },
      sidecar_path = if (is.null(decision) && !is.null(previous)) previous$sidecar_path[[1L]] else if (is.null(decision)) NA_character_ else decision$sidecar_path[[1L]],
      revision = as.integer(result$revision),
      deleted = TRUE,
      stringsAsFactors = FALSE
    )
    tombstone <- curation_app_align_review_row(rows, tombstone, previous)
    return(rbind(rows, tombstone))
  }
  if (is.null(decision)) {
    return(rows)
  }
  decision$revision <- as.integer(result$revision)
  decision$deleted <- FALSE
  decision$reviewer_id <- reviewer_id
  decision <- curation_app_align_review_row(rows, decision, previous)
  curation_app_normalize_reviews(rbind(rows, decision))
}

curation_app_record_key <- function(row) {
  list(country = as.character(row$country[[1L]]), document_id = as.character(row$document_id[[1L]]))
}

curation_app_request_for_session <- function(request, session) {
  if (is.function(request)) {
    return(request(session = session))
  }
  if (is.null(request)) {
    return(list(session = session))
  }
  request
}

curation_app_translate <- function(config, row) {
  key <- curation_app_record_key(row)
  result <- curation_translate(
    provider = config$translator,
    record = row,
    key = key,
    write = FALSE
  )
  list(
    title_en = result$title_en %||% NA_character_,
    summary_en = result$summary_en %||% NA_character_,
    status = result$status %||% "disabled"
  )
}

curation_app_has_text <- function(value) {
  !is.null(value) && length(value) == 1L && !is.na(value) && nzchar(trimws(as.character(value)))
}

curation_app_column_value <- function(row, column, default = NULL) {
  if (!is.data.frame(row) || nrow(row) == 0L || !column %in% names(row)) {
    return(default)
  }
  value <- row[[column]][[1L]]
  if (length(value) != 1L || is.na(value)) default else value
}

curation_app_record_detail <- function(row, config) {
  has <- curation_app_has_text
  tr <- curation_app_translate(config, row)
  text_or <- function(value, fallback) if (has(value)) as.character(value) else fallback
  country <- as.character(curation_app_column_value(row, "country", ""))
  document_id <- as.character(curation_app_column_value(row, "document_id", ""))
  title <- curation_app_column_value(row, "title")
  summary <- curation_app_column_value(row, "summary")
  url <- curation_app_column_value(row, "url")
  htmltools::div(
    style = "padding:12px 16px;background:#fafafa;",
    htmltools::div(
      style = "color:#777;font-size:12px;margin-bottom:8px;",
      sprintf("ID: %s \u00b7 %s", document_id, toupper(country))
    ),
    htmltools::div(
      style = "display:flex;gap:20px;flex-wrap:wrap;",
      htmltools::div(
        style = "flex:1;min-width:280px;",
        htmltools::tags$b("Original"),
        htmltools::tags$h5(text_or(title, "(no title)")),
        htmltools::tags$p(text_or(summary, "(no summary)"), style = "white-space:pre-wrap;")
      ),
      htmltools::div(
        style = "flex:1;min-width:280px;border-left:1px solid #ddd;padding-left:20px;",
        htmltools::tags$b("English"),
        if (tr$status == "disabled") {
          htmltools::tags$p("Translation disabled; original text shown.", style = "color:#888;")
        } else {
          htmltools::tagList(
            htmltools::tags$h5(text_or(tr$title_en, "(no title)")),
            htmltools::tags$p(text_or(tr$summary_en, "(no summary)"), style = "white-space:pre-wrap;")
          )
        }
      )
    ),
    if (has(url)) {
      htmltools::tags$a(href = url, target = "_blank", "Open source page \u2197")
    }
  )
}

curation_app_decision_cell <- function(source, reviewer_id) {
  js_literal <- function(value) {
    value <- gsub("\\\\", "\\\\\\\\", as.character(value))
    value <- gsub("'", "\\'", value, fixed = TRUE)
    value <- gsub("\r", "\\r", value, fixed = TRUE)
    value <- gsub("\n", "\\n", value, fixed = TRUE)
    value <- gsub("\u2028", "\\u2028", value, fixed = TRUE)
    gsub("\u2029", "\\u2029", value, fixed = TRUE)
  }
  reactable::JS(sprintf(
    "function(cellInfo) {
      var src = '%s';
      var id = cellInfo.row['document_id'];
      var country = cellInfo.row['country'];
      var rid = '%s';
      var key = String(country) + '::' + String(id) + '::' + rid;
      var val = cellInfo.value;
      if (window.__curationReviewState === undefined) window.__curationReviewState = {};
      if (!(key in window.__curationReviewState)) {
        window.__curationReviewState[key] = (val === null || val === undefined) ? null : val;
      }
      var cur = window.__curationReviewState[key];
      function attr(s) {
        return String(s)
          .replace(/&/g, '&amp;')
          .replace(/\\r/g, '&#13;')
          .replace(/\\n/g, '&#10;')
          .replace(/\\\"/g, '&quot;')
          .replace(/</g, '&lt;')
          .replace(/>/g, '&gt;');
      }
      function btn(dec, label, color) {
        var active = (cur === dec) ? ' active' : '';
        return '<button type=\"button\" class=\"curation-decision' + active + '\"' +
          ' data-decision=\"' + attr(dec) + '\" style=\"--curation-color:' + attr(color) + '\"' +
          '>' + label + '</button>';
      }
      return '<div class=\"curation-decision-group\" data-review-id=\"' + attr(key) +
        '\" data-record-id=\"' + attr(id) + '\" data-country=\"' + attr(country) +
        '\" data-source=\"' + attr(src) + '\" data-reviewer=\"' + attr(rid) + '\">' +
        btn('keep','Keep','#1a9850') + btn('drop','Drop','#d73027') + btn('unsure','?','#fc8d59') + '</div>';
    }",
    js_literal(source),
    js_literal(reviewer_id)
  ))
}

curation_app_table <- function(data, reviews, config, source = "browse", blind = FALSE, reviewer_id) {
  data <- as.data.frame(data, stringsAsFactors = FALSE)
  if (nrow(data) == 0L) {
    return(reactable::reactable(data.frame(Note = "No records match this view."), pagination = FALSE))
  }
  reviews <- curation_app_active_reviews(reviews)
  lookup <- stats::setNames(
    reviews$decision[reviews$reviewer_id == reviewer_id],
    curation_app_key_string(
      reviews$country[reviews$reviewer_id == reviewer_id],
      reviews$document_id[reviews$reviewer_id == reviewer_id],
      reviewer_id
    )
  )
  data$decision <- unname(lookup[curation_app_key_string(data$country, data$document_id, reviewer_id)])
  has_model <- "select_prob" %in% names(data) && any(!is.na(data$select_prob))
  trunc_cell <- function(value) {
    htmltools::div(
      title = if (is.na(value)) "" else as.character(value),
      style = "white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
      if (is.na(value)) "" else as.character(value)
    )
  }
  cols <- list(
    decision = reactable::colDef(
      name = "Review", sticky = "left", width = 160, sortable = FALSE,
      filterable = FALSE, html = TRUE, cell = curation_app_decision_cell(source, reviewer_id)
    ),
    country = reactable::colDef(name = "Country", width = 90, align = "center"),
    document_id = reactable::colDef(name = "Document", width = 130),
    title = reactable::colDef(name = "Title", minWidth = 220, cell = trunc_cell),
    cosine_max = reactable::colDef(name = "Topic match", width = 120, format = reactable::colFormat(digits = 3)),
    class_label = reactable::colDef(name = "Category", width = 140),
    class_score = reactable::colDef(name = "Category confidence", width = 160, format = reactable::colFormat(digits = 2)),
    kw_total = reactable::colDef(name = "Keywords", width = 90, align = "center"),
    selected = reactable::colDef(name = "Pre-selected", width = 120),
    selected_model = reactable::colDef(name = "Model", width = 100),
    n_attachments = reactable::colDef(name = "Attachments", width = 100, align = "center")
  )
  if (!has_model) cols$selected_model <- NULL
  if (isTRUE(blind)) {
    for (column in c("cosine_max", "class_label", "class_score", "kw_total", "selected", "selected_model")) {
      cols[[column]] <- NULL
    }
  }
  present <- intersect(names(cols), names(data))
  show <- c("decision", present[present != "decision"])
  for (column in setdiff(names(data), show)) cols[[column]] <- reactable::colDef(show = FALSE)
  cols <- cols[intersect(names(cols), names(data))]
  reactable::reactable(
    data,
    columns = cols,
    selection = "multiple",
    onClick = "expand",
    details = function(index) curation_app_record_detail(data[index, , drop = FALSE], config),
    searchable = TRUE,
    filterable = TRUE,
    resizable = TRUE,
    defaultPageSize = 25,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(25, 50, 100, 250),
    compact = TRUE,
    highlight = TRUE,
    defaultColDef = reactable::colDef(headerStyle = list(whiteSpace = "normal"))
  )
}

curation_app_single_record_ui <- function(row, config) {
  if (is.null(row) || nrow(row) == 0L) {
    return(shiny::helpText("Draw a sample to begin."))
  }
  tr <- curation_app_translate(config, row)
  has <- curation_app_has_text
  val <- function(x) if (length(x) == 1L && !is.na(x) && nzchar(trimws(as.character(x)))) as.character(x) else NULL
  country <- as.character(curation_app_column_value(row, "country", ""))
  document_id <- as.character(curation_app_column_value(row, "document_id", ""))
  title <- curation_app_column_value(row, "title")
  summary <- curation_app_column_value(row, "summary")
  url <- curation_app_column_value(row, "url")
  meta <- list(
    Date = val(curation_app_column_value(row, "date_decision") %||% curation_app_column_value(row, "year")),
    Category = val(curation_app_column_value(row, "native_type")),
    Status = val(curation_app_column_value(row, "status")),
    Authority = val(curation_app_column_value(row, "competent_authority")),
    Proponent = val(curation_app_column_value(row, "proponent")),
    Jurisdiction = val(curation_app_column_value(row, "jurisdiction"))
  )
  meta <- meta[!vapply(meta, is.null, logical(1))]
  meta_ui <- if (length(meta) == 0L) {
    NULL
  } else {
    htmltools::div(
      style = "display:flex;gap:8px;flex-wrap:wrap;margin-bottom:12px;",
      lapply(names(meta), function(name) {
        htmltools::span(
          paste0(name, ": ", meta[[name]]),
          style = "background:#eef2f5;border-radius:10px;padding:3px 10px;font-size:12px;"
        )
      })
    )
  }
  text_or <- function(value, fallback) if (has(value)) as.character(value) else fallback
  htmltools::div(
    style = "font-size:15px;line-height:1.5;",
    htmltools::div(
      style = "color:#777;font-size:12px;margin-bottom:8px;",
      sprintf("ID: %s \u00b7 %s", document_id, toupper(country))
    ),
    meta_ui,
    htmltools::div(
      style = "display:flex;gap:24px;flex-wrap:wrap;",
      htmltools::div(
        style = "flex:1;min-width:320px;",
        htmltools::tags$b("Original"),
        htmltools::tags$h4(text_or(title, "(no title)")),
        htmltools::tags$p(text_or(summary, "(no summary)"), style = "white-space:pre-wrap;")
      ),
      htmltools::div(
        style = "flex:1;min-width:320px;",
        htmltools::tags$b("English"),
        if (tr$status == "disabled") {
          htmltools::tags$p("Translation disabled; original text shown.", style = "color:#888;")
        } else {
          htmltools::tagList(
            htmltools::tags$h4(text_or(tr$title_en, "(no title)")),
            htmltools::tags$p(text_or(tr$summary_en, "(no summary)"), style = "white-space:pre-wrap;")
          )
        }
      )
    ),
    if (has(url)) htmltools::tags$a(href = url, target = "_blank", "Open source page \u2197")
  )
}

curation_app_ui <- function(config) {
  curation_app_require_ui()
  selection_defaults <- config$selection_controls
  default_value <- function(name, fallback) {
    value <- selection_defaults[[name]]
    if (is.null(value) || length(value) != 1L || is.na(value)) fallback else value
  }
  default_labels <- as.character(selection_defaults$nonrenewable %||% character(0))
  learner_choices <- tryCatch(
    curation_selection_learner_choices(),
    error = function(e) c("Logistic regression" = "logistic")
  )
  if (length(learner_choices) == 0L) {
    learner_choices <- c("Logistic regression" = "logistic")
  }
  bslib <- asNamespace("bslib")
  shiny <- asNamespace("shiny")
  bslib$page_navbar(
    title = config$branding$title,
    id = "nav",
    theme = bslib$bs_theme(version = 5, primary = config$branding$primary),
    header = htmltools::tagList(
      htmltools::tags$style(htmltools::HTML(
        ".curation-decision{border:1px solid #ccc;background:#fff;border-radius:4px;",
        "font-size:11px;padding:2px 7px;cursor:pointer;line-height:1.5;}",
        ".curation-decision.active{background:var(--curation-color);color:#fff;}",
        ".curation-decision-group{display:flex;gap:3px;}",
        ".curation-stat{font-variant-numeric:tabular-nums;font-size:1.35rem;font-weight:600;}",
        ".curation-comparison{width:100%;border-collapse:collapse;}",
        ".curation-comparison td,.curation-comparison th{padding:7px;border-bottom:1px solid #eee;}"
      )),
      htmltools::tags$script(htmltools::HTML(
        "window.__curationReviewState = window.__curationReviewState || {};",
        "window.curationReviewSet = function(id, country, decision, source, reviewer) {",
        "  var key = String(country) + '::' + String(id) + '::' + String(reviewer || window.__curationReviewerId || '');",
        "  var current = window.__curationReviewState[key];",
        "  var next = current === decision ? null : decision;",
        "  window.__curationReviewState[key] = next;",
        "  if (window.Shiny) window.Shiny.setInputValue('row_decision', {id:id, country:country, decision:next, source:source, nonce:Date.now() + Math.random()}, {priority:'event'});",
        "};",
        "window.__curationRefreshDecisionButtons = function() {",
        "  document.querySelectorAll('[data-review-id]').forEach(function(group) {",
        "    var value = window.__curationReviewState[group.getAttribute('data-review-id')];",
        "    group.querySelectorAll('button[data-decision]').forEach(function(button) {",
        "      button.classList.toggle('active', button.getAttribute('data-decision') === value);",
        "    });",
        "  });",
        "};",
        "if (window.Shiny) {",
        "  window.Shiny.addCustomMessageHandler('curationSetReviewer', function(message) { window.__curationReviewerId = message.id || ''; });",
        "  window.Shiny.addCustomMessageHandler('curationBulkDecision', function(message) {",
        "    (message.keys || []).forEach(function(key) { window.__curationReviewState[key] = message.decision || null; });",
        "    window.__curationRefreshDecisionButtons();",
        "  });",
        "}",
        "document.addEventListener('click', function(event) {",
        "  var button = event.target && event.target.closest ? event.target.closest('button.curation-decision') : null;",
        "  if (!button) return;",
        "  var group = button.closest('.curation-decision-group');",
        "  if (!group) return;",
        "  event.stopPropagation();",
        "  window.curationReviewSet(group.getAttribute('data-record-id'), group.getAttribute('data-country'), button.getAttribute('data-decision'), group.getAttribute('data-source'), group.getAttribute('data-reviewer'));",
        "});",
        "document.addEventListener('keydown', function(event) {",
        "  var tag = (event.target && event.target.tagName || '').toLowerCase();",
        "  if (tag === 'input' || tag === 'textarea' || tag === 'select' || event.altKey || event.ctrlKey || event.metaKey) return;",
        "  if (!window.Shiny || !document.querySelector('input[name=rnd_view][value=single]:checked')) return;",
        "  if (event.key === 'ArrowRight') { event.preventDefault(); window.Shiny.setInputValue('sr_next', Date.now() + Math.random(), {priority:'event'}); }",
        "  if (event.key === 'ArrowLeft') { event.preventDefault(); window.Shiny.setInputValue('sr_prev', Date.now() + Math.random(), {priority:'event'}); }",
        "});"
      ))
    ),
    sidebar = bslib$sidebar(
      width = 300,
      shiny$uiOutput("actor_info"),
      shiny$selectInput("countries", "Countries", choices = character(0), multiple = TRUE),
      shiny$uiOutput("country_labels_summary"),
      shiny$actionButton("show_label_counts", "Label counts by country", class = "btn-outline-secondary btn-sm"),
      shiny$conditionalPanel(
        condition = "input.nav == 'Overview'",
        bslib$accordion(
          open = FALSE,
          bslib$accordion_panel(
            "Heuristic knobs",
            shiny$sliderInput("threshold", "Cosine threshold", 0, 1, as.numeric(default_value("relevance_threshold", 0.5)), 0.01),
            shiny$sliderInput("kw_min", "Min keyword hits", 0, 6, as.numeric(default_value("kw_min", 2)), 1),
            shiny$checkboxInput("use_classifier", "Use classifier arm", isTRUE(default_value("use_classifier", TRUE))),
            shiny$checkboxInput("use_trim", "Trim confident non-renewables", isTRUE(default_value("use_trim", TRUE))),
            shiny$conditionalPanel(
              "input.use_trim == true",
              shiny$selectizeInput("nonrenewable", "Excluded labels", choices = names(config$labels), selected = default_labels, multiple = TRUE),
              shiny$sliderInput("nonrenewable_score", "Excluded confidence", 0, 1, as.numeric(default_value("nonrenewable_score", 0.5)), 0.01)
            )
          )
        )
      ),
      htmltools::tags$hr(),
      shiny$selectInput("learner", "Selection model", choices = learner_choices, selected = unname(learner_choices[[1L]])),
      shiny$actionButton("train_model", "Train selection model", class = "btn-outline-primary btn-sm"),
      shiny$uiOutput("model_info"),
      shiny$sliderInput("model_threshold", "Model decision threshold", 0, 1, 0.5, 0.01),
      shiny$actionButton("compute_lc", "Compute learning curve", class = "btn-primary btn-sm"),
      plotly::plotlyOutput("learning_curve_plot", height = "260px"),
      htmltools::tags$hr(),
      shiny$actionButton("rebuild", "Rebuild snapshot", class = "btn-outline-secondary btn-sm"),
      shiny$uiOutput("snapshot_info")
    ),
    bslib$nav_panel(
      "Overview",
      bslib$layout_columns(
        bslib$card(htmltools::div(class = "curation-stat", shiny$textOutput("vb_total", inline = TRUE)), bslib$card_header("Indexed")),
        bslib$card(htmltools::div(class = "curation-stat", shiny$textOutput("vb_selected", inline = TRUE)), bslib$card_header("Selected (heuristic)")),
        bslib$card(htmltools::div(class = "curation-stat", shiny$textOutput("vb_selected_model", inline = TRUE)), bslib$card_header("Selected (model)")),
        bslib$card(htmltools::div(class = "curation-stat", shiny$textOutput("vb_reviewed", inline = TRUE)), bslib$card_header("Reviewed")),
        col_widths = c(3, 3, 3, 3)
      ),
      bslib$layout_columns(
        bslib$card(bslib$card_header("Pipeline funnel"), plotly::plotlyOutput("funnel_plot", height = "360px")),
        bslib$card(bslib$card_header("Per-country breakdown"), reactable::reactableOutput("funnel_table")),
        col_widths = c(7, 5)
      ),
      bslib$card(
        bslib$card_header(
          htmltools::div(
            class = "d-flex justify-content-between align-items-center",
            htmltools::span("Automated selection vs. human review"),
            shiny$actionButton("show_perf_country", "Performance by country", class = "btn-outline-secondary btn-sm")
          )
        ),
        shiny$radioButtons("eval_on", NULL, inline = TRUE, choices = c("All reviews" = "all", "Random sample only (unbiased)" = "random"), selected = "all"),
        shiny$uiOutput("agreement_ui")
      )
    ),
    bslib$nav_panel(
      "Review",
      bslib$card(
        bslib$card_header("Triage"),
        htmltools::div(
          class = "d-flex align-items-end flex-wrap gap-2",
          shiny$selectInput(
            "stage_filter", "Show",
            choices = c("All" = "all", "Heuristic-selected" = "selected", "Heuristic not selected" = "not_selected", "Model-selected" = "model_selected", "Model not selected" = "model_not_selected", "Heuristic vs Model differ" = "model_vs_heuristic", "Reviewed" = "reviewed", "Unreviewed" = "unreviewed", "Disagreements (auto vs human)" = "disagree"),
            selected = "model_selected", width = "300px"
          ),
          shiny$actionButton("mark_keep", "Keep", class = "btn-success btn-sm"),
          shiny$actionButton("mark_drop", "Drop", class = "btn-danger btn-sm"),
          shiny$actionButton("mark_unsure", "Unsure", class = "btn-warning btn-sm"),
          shiny$actionButton("mark_clear", "Clear", class = "btn-outline-secondary btn-sm"),
          shiny$textOutput("review_info", inline = TRUE)
        ),
        shiny$helpText("Select rows for bulk actions. Click a row to expand the original and optional translated text."),
        reactable::reactableOutput("review_tbl")
      )
    ),
    bslib$nav_panel(
      "Random review",
      bslib$card(
        bslib$card_header("Random sample \u2014 unbiased review"),
        htmltools::div(
          class = "d-flex align-items-end flex-wrap gap-3",
          shiny$numericInput("rnd_n", "Records / country", 50, min = 1, step = 10),
          shiny$numericInput("rnd_seed", "Seed (optional)", NA, min = 0),
          shiny$actionButton("rnd_draw", "Build queue", class = "btn-primary"),
          shiny$checkboxInput("rnd_blind", "Blind (hide pipeline signals)", TRUE),
          shiny$radioButtons("rnd_view", NULL, choices = c("One at a time" = "single", "Table" = "table"), selected = "single", inline = TRUE),
          shiny$textOutput("rnd_info", inline = TRUE),
          shiny$uiOutput("warm_status")
        ),
        shiny$helpText("Other reviewers' records are queued first; fresh records are then sampled by country. Bulk and stepper decisions retain random provenance."),
        shiny$conditionalPanel(
          "input.rnd_view == 'table'",
          htmltools::div(
            shiny$actionButton("rnd_keep", "Keep", class = "btn-success btn-sm"),
            shiny$actionButton("rnd_drop", "Drop", class = "btn-danger btn-sm"),
            shiny$actionButton("rnd_unsure", "Unsure", class = "btn-warning btn-sm"),
            shiny$actionButton("rnd_clear", "Clear", class = "btn-outline-secondary btn-sm")
          ),
          reactable::reactableOutput("random_tbl")
        ),
        shiny$conditionalPanel(
          "input.rnd_view == 'single'",
          htmltools::div(
            shiny$actionButton("sr_prev", "\u2190 Previous", class = "btn-outline-secondary btn-sm"),
            shiny$actionButton("sr_next", "Next \u2192", class = "btn-outline-secondary btn-sm"),
            shiny$checkboxInput("sr_skip", "Skip already-classified", TRUE),
            shiny$textOutput("sr_pos", inline = TRUE)
          ),
          shiny$uiOutput("sr_record"),
          shiny$uiOutput("sr_decision")
        )
      )
    )
  )
}

curation_app_read_reviews <- function(repository, scope, actor) {
  rows <- curation_review_list(
    repository,
    scope,
    filter = list(),
    actor = actor
  )
  curation_app_normalize_reviews(rows)
}

curation_app_read_model <- function(store, scope, actor, config) {
  active <- tryCatch(
    curation_artifact_active_model(store, scope, actor),
    error = function(e) NULL
  )
  if (is.null(active)) {
    return(NULL)
  }
  if (inherits(active, "planscanR_selection_model")) {
    return(active)
  }
  payload <- active$model %||% active$payload
  if (inherits(payload, "planscanR_selection_model")) {
    return(payload)
  }
  model_id <- active$active_model_id %||% active$model_id
  if (is.null(model_id)) {
    return(NULL)
  }
  loaded <- tryCatch(
    curation_artifact_read_model(store, scope, model_id, actor),
    error = function(e) NULL
  )
  if (is.null(loaded)) {
    return(NULL)
  }
  loaded$model %||% loaded$payload %||% loaded
}

curation_app_read_sample <- function(store, scope, actor) {
  listed <- tryCatch(
    curation_artifact_list_samples(store, scope, reviewer_id = actor$id, actor = actor),
    error = function(e) NULL
  )
  if (is.null(listed) || !is.data.frame(listed) || nrow(listed) == 0L) {
    return(list(id = NULL, revision = 0L, queue = tibble::tibble(document_id = character(0), country = character(0))))
  }
  id_column <- intersect(c("sample_id", "id"), names(listed))
  if (length(id_column) == 0L) {
    return(list(id = NULL, revision = 0L, queue = tibble::tibble(document_id = character(0), country = character(0))))
  }
  id <- as.character(listed[[id_column[[1L]]]][[1L]])
  revision <- if ("revision" %in% names(listed)) as.integer(listed$revision[[1L]]) else 0L
  read <- tryCatch(curation_artifact_read_sample(store, scope, id, actor), error = function(e) NULL)
  payload <- if (is.null(read)) NULL else read$payload %||% read$sample %||% read
  queue <- if (is.list(payload) && is.data.frame(payload$queue)) payload$queue else if (is.data.frame(payload)) payload else NULL
  if (is.null(queue) || !all(c("country", "document_id") %in% names(queue))) {
    queue <- tibble::tibble(document_id = character(0), country = character(0))
  }
  list(id = id, revision = revision, queue = queue)
}

curation_app_read_snapshot <- function(store, scope, actor) {
  snapshot <- tryCatch(
    curation_artifact_read_snapshot(store, scope, actor),
    error = function(e) NULL
  )
  if (is.null(snapshot)) {
    return(list(records = NULL, id = NULL, version = NULL, revision = 0L))
  }
  payload <- snapshot$payload %||% snapshot$snapshot %||% snapshot
  records <- if (is.list(payload) && is.data.frame(payload$records)) payload$records else if (is.data.frame(payload)) payload else NULL
  list(
    records = records,
    id = snapshot$snapshot_id %||% snapshot$id %||% if (is.list(payload)) payload$snapshot_id else NULL,
    version = snapshot$version %||% if (is.list(payload)) payload$version else NULL,
    revision = as.integer(snapshot$revision %||% if (is.list(payload)) payload$revision else 0L)
  )
}

curation_app_funnel <- function(records, config, context = list(), by_country = FALSE) {
  one <- function(data) {
    result <- curation_compute_funnel(config, data, context = context)
    result$country <- if (nrow(data) == 0L) "all" else as.character(data$country[[1L]])
    result
  }
  if (!isTRUE(by_country) || nrow(records) == 0L) {
    result <- curation_compute_funnel(config, records, context = context)
    result$country <- "all"
    return(result)
  }
  dplyr::bind_rows(lapply(split(records, records$country), one))
}

curation_app_funnel_plot <- function(funnel) {
  if (is.null(funnel) || nrow(funnel) == 0L) {
    return(plotly::plot_ly(type = "bar") |> plotly::layout(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(text = "No data", showarrow = FALSE, x = 0.5, y = 0.5, xref = "paper", yref = "paper")
    ) |> plotly::config(displayModeBar = FALSE))
  }
  d <- funnel[funnel$country == "all", , drop = FALSE]
  if (nrow(d) == 0L) d <- funnel
  d$stage <- factor(d$stage, levels = rev(unique(d$stage[order(d$order)])))
  d$label <- sprintf("%s (%.1f%%)", format(d$n, big.mark = ",", trim = TRUE), d$pct)
  d$hover <- sprintf("<b>%s</b><br>Count: %s<br>Percent: %.1f%%", d$stage, format(d$n, big.mark = ",", trim = TRUE), d$pct)
  plotly::plot_ly(
    d,
    x = ~n, y = ~stage, type = "bar", orientation = "h",
    marker = list(color = "#0e3c62"), text = ~label, textposition = "outside",
    cliponaxis = FALSE, hovertext = ~hover, hoverinfo = "text"
  ) |>
    plotly::layout(
      xaxis = list(title = "records", zeroline = FALSE),
      yaxis = list(title = "", automargin = TRUE),
      margin = list(l = 10, r = 60, t = 10, b = 10), showlegend = FALSE, bargap = 0.3
    ) |>
    plotly::config(displayModeBar = FALSE)
}

curation_app_learning_plot <- function(curve) {
  if (is.null(curve) || nrow(curve) == 0L) {
    return(plotly::plot_ly(type = "scatter", mode = "lines") |> plotly::layout(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(text = "Click Compute learning curve", showarrow = FALSE, x = 0.5, y = 0.5, xref = "paper", yref = "paper")
    ) |> plotly::config(displayModeBar = FALSE))
  }
  summary <- tryCatch(learning_curve_summary(curve), error = function(e) NULL)
  if (is.null(summary) || nrow(summary) == 0L) {
    return(curation_app_learning_plot(NULL))
  }
  summary <- summary[order(summary$n_train_used), , drop = FALSE]
  has_country <- "country" %in% names(summary)
  all_rows <- if (has_country) summary[summary$country == "all", , drop = FALSE] else summary
  plot <- plotly::plot_ly()
  if (nrow(all_rows) > 0L) {
    sd <- if ("f1_sd" %in% names(all_rows)) ifelse(is.na(all_rows$f1_sd), 0, all_rows$f1_sd) else rep(0, nrow(all_rows))
    plot <- plotly::add_trace(
      plot,
      x = all_rows$n_train_used, y = pmin(1, all_rows$f1_mean + sd),
      type = "scatter", mode = "lines", line = list(width = 0),
      showlegend = FALSE, hoverinfo = "skip"
    )
    plot <- plotly::add_trace(
      plot,
      x = all_rows$n_train_used, y = pmax(0, all_rows$f1_mean - sd),
      type = "scatter", mode = "lines", line = list(width = 0),
      fill = "tonexty", fillcolor = "rgba(14,60,98,0.16)",
      showlegend = FALSE, hoverinfo = "skip"
    )
    plot <- plotly::add_trace(
      plot,
      x = all_rows$n_train_used, y = all_rows$f1_mean,
      type = "scatter", mode = "lines+markers", name = "All",
      line = list(color = "#0e3c62", width = 2.5), marker = list(color = "#0e3c62")
    )
  }
  if (has_country) {
    countries <- setdiff(unique(as.character(summary$country)), "all")
    palette <- grDevices::hcl.colors(max(1L, length(countries)), "Dark 3")
    for (i in seq_along(countries)) {
      d <- summary[summary$country == countries[[i]], , drop = FALSE]
      plot <- plotly::add_trace(
        plot,
        x = d$n_train_used, y = d$f1_mean, type = "scatter",
        mode = "lines+markers", name = countries[[i]],
        line = list(color = palette[[i]], width = 1.2), marker = list(size = 4)
      )
    }
  }
  plotly::layout(
    plot,
    xaxis = list(title = "Number of training labels"),
    yaxis = list(title = "F1 (held-out)", range = c(0, 1)),
    legend = list(orientation = "h")
  ) |>
    plotly::config(displayModeBar = FALSE)
}

curation_app_server <- function(input, output, session, context) {
  config <- context$config
  provider <- context$records_provider
  repository <- context$review_repository
  artifact_store <- context$artifact_store
  curation_app_check_actor_resolver(context$actor_resolver)
  actor <- curation_resolve_actor(
    context$actor_resolver,
    curation_app_request_for_session(context$request, session)
  )
  scope <- config$identity
  reviewer_id <- actor$id
  session$sendCustomMessage("curationSetReviewer", list(id = reviewer_id))

  read_records <- function(refresh = FALSE) {
    curation_records_snapshot(provider, config, refresh = refresh)
  }
  snapshot_info <- curation_app_read_snapshot(artifact_store, scope, actor)
  initial_records <- if (!is.null(snapshot_info$records)) {
    snapshot_info$records
  } else {
    tryCatch(
      read_records(FALSE),
      error = function(e) {
        shiny::showNotification(paste0("Unable to load records: ", conditionMessage(e)), type = "error", duration = 8)
        curation_app_empty_records()
      }
    )
  }
  records <- shiny::reactiveVal(initial_records)
  snapshot_id <- shiny::reactiveVal(snapshot_info$id)
  snapshot_revision <- shiny::reactiveVal(snapshot_info$revision)
  snapshot_version <- shiny::reactiveVal(snapshot_info$version)
  reviews <- shiny::reactiveVal(tryCatch(
    curation_app_read_reviews(repository, scope, actor),
    error = function(e) {
      shiny::showNotification(paste0("Unable to load reviews: ", conditionMessage(e)), type = "error", duration = 8)
      curation_app_empty_reviews()
    }
  ))
  model <- shiny::reactiveVal(curation_app_read_model(artifact_store, scope, actor, config))
  sample_info <- curation_app_read_sample(artifact_store, scope, actor)
  sample <- shiny::reactiveVal(sample_info$queue)
  sample_id <- shiny::reactiveVal(sample_info$id)
  sample_revision <- shiny::reactiveVal(sample_info$revision)
  sample_index <- shiny::reactiveVal(1L)
  learning_curve <- shiny::reactiveVal(NULL)
  random_df <- shiny::reactive({
    queue <- sample()
    data <- selected_records()
    if (nrow(queue) == 0L || nrow(data) == 0L) {
      return(data[0, , drop = FALSE])
    }
    key <- curation_app_key_string(data$country, data$document_id)
    wanted <- curation_app_key_string(queue$country, queue$document_id)
    result <- data[match(wanted, key), , drop = FALSE]
    result[!is.na(result$document_id), , drop = FALSE]
  })

  output$actor_info <- shiny::renderUI({
    shiny::tagList(
      shiny::tags$div("Authenticated reviewer", style = "font-size:0.75rem;font-weight:600;text-transform:uppercase;color:#666;"),
      shiny::tags$div(actor$display_name %||% actor$id, style = "margin-bottom:6px;"),
      shiny::tags$small(paste0("Reviewer ID: ", actor$id), style = "color:#888;")
    )
  })

  countries <- shiny::reactive({
    sort(unique(as.character(records()$country)))
  })
  shiny::observe({
    cc <- countries()
    shiny::updateSelectInput(session, "countries", choices = cc, selected = cc)
  })
  selected_countries <- shiny::reactive({
    value <- input$countries
    if (is.null(value) || length(value) == 0L) countries() else as.character(value)
  })

  shiny::observeEvent(input$rebuild, {
    fresh <- tryCatch(read_records(TRUE), error = function(e) e)
    if (inherits(fresh, "error")) {
      shiny::showNotification(paste0("Snapshot refresh failed: ", conditionMessage(fresh)), type = "error", duration = 8)
      return()
    }
    if (is.null(snapshot_id())) snapshot_id(paste0("snapshot-", config$identity$config_id))
    next_version <- as.integer(snapshot_version() %||% 0L) + 1L
    written <- tryCatch(
      curation_artifact_write_snapshot(
        artifact_store,
        scope,
        snapshot = list(
          snapshot_id = snapshot_id(),
          version = next_version,
          records = fresh,
          created_at = Sys.time()
        ),
        actor = actor,
        expected_revision = snapshot_revision()
      ),
      error = function(e) e
    )
    if (inherits(written, "error") || (is.list(written) && "ok" %in% names(written) && isFALSE(written$ok))) {
      message <- if (inherits(written, "error")) conditionMessage(written) else "the artifact store rejected the write"
      shiny::showNotification(paste0("Snapshot refresh was not persisted: ", message), type = "error", duration = 8)
      return()
    }
    records(fresh)
    snapshot_version(next_version)
    if (!is.null(written$revision)) snapshot_revision(as.integer(written$revision))
    shiny::showNotification("Snapshot refreshed through the injected artifact store.", type = "message")
  })

  nonrenewable_input_seen <- shiny::reactiveVal(FALSE)
  shiny::observeEvent(input$nonrenewable,
    {
      nonrenewable_input_seen(TRUE)
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )
  selection_controls <- shiny::reactive({
    controls <- config$selection_controls
    replace_if_configured <- function(name, value) {
      has_value <- !is.null(value) && length(value) > 0L && !all(is.na(value))
      is_empty_label_set <- identical(name, "nonrenewable") && is.character(value) && length(value) == 0L
      if (name %in% names(controls) && (has_value || is_empty_label_set)) {
        controls[[name]] <<- value
      }
    }
    replace_if_configured("relevance_threshold", input$threshold)
    replace_if_configured("kw_min", as.integer(input$kw_min))
    replace_if_configured("use_classifier", if (is.null(input$use_classifier)) NULL else isTRUE(input$use_classifier))
    selected_exclusions <- if (is.null(input$use_trim)) {
      NULL
    } else if (!isTRUE(input$use_trim)) {
      character(0)
    } else if (!nonrenewable_input_seen()) {
      NULL
    } else {
      if (is.null(input$nonrenewable)) character(0) else as.character(input$nonrenewable)
    }
    replace_if_configured("use_trim", if (is.null(input$use_trim)) NULL else isTRUE(input$use_trim))
    replace_if_configured("nonrenewable", selected_exclusions)
    replace_if_configured("nonrenewable_score", input$nonrenewable_score)
    controls
  })
  model_scored <- shiny::reactive({
    data <- records()
    fitted <- model()
    if (is.null(fitted)) {
      data$select_prob <- rep(NA_real_, nrow(data))
      data$selected_model <- rep(FALSE, nrow(data))
      return(data)
    }
    tryCatch(
      curation_predict_model(fitted, data, config, threshold = input$model_threshold %||% 0.5),
      error = function(e) {
        shiny::showNotification(paste0("Model prediction failed: ", conditionMessage(e)), type = "warning")
        data$select_prob <- rep(NA_real_, nrow(data))
        data$selected_model <- rep(FALSE, nrow(data))
        data
      }
    )
  })
  selected_records <- shiny::reactive({
    data <- curation_apply_selection(config, model_scored(), selection_controls())
    if (!"selected_model" %in% names(data)) data$selected_model <- rep(FALSE, nrow(data))
    data
  })
  countries_debounced <- shiny::debounce(shiny::reactive(selected_countries()), 500)
  filtered <- shiny::reactive({
    data <- selected_records()
    data[data$country %in% countries_debounced(), , drop = FALSE]
  })

  active_reviews <- shiny::reactive({
    curation_app_active_reviews(reviews())
  })
  current_reviewed_keys <- shiny::reactive({
    rv <- active_reviews()
    rv <- rv[rv$reviewer_id == reviewer_id, , drop = FALSE]
    if (nrow(rv) == 0L) character(0) else unique(curation_app_key_string(rv$country, rv$document_id))
  })

  output$country_labels_summary <- shiny::renderUI({
    counts <- curation_country_label_counts(
      records(), reviews(), countries(),
      reviewer_id = reviewer_id, scope = scope
    )
    htmltools::tags$table(
      style = "width:100%;font-size:12px;border-collapse:collapse;margin:6px 0;",
      htmltools::tags$tbody(lapply(seq_len(nrow(counts)), function(i) {
        htmltools::tags$tr(
          htmltools::tags$td(toupper(counts$country[[i]])),
          htmltools::tags$td(sprintf("%s / %s", counts$labelled[[i]], counts$records[[i]]), style = "text-align:right;")
        )
      }))
    )
  })
  output$label_counts_tbl <- reactable::renderReactable({
    counts <- curation_country_label_counts(records(), reviews(), countries(), reviewer_id = reviewer_id, scope = scope)
    reactable::reactable(counts, compact = TRUE, pagination = FALSE)
  })
  shiny::observeEvent(input$show_label_counts, {
    shiny::showModal(shiny::modalDialog(
      title = "Label counts by country", size = "l", reactable::reactableOutput("label_counts_tbl"),
      footer = shiny::modalButton("Close")
    ))
  })

  output$model_info <- shiny::renderUI({
    fitted <- model()
    if (is.null(fitted)) {
      return(shiny::helpText("No model published yet."))
    }
    shiny::helpText(sprintf("Model: %s \u00b7 %d labels", fitted$learner_name %||% "selection", fitted$n_train %||% NA_integer_))
  })
  output$learning_curve_plot <- plotly::renderPlotly({
    curation_app_learning_plot(learning_curve())
  })

  shiny::observeEvent(input$train_model, {
    fitted <- tryCatch(
      shiny::withProgress(message = "Training selection model (cross-validating)...", value = 0.5, {
        curation_train_model(
          records(), reviews(), config,
          learner_key = input$learner %||% "logistic",
          eval_source = "random"
        )
      }),
      error = function(e) e
    )
    if (inherits(fitted, "error")) {
      shiny::showNotification(paste0("Training failed: ", conditionMessage(fitted)), type = "error", duration = 8)
      return()
    }
    model(fitted)
    # Model publication is the only artifact write here.  The app never writes
    # an RDS path or a session-wide review frame.
    tryCatch(
      {
        listed <- curation_artifact_list_models(artifact_store, scope, actor)
        expected_model_revision <- if (is.data.frame(listed) && nrow(listed) > 0L && "revision" %in% names(listed)) max(as.integer(listed$revision)) else 0L
        active <- curation_artifact_active_model(artifact_store, scope, actor)
        expected_active_revision <- if (is.null(active)) 0L else as.integer(active$active_revision %||% active$revision %||% 0L)
        curation_publish_model(
          artifact_store, scope, fitted,
          provenance = list(
            reviewer_id = reviewer_id,
            eval_source = "random",
            config_id = config$identity$config_id,
            trained_at = Sys.time()
          ),
          actor = actor,
          expected_revision = expected_model_revision,
          expected_active_revision = expected_active_revision,
          config = config
        )
      },
      error = function(e) {
        shiny::showNotification(paste0("Model trained but could not be published: ", conditionMessage(e)), type = "warning", duration = 8)
      }
    )
    shiny::showNotification("Selection model trained.", type = "message")
  })
  shiny::observeEvent(input$compute_lc, {
    result <- tryCatch(
      shiny::withProgress(message = "Computing learning curve...", value = 0.5, {
        curation_learning_curve(records(), reviews(), config, learner_key = input$learner %||% "logistic", by_country = TRUE)
      }),
      error = function(e) e
    )
    if (inherits(result, "error")) {
      shiny::showNotification(paste0("Learning curve failed: ", conditionMessage(result)), type = "error", duration = 8)
    } else {
      learning_curve(result)
    }
  })

  output$vb_total <- shiny::renderText(format(nrow(filtered()), big.mark = ","))
  output$vb_selected <- shiny::renderText({
    data <- filtered()
    n <- sum(data$selected %in% TRUE)
    if (nrow(data) == 0L) "\u2014" else sprintf("%s (%.1f%%)", format(n, big.mark = ","), 100 * n / nrow(data))
  })
  output$vb_selected_model <- shiny::renderText({
    data <- filtered()
    if (nrow(data) == 0L || !any(!is.na(data$select_prob))) {
      return("\u2014")
    }
    n <- sum(data$selected_model %in% TRUE)
    sprintf("%s (%.1f%%)", format(n, big.mark = ","), 100 * n / nrow(data))
  })
  output$vb_reviewed <- shiny::renderText({
    data <- filtered()
    if (nrow(data) == 0L) {
      return("\u2014")
    }
    n <- sum(curation_app_key_string(data$country, data$document_id) %in% current_reviewed_keys())
    sprintf("%s (%.1f%%)", format(n, big.mark = ","), 100 * n / nrow(data))
  })

  funnel_context <- shiny::reactive(list(
    model = model(),
    has_model = any(!is.na(filtered()$select_prob)),
    reviews = active_reviews(),
    reviewer_id = reviewer_id,
    scope = scope
  ))
  output$funnel_plot <- plotly::renderPlotly({
    curation_app_funnel_plot(curation_app_funnel(filtered(), config, context = funnel_context(), by_country = FALSE))
  })
  output$funnel_table <- reactable::renderReactable({
    data <- curation_app_funnel(filtered(), config, context = funnel_context(), by_country = TRUE)
    reactable::reactable(data[, intersect(c("country", "stage", "n", "pct"), names(data)), drop = FALSE], groupBy = "country", compact = TRUE, pagination = FALSE)
  })

  display_df <- shiny::reactive({
    data <- filtered()
    rv <- active_reviews()
    keys <- curation_app_key_string(data$country, data$document_id)
    f <- input$stage_filter %||% "all"
    data <- switch(f,
      selected = data[data$selected %in% TRUE, , drop = FALSE],
      not_selected = data[!(data$selected %in% TRUE), , drop = FALSE],
      model_selected = data[data$selected_model %in% TRUE, , drop = FALSE],
      model_not_selected = data[!(data$selected_model %in% TRUE), , drop = FALSE],
      model_vs_heuristic = data[(data$selected %in% TRUE) != (data$selected_model %in% TRUE), , drop = FALSE],
      reviewed = data[keys %in% current_reviewed_keys(), , drop = FALSE],
      unreviewed = data[!keys %in% current_reviewed_keys(), , drop = FALSE],
      disagree = {
        agreed <- tryCatch(curation_selection_vs_human(data, rv, config = config, eval_source = NULL), error = function(e) NULL)
        if (is.null(agreed)) {
          data[0, , drop = FALSE]
        } else {
          labels <- stats::setNames(rv$decision, curation_app_key_string(rv$country, rv$document_id))
          human <- labels[curation_app_key_string(data$country, data$document_id)]
          auto <- data$selected %in% TRUE
          reviewed <- !is.na(human)
          data[reviewed & ((auto & human == "drop") | (!auto & human == "keep")), , drop = FALSE]
        }
      },
      data
    )
    cosine <- data$cosine_max
    cosine[is.na(cosine)] <- -1
    data[order(-(data$selected %in% TRUE), -cosine), , drop = FALSE]
  })
  display_capped <- shiny::reactive({
    data <- display_df()
    if (nrow(data) > 5000L) {
      attr(data, "truncated") <- nrow(data)
      data <- data[seq_len(5000L), , drop = FALSE]
    }
    data
  })
  output$review_info <- shiny::renderText({
    data <- display_capped()
    total <- attr(data, "truncated")
    if (is.null(total)) sprintf("%d rows", nrow(data)) else sprintf("showing %d of %d (capped)", nrow(data), total)
  })
  output$review_tbl <- reactable::renderReactable({
    curation_app_table(display_capped(), reviews(), config, source = "browse", reviewer_id = reviewer_id)
  })

  write_one <- function(country, document_id, decision, source) {
    available <- records()
    valid_record <- nrow(available) > 0L && any(
      as.character(available$country) == as.character(country) &
        as.character(available$document_id) == as.character(document_id)
    )
    if (!valid_record) {
      curation_abort(
        "The review event did not identify a record in the current snapshot.",
        class = "planscanR_error_curation_repository"
      )
    }
    expected <- curation_app_current_revision(reviews(), country, document_id, reviewer_id)
    key <- curation_review_key(country, document_id, reviewer_id)
    if (is.null(decision)) {
      result <- curation_review_clear(repository, scope, key, actor, expected)
      decision_row <- NULL
    } else {
      decision_row <- data.frame(
        country = as.character(country), document_id = as.character(document_id), reviewer_id = reviewer_id,
        decision = as.character(decision), source = as.character(source), note = NA_character_,
        reviewed_at = as.POSIXct(Sys.time(), tz = "UTC"), sidecar_path = NA_character_,
        stringsAsFactors = FALSE
      )
      result <- curation_review_upsert(repository, scope, decision_row, actor, expected)
    }
    reviews(curation_app_replace_review_result(reviews(), decision_row, result, actor))
    result
  }
  safe_write_one <- function(country, document_id, decision, source) {
    tryCatch(
      write_one(country, document_id, decision, source),
      error = function(e) {
        shiny::showNotification(paste0("Review write failed: ", conditionMessage(e)), type = "error", duration = 8)
        NULL
      }
    )
  }
  shiny::observeEvent(input$row_decision,
    {
      info <- input$row_decision
      if (is.null(info) || is.null(info$id) || is.null(info$country)) {
        return()
      }
      decision <- if (is.null(info$decision)) NULL else as.character(info$decision)
      source <- if (is.null(info$source)) "browse" else as.character(info$source)
      safe_write_one(info$country, info$id, decision, source)
    },
    ignoreInit = TRUE
  )

  apply_bulk <- function(decision, output_id, data, source) {
    index <- tryCatch(reactable::getReactableState(output_id, "selected"), error = function(e) integer(0))
    index <- as.integer(index)
    index <- index[is.finite(index) & index >= 1L & index <= nrow(data)]
    if (length(index) == 0L) {
      shiny::showNotification("No rows ticked.", type = "warning")
      return(invisible(NULL))
    }
    selected <- data[index, c("country", "document_id"), drop = FALSE]
    if (is.null(decision)) {
      result <- tryCatch(
        {
          out <- vector("list", nrow(selected))
          for (i in seq_len(nrow(selected))) {
            out[[i]] <- write_one(selected$country[[i]], selected$document_id[[i]], NULL, source)
          }
          out
        },
        error = function(e) e
      )
      if (inherits(result, "error")) {
        shiny::showNotification(paste0("Bulk clear failed: ", conditionMessage(result)), type = "error", duration = 8)
        return(invisible(NULL))
      }
      keys <- curation_app_key_string(selected$country, selected$document_id, reviewer_id)
    } else {
      decisions <- data.frame(
        country = as.character(selected$country), document_id = as.character(selected$document_id), reviewer_id = reviewer_id,
        decision = rep(as.character(decision), nrow(selected)), source = rep(as.character(source), nrow(selected)),
        note = NA_character_, reviewed_at = rep(as.POSIXct(Sys.time(), tz = "UTC"), nrow(selected)),
        sidecar_path = NA_character_, stringsAsFactors = FALSE
      )
      expected <- vapply(seq_len(nrow(decisions)), function(i) curation_app_current_revision(reviews(), decisions$country[[i]], decisions$document_id[[i]], reviewer_id), integer(1))
      result <- tryCatch(curation_review_bulk_upsert(repository, scope, decisions, actor, expected), error = function(e) e)
      if (inherits(result, "error")) {
        shiny::showNotification(paste0("Bulk write failed: ", conditionMessage(result)), type = "error", duration = 8)
        return(invisible(NULL))
      }
      result_rows <- result$results
      for (i in seq_len(nrow(decisions))) {
        one_result <- list(
          key = curation_review_key(decisions$country[[i]], decisions$document_id[[i]], reviewer_id),
          revision = result_rows$revision[[i]], deleted = result_rows$deleted[[i]]
        )
        reviews(curation_app_replace_review_result(reviews(), decisions[i, , drop = FALSE], one_result, actor))
      }
      keys <- curation_app_key_string(decisions$country, decisions$document_id, reviewer_id)
    }
    session$sendCustomMessage("curationBulkDecision", list(keys = as.list(keys), decision = decision))
    try(reactable::updateReactable(output_id, selected = NA), silent = TRUE)
    shiny::showNotification(sprintf("%d record(s) updated.", length(index)), type = "message")
    invisible(NULL)
  }
  shiny::observeEvent(input$mark_keep, apply_bulk("keep", "review_tbl", display_capped(), "browse"))
  shiny::observeEvent(input$mark_drop, apply_bulk("drop", "review_tbl", display_capped(), "browse"))
  shiny::observeEvent(input$mark_unsure, apply_bulk("unsure", "review_tbl", display_capped(), "browse"))
  shiny::observeEvent(input$mark_clear, apply_bulk(NULL, "review_tbl", display_capped(), "browse"))

  output$agreement_ui <- shiny::renderUI({
    agreement <- tryCatch(curation_inter_reviewer_summary(reviews(), config), error = function(e) NULL)
    agreement_ui <- if (is.null(agreement)) NULL else shiny::helpText(sprintf("Cross-reviewer agreement: %.0f%% \u2014 %d of %d records.", 100 * agreement$agreement_rate, agreement$n_agree, agreement$n_multi))
    eval_source <- if (identical(input$eval_on, "random")) "random" else NULL
    heuristic <- tryCatch(curation_selection_vs_human(filtered(), reviews(), config, eval_source = eval_source), error = function(e) NULL)
    fitted <- model()
    model_metrics <- if (is.null(fitted)) NULL else tryCatch(curation_model_metrics(fitted, config, input$model_threshold), error = function(e) NULL)
    if (is.null(heuristic)) {
      return(shiny::tagList(agreement_ui, shiny::helpText("No agreed labels for this view yet.")))
    }
    fmt <- function(value) if (is.null(value) || is.na(value)) "\u2014" else sprintf("%.2f", value)
    shiny::tagList(
      agreement_ui,
      htmltools::tags$table(
        class = "curation-comparison",
        htmltools::tags$thead(htmltools::tags$tr(htmltools::tags$th("Method"), htmltools::tags$th("Reviewed"), htmltools::tags$th("Precision \u00b7 Recall \u00b7 F1"), htmltools::tags$th("TP / FP / FN / TN"))),
        htmltools::tags$tbody(
          htmltools::tags$tr(htmltools::tags$td("Heuristic"), htmltools::tags$td(heuristic$n_reviewed), htmltools::tags$td(sprintf("%s \u00b7 %s \u00b7 %s", fmt(heuristic$precision), fmt(heuristic$recall), fmt(heuristic$f1))), htmltools::tags$td(sprintf("%d / %d / %d / %d", heuristic$tp, heuristic$fp, heuristic$fn, heuristic$tn))),
          htmltools::tags$tr(htmltools::tags$td("Model"), htmltools::tags$td(if (is.null(model_metrics)) "\u2014" else model_metrics$n_reviewed), htmltools::tags$td(if (is.null(model_metrics)) "\u2014" else sprintf("%s \u00b7 %s \u00b7 %s", fmt(model_metrics$precision), fmt(model_metrics$recall), fmt(model_metrics$f1))), htmltools::tags$td(if (is.null(model_metrics)) "\u2014" else sprintf("%d / %d / %d / %d", model_metrics$tp, model_metrics$fp, model_metrics$fn, model_metrics$tn)))
        )
      )
    )
  })
  output$perf_by_country <- reactable::renderReactable({
    heur <- tryCatch(curation_selection_vs_human(filtered(), reviews(), config, eval_source = "random", by_country = TRUE), error = function(e) NULL)
    if (is.null(heur)) {
      return(reactable::reactable(data.frame(Note = "No random-sample labels yet."), pagination = FALSE))
    }
    heur$method <- "Heuristic"
    fitted <- model()
    model_metrics <- if (is.null(fitted)) {
      NULL
    } else {
      tryCatch(
        curation_model_metrics(fitted, config, input$model_threshold, by_country = TRUE),
        error = function(e) NULL
      )
    }
    if (!is.null(model_metrics)) model_metrics$method <- "Model"
    columns <- c("method", "country", setdiff(names(heur), c("method", "country")))
    combined <- dplyr::bind_rows(heur, model_metrics)
    combined <- combined[, intersect(columns, names(combined)), drop = FALSE]
    reactable::reactable(combined, compact = TRUE, pagination = FALSE)
  })
  shiny::observeEvent(input$show_perf_country, {
    shiny::showModal(shiny::modalDialog(title = "Performance by country", size = "l", reactable::reactableOutput("perf_by_country"), footer = shiny::modalButton("Close")))
  })

  output$random_tbl <- reactable::renderReactable({
    curation_app_table(random_df(), reviews(), config, source = "random", blind = isTRUE(input$rnd_blind), reviewer_id = reviewer_id)
  })
  output$warm_status <- shiny::renderUI({
    if (is.null(config$translator)) shiny::helpText("Translation provider disabled.") else shiny::helpText("Translations are supplied by the injected provider.")
  })
  output$rnd_info <- shiny::renderText({
    queue <- sample()
    if (nrow(queue) == 0L) {
      return("No queue yet \u2014 click Build queue.")
    }
    mine <- curation_app_active_reviews(reviews())
    mine <- mine[mine$reviewer_id == reviewer_id, , drop = FALSE]
    done <- sum(curation_app_key_string(queue$country, queue$document_id) %in% curation_app_key_string(mine$country, mine$document_id))
    sprintf("%d in queue \u00b7 %d done by you \u00b7 %d remaining", nrow(queue), done, nrow(queue) - done)
  })

  shiny::observeEvent(input$rnd_draw, {
    countries_now <- selected_countries()
    queue <- tryCatch(
      curation_build_review_queue(
        records(), reviews(),
        reviewer_id = reviewer_id, countries = countries_now,
        n_per_country = input$rnd_n %||% 50L, seed = input$rnd_seed, scope = scope
      ),
      error = function(e) e
    )
    if (inherits(queue, "error")) {
      shiny::showNotification(paste0("Queue build failed: ", conditionMessage(queue)), type = "error", duration = 8)
      return()
    }
    if (is.null(sample_id())) sample_id(paste0("sample-", gsub("[^A-Za-z0-9_.-]", "-", reviewer_id)))
    expected <- sample_revision()
    sample(queue)
    sample_index(1L)
    written <- tryCatch(
      curation_artifact_write_sample(
        artifact_store, scope,
        sample = list(sample_id = sample_id(), reviewer_id = reviewer_id, queue = queue, mode = attr(queue, "mode") %||% "empty", seed = input$rnd_seed),
        reviewer_id = reviewer_id, actor = actor, expected_revision = expected
      ),
      error = function(e) e
    )
    if (inherits(written, "error") || (is.list(written) && "ok" %in% names(written) && isFALSE(written$ok))) {
      message <- if (inherits(written, "error")) conditionMessage(written) else "the artifact store rejected the write"
      shiny::showNotification(paste0("Queue built but sample artifact was not saved: ", message), type = "warning", duration = 8)
    } else if (!is.null(written$revision)) {
      sample_revision(as.integer(written$revision))
    }
    shiny::showNotification(sprintf("Queued %d records (%s).", nrow(queue), attr(queue, "mode") %||% "empty"), type = "message")
  })
  shiny::observeEvent(input$rnd_keep, apply_bulk("keep", "random_tbl", random_df(), "random"))
  shiny::observeEvent(input$rnd_drop, apply_bulk("drop", "random_tbl", random_df(), "random"))
  shiny::observeEvent(input$rnd_unsure, apply_bulk("unsure", "random_tbl", random_df(), "random"))
  shiny::observeEvent(input$rnd_clear, apply_bulk(NULL, "random_tbl", random_df(), "random"))

  sr_current <- shiny::reactive({
    data <- random_df()
    if (nrow(data) == 0L) {
      return(NULL)
    }
    data[max(1L, min(as.integer(sample_index()), nrow(data))), , drop = FALSE]
  })
  sr_decision <- function(row) {
    if (is.null(row)) {
      return(NA_character_)
    }
    rv <- active_reviews()
    hit <- rv$country == row$country[[1L]] & rv$document_id == row$document_id[[1L]] & rv$reviewer_id == reviewer_id
    if (!any(hit)) NA_character_ else as.character(rv$decision[which(hit)[1L]])
  }
  sr_set <- function(decision) {
    row <- sr_current()
    if (is.null(row)) {
      return(invisible(NULL))
    }
    result <- safe_write_one(row$country[[1L]], row$document_id[[1L]], decision, "random")
    if (is.null(result)) {
      return(invisible(NULL))
    }
    data <- random_df()
    if (nrow(data) == 0L) {
      return(invisible(NULL))
    }
    done <- curation_app_key_string(data$country, data$document_id) %in% curation_app_key_string(active_reviews()$country[active_reviews()$reviewer_id == reviewer_id], active_reviews()$document_id[active_reviews()$reviewer_id == reviewer_id])
    next_index <- which(!done & seq_len(nrow(data)) > sample_index())
    if (length(next_index)) sample_index(next_index[[1L]]) else if (any(!done)) sample_index(which(!done)[[1L]])
    invisible(result)
  }
  shiny::observeEvent(input$sr_keep, sr_set("keep"))
  shiny::observeEvent(input$sr_drop, sr_set("drop"))
  shiny::observeEvent(input$sr_unsure, sr_set("unsure"))
  shiny::observeEvent(input$sr_next, {
    data <- random_df()
    if (nrow(data) == 0L) {
      return()
    }
    index <- as.integer(sample_index())
    if (isTRUE(input$sr_skip)) {
      done <- curation_app_key_string(data$country, data$document_id) %in% curation_app_key_string(active_reviews()$country[active_reviews()$reviewer_id == reviewer_id], active_reviews()$document_id[active_reviews()$reviewer_id == reviewer_id])
      candidates <- which(!done & seq_len(nrow(data)) > index)
      sample_index(if (length(candidates)) candidates[[1L]] else min(nrow(data), index + 1L))
    } else {
      sample_index(min(nrow(data), index + 1L))
    }
  })
  shiny::observeEvent(input$sr_prev, {
    data <- random_df()
    if (nrow(data) == 0L) {
      return()
    }
    index <- as.integer(sample_index())
    if (isTRUE(input$sr_skip)) {
      done <- curation_app_key_string(data$country, data$document_id) %in% curation_app_key_string(active_reviews()$country[active_reviews()$reviewer_id == reviewer_id], active_reviews()$document_id[active_reviews()$reviewer_id == reviewer_id])
      candidates <- which(!done & seq_len(nrow(data)) < index)
      sample_index(if (length(candidates)) candidates[[length(candidates)]] else max(1L, index - 1L))
    } else {
      sample_index(max(1L, index - 1L))
    }
  })
  shiny::observeEvent(input$sr_skip, {
    if (!isTRUE(input$sr_skip)) {
      return()
    }
    data <- random_df()
    if (nrow(data) == 0L) {
      return()
    }
    done <- curation_app_key_string(data$country, data$document_id) %in% curation_app_key_string(active_reviews()$country[active_reviews()$reviewer_id == reviewer_id], active_reviews()$document_id[active_reviews()$reviewer_id == reviewer_id])
    if (any(!done)) sample_index(which(!done)[[1L]])
  })
  output$sr_pos <- shiny::renderText({
    row <- sr_current()
    if (is.null(row)) {
      return("no sample")
    }
    sprintf("Record %d of %d \u00b7 %s%s", sample_index(), nrow(random_df()), toupper(row$country[[1L]]), if (!is.na(sr_decision(row))) paste0(" \u00b7 ", toupper(sr_decision(row))) else "")
  })
  output$sr_record <- shiny::renderUI({
    curation_app_single_record_ui(sr_current(), config)
  })
  output$sr_decision <- shiny::renderUI({
    row <- sr_current()
    if (is.null(row)) {
      return(NULL)
    }
    current <- sr_decision(row)
    shiny::tagList(
      shiny::actionButton("sr_keep", "Keep", class = paste("btn-lg btn-success", if (identical(current, "keep")) "active" else "")),
      shiny::actionButton("sr_drop", "Drop", class = paste("btn-lg btn-danger", if (identical(current, "drop")) "active" else "")),
      shiny::actionButton("sr_unsure", "Unsure", class = paste("btn-lg btn-warning", if (identical(current, "unsure")) "active" else ""))
    )
  })

  output$snapshot_info <- shiny::renderUI({
    origin <- if (!is.null(snapshot_version())) {
      sprintf("artifact snapshot v%s (revision %s)", snapshot_version(), snapshot_revision())
    } else {
      "injected record provider"
    }
    shiny::helpText(sprintf("%s records loaded from the %s.", format(nrow(records()), big.mark = ","), origin))
  })
  invisible(NULL)
}

#' Create a configurable curation Shiny application.
#'
#' All corpus, review, artifact, identity, and translation boundaries are
#' supplied by the caller.  The app resolves its trusted actor once per
#' session; display names are informational and cannot select a reviewer.
#'
#' @param config A `planscanR_curation_config`.
#' @param records_provider A `planscanR_curation_records_provider`.
#' @param review_repository A `planscanR_curation_review_repository`.
#' @param artifact_store A `planscanR_curation_artifact_store` used for sample,
#'   snapshot, and model seams.
#' @param actor_resolver A trusted `planscanR_curation_actor_resolver`.
#' @param request Optional deployment request value or function accepting
#'   `session`; it is passed to the actor resolver.
#' @return A `shiny.appobj`.
#' @export
curation_app <- function(
  config,
  records_provider,
  review_repository,
  artifact_store,
  actor_resolver,
  request = NULL
) {
  curation_app_require_ui()
  curation_check_config(config)
  curation_check_records_provider(records_provider)
  curation_check_review_repository(review_repository)
  curation_check_artifact_store(artifact_store)
  curation_app_check_actor_resolver(actor_resolver)
  context <- list(
    config = config,
    records_provider = records_provider,
    review_repository = review_repository,
    artifact_store = artifact_store,
    actor_resolver = actor_resolver,
    request = request
  )
  shiny::shinyApp(
    ui = curation_app_ui(config),
    server = function(input, output, session) curation_app_server(input, output, session, context)
  )
}

#' Run a configurable curation Shiny application.
#'
#' @inheritParams curation_app
#' @param launch.browser Passed to [shiny::runApp()].
#' @param ... Further arguments passed to [shiny::runApp()], such as `host` or
#'   `port`.
#' @return Invisibly, the result of [shiny::runApp()].
#' @export
run_curation <- function(
  config,
  records_provider,
  review_repository,
  artifact_store,
  actor_resolver,
  request = NULL,
  launch.browser = interactive(),
  ...
) {
  app <- curation_app(
    config = config,
    records_provider = records_provider,
    review_repository = review_repository,
    artifact_store = artifact_store,
    actor_resolver = actor_resolver,
    request = request
  )
  shiny::runApp(app, launch.browser = launch.browser, ...)
}
