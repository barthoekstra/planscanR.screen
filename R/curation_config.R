# Configuration and identity contracts for the generic curation services.
#
# This file deliberately contains no project defaults and no reads from options,
# environment variables, or package-global state. A caller resolves its inputs
# once and passes the resulting values into `curation_config()`.

curation_abort <- function(message, class = "planscanR_error_curation_config") {
  cli::cli_abort(message, class = class)
}

curation_scalar_character <- function(x, arg, class = "planscanR_error_curation_config") {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    curation_abort(
      paste0("{.arg ", arg, "} must be one non-empty character string."),
      class = class
    )
  }
  trimws(x)
}

curation_named_character <- function(x, arg) {
  if (
    !is.character(x) ||
      length(x) == 0L ||
      is.null(names(x)) ||
      anyNA(x) ||
      any(!nzchar(trimws(x))) ||
      anyNA(names(x)) ||
      any(!nzchar(names(x))) ||
      anyDuplicated(names(x))
  ) {
    curation_abort(
      paste0(
        "{.arg ", arg,
        "} must be a non-empty named character vector with unique names and values."
      )
    )
  }
  x
}

curation_function_accepts <- function(fn, required) {
  if (!is.function(fn)) {
    return(FALSE)
  }
  f <- names(formals(fn))
  all(required %in% f) || "..." %in% f
}

curation_validate_selection_controls <- function(x) {
  if (!is.list(x) || (!is.null(names(x)) && anyNA(names(x)))) {
    curation_abort("{.arg selection_controls} must be a named list.")
  }
  nms <- names(x)
  if (is.null(nms)) {
    if (length(x) > 0L) {
      curation_abort("{.arg selection_controls} must be named.")
    }
    return(list())
  }
  if (any(!nzchar(nms)) || anyDuplicated(nms)) {
    curation_abort("{.arg selection_controls} must have unique non-empty names.")
  }
  x
}

curation_validate_funnel_stages <- function(stages) {
  if (!is.list(stages) || length(stages) == 0L) {
    curation_abort("{.arg funnel_stages} must be a non-empty list.")
  }
  keys <- vapply(seq_along(stages), function(i) {
    stage <- stages[[i]]
    if (!is.list(stage)) {
      curation_abort("Each {.arg funnel_stages} entry must be a list.")
    }
    required <- c("key", "label", "fn")
    if (!all(required %in% names(stage))) {
      curation_abort(
        "Each funnel stage must contain {.field key}, {.field label}, and {.field fn}."
      )
    }
    key <- curation_scalar_character(stage$key, "funnel_stages[[i]]$key")
    curation_scalar_character(stage$label, "funnel_stages[[i]]$label")
    if (!curation_function_accepts(stage$fn, c("records", "context"))) {
      curation_abort(
        "Each funnel stage {.field fn} must accept {.arg records} and {.arg context}."
      )
    }
    key
  }, character(1))
  if (anyDuplicated(keys)) {
    curation_abort("{.arg funnel_stages} keys must be unique.")
  }
  stages
}

curation_validate_branding <- function(branding) {
  if (!is.list(branding) || is.null(names(branding))) {
    curation_abort("{.arg branding} must be a named list.")
  }
  required <- c("title", "primary")
  if (!all(required %in% names(branding))) {
    curation_abort(
      "{.arg branding} must include explicit {.field title} and {.field primary} values."
    )
  }
  for (nm in names(branding)) {
    value <- branding[[nm]]
    if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
      curation_abort("Each branding value must be one non-empty character string.")
    }
  }
  branding
}

curation_validate_artifact_paths <- function(paths) {
  if (!is.list(paths) || is.null(names(paths)) || length(paths) == 0L) {
    curation_abort("{.arg artifact_paths} must be a non-empty named list.")
  }
  if (!"root" %in% names(paths)) {
    curation_abort("{.arg artifact_paths} must include an explicit {.field root}.")
  }
  if (anyNA(names(paths)) || any(!nzchar(names(paths))) || anyDuplicated(names(paths))) {
    curation_abort("{.arg artifact_paths} must have unique non-empty names.")
  }
  for (nm in names(paths)) {
    curation_scalar_character(paths[[nm]], paste0("artifact_paths$", nm))
  }
  paths
}

#' Build a scope shared by curation services.
#'
#' A scope is explicit and immutable by convention. `project_id` identifies a
#' curation workspace, `corpus_id` identifies the source corpus, and `config_id`
#' identifies the versioned curation configuration. Review repositories may key
#' authored decisions by project and corpus while retaining the complete scope
#' on imported/exported provenance; artifact stores use all three values.
#'
#' @param project_id Non-empty curation-workspace identifier.
#' @param corpus_id Non-empty corpus identifier.
#' @param config_id Non-empty versioned configuration identifier.
#' @return A `planscanR_curation_scope` object.
#' @export
curation_scope <- function(project_id, corpus_id, config_id) {
  out <- list(
    project_id = curation_scalar_character(project_id, "project_id"),
    corpus_id = curation_scalar_character(corpus_id, "corpus_id"),
    config_id = curation_scalar_character(config_id, "config_id")
  )
  structure(out, class = "planscanR_curation_scope")
}

#' Build a trusted authenticated actor value.
#'
#' This constructor is intended for an authenticated deployment resolver or a
#' test double. Review UI input, display names, and untrusted headers must never
#' be used to construct the identity passed to a repository.
#'
#' @param id Stable authenticated actor identifier.
#' @param roles Character vector of server-assigned roles.
#' @param display_name Optional informational display name; it is never an
#'   identity key.
#' @return A `planscanR_curation_actor` object.
#' @export
curation_actor <- function(id, roles = character(0), display_name = NULL) {
  id <- curation_scalar_character(
    id,
    "id",
    class = "planscanR_error_curation_actor"
  )
  if (!is.character(roles) || anyNA(roles) || any(!nzchar(roles))) {
    curation_abort(
      "{.arg roles} must be a character vector without empty values.",
      class = "planscanR_error_curation_actor"
    )
  }
  if (!is.null(display_name)) {
    display_name <- curation_scalar_character(
      display_name,
      "display_name",
      class = "planscanR_error_curation_actor"
    )
  }
  structure(
    list(id = id, roles = unique(roles), display_name = display_name),
    class = "planscanR_curation_actor"
  )
}

#' Wrap an authenticated actor resolver for injection into a curation context.
#'
#' @param resolve_fn Function accepting a deployment request/session context and
#'   returning a `planscanR_curation_actor`.
#' @return A `planscanR_curation_actor_resolver` object.
#' @export
curation_actor_resolver <- function(resolve_fn) {
  if (!curation_function_accepts(resolve_fn, "request")) {
    curation_abort(
      "{.arg resolve_fn} must accept {.arg request}.",
      class = "planscanR_error_curation_actor"
    )
  }
  structure(list(resolve_fn = resolve_fn), class = "planscanR_curation_actor_resolver")
}

#' Resolve the authenticated actor for a request.
#'
#' @param resolver A `planscanR_curation_actor_resolver`.
#' @param request Deployment-supplied request/session context.
#' @return A `planscanR_curation_actor`.
#' @export
curation_resolve_actor <- function(resolver, request) {
  if (!inherits(resolver, "planscanR_curation_actor_resolver")) {
    curation_abort(
      "{.arg resolver} must be a planscanR_curation_actor_resolver.",
      class = "planscanR_error_curation_actor"
    )
  }
  actor <- resolver$resolve_fn(request = request)
  if (!inherits(actor, "planscanR_curation_actor")) {
    curation_abort(
      "The actor resolver must return a planscanR_curation_actor.",
      class = "planscanR_error_curation_actor"
    )
  }
  actor
}

#' Build a configuration for the generic curation services.
#'
#' All policy and identity inputs are required. The constructor does not read
#' environment variables or options and does not install a project default, so
#' independent configurations can coexist in one R process.
#'
#' @param project_id,corpus_id,config_id Explicit curation, corpus, and
#'   versioned-configuration identifiers.
#' @param topics Named topic vector used by the caller's scoring workflow.
#' @param labels Named classifier-label vector. An optional `relevant`
#'   attribute must contain only label names.
#' @param lexicon Named list of keyword vectors.
#' @param select_fn Function accepting `records` and `controls`, returning a
#'   same-row data frame with a logical `selected` column.
#' @param selection_controls Named list of caller-supplied policy controls.
#' @param funnel_stages Non-empty list of `{key, label, fn}` entries. Each `fn`
#'   accepts `records` and `context` and returns a logical stage mask.
#' @param branding Explicit named branding values, including `title` and
#'   `primary`.
#' @param artifact_paths Explicit named artifact paths, including `root`.
#' @param translator Optional `curation_translation_provider`; `NULL` is valid.
#' @return A `planscanR_curation_config` object.
#' @export
curation_config <- function(
  project_id,
  corpus_id,
  config_id,
  topics,
  labels,
  lexicon,
  select_fn,
  selection_controls,
  funnel_stages,
  branding,
  artifact_paths,
  translator = NULL
) {
  identity <- curation_scope(project_id, corpus_id, config_id)
  topics <- curation_named_character(topics, "topics")
  labels <- curation_named_character(labels, "labels")
  if (length(labels) < 2L) {
    curation_abort("{.arg labels} must contain at least two candidate labels.")
  }
  relevant <- attr(labels, "relevant", exact = TRUE)
  if (!is.null(relevant)) {
    if (!is.character(relevant) || anyNA(relevant) || any(!relevant %in% names(labels))) {
      curation_abort(
        "The {.field relevant} label attribute must contain only names from {.arg labels}."
      )
    }
  }
  if (!is.list(lexicon) || is.null(names(lexicon)) || length(lexicon) == 0L) {
    curation_abort("{.arg lexicon} must be a non-empty named list.")
  }
  if (anyNA(names(lexicon)) || any(!nzchar(names(lexicon))) || anyDuplicated(names(lexicon))) {
    curation_abort("{.arg lexicon} must have unique non-empty names.")
  }
  for (nm in names(lexicon)) {
    terms <- lexicon[[nm]]
    if (!is.character(terms) || length(terms) == 0L || anyNA(terms) || any(!nzchar(terms))) {
      curation_abort("Each {.arg lexicon} entry must be a non-empty character vector.")
    }
  }
  if (!curation_function_accepts(select_fn, c("records", "controls"))) {
    curation_abort("{.arg select_fn} must accept {.arg records} and {.arg controls}.")
  }
  selection_controls <- curation_validate_selection_controls(selection_controls)
  funnel_stages <- curation_validate_funnel_stages(funnel_stages)
  branding <- curation_validate_branding(branding)
  artifact_paths <- curation_validate_artifact_paths(artifact_paths)
  if (!is.null(translator) && !inherits(translator, "planscanR_curation_translation_provider")) {
    curation_abort(
      "{.arg translator} must be NULL or a planscanR_curation_translation_provider."
    )
  }
  structure(
    list(
      identity = identity,
      project_id = identity$project_id,
      corpus_id = identity$corpus_id,
      config_id = identity$config_id,
      topics = topics,
      labels = labels,
      lexicon = lexicon,
      select_fn = select_fn,
      selection_controls = selection_controls,
      funnel_stages = funnel_stages,
      branding = branding,
      artifact_paths = artifact_paths,
      translator = translator
    ),
    class = "planscanR_curation_config"
  )
}

curation_check_config <- function(config) {
  if (!inherits(config, "planscanR_curation_config")) {
    curation_abort("{.arg config} must be a planscanR_curation_config.")
  }
  invisible(config)
}

#' Apply a configuration's selection callback to records.
#'
#' @param config A `planscanR_curation_config`.
#' @param records Data frame of enriched records.
#' @param controls Optional named overrides merged over the configuration's
#'   explicit `selection_controls`.
#' @return The callback result, with a logical `selected` column.
#' @export
curation_apply_selection <- function(config, records, controls = NULL) {
  curation_check_config(config)
  if (!is.data.frame(records)) {
    curation_abort("{.arg records} must be a data frame.", "planscanR_error_bad_input")
  }
  if (!is.null(controls)) {
    controls <- curation_validate_selection_controls(controls)
  } else {
    controls <- list()
  }
  resolved <- modifyList(config$selection_controls, controls)
  out <- config$select_fn(records = records, controls = resolved)
  if (!is.data.frame(out) || nrow(out) != nrow(records) || !"selected" %in% names(out)) {
    curation_abort(
      "The selection callback must return the same number of rows and a {.field selected} column.",
      class = "planscanR_error_curation_selection"
    )
  }
  if (!is.logical(out$selected) || length(out$selected) != nrow(records)) {
    curation_abort(
      "The selection callback's {.field selected} column must be logical.",
      class = "planscanR_error_curation_selection"
    )
  }
  out
}

#' Compute configured funnel-stage counts.
#'
#' @param config A `planscanR_curation_config`.
#' @param records Records after selection/model enrichment.
#' @param context Optional per-call context passed to each stage predicate.
#' @return A data frame with `order`, `stage`, `key`, `n`, and `pct` columns.
#' @export
curation_compute_funnel <- function(config, records, context = list()) {
  curation_check_config(config)
  if (!is.data.frame(records)) {
    curation_abort("{.arg records} must be a data frame.", "planscanR_error_bad_input")
  }
  if (!is.list(context)) {
    curation_abort("{.arg context} must be a list.", "planscanR_error_curation_selection")
  }
  ctx <- c(config = list(config), context)
  total <- nrow(records)
  rows <- lapply(seq_along(config$funnel_stages), function(i) {
    stage <- config$funnel_stages[[i]]
    mask <- stage$fn(records = records, context = ctx)
    if (!is.logical(mask) || length(mask) != total) {
      curation_abort(
        paste0("Funnel stage {.field ", stage$key, "} must return a logical mask."),
        class = "planscanR_error_curation_selection"
      )
    }
    data.frame(
      order = as.integer(i),
      stage = stage$label,
      key = stage$key,
      n = as.integer(sum(mask %in% TRUE)),
      pct = if (total > 0L) 100 * sum(mask %in% TRUE) / total else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}
