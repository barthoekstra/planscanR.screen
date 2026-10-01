# Generic curation app entry point.
#
# The production entry point is `planscanR.screen::curation_app()` because it
# requires an explicit configuration and service context.  This file is kept
# as a conventional Shiny app resource for deployments that construct
# `.curation_context` before sourcing it; it contains no project defaults.

if (!exists(".curation_context", envir = .GlobalEnv, inherits = FALSE)) {
  stop(
    "Set .curation_context to a planscanR curation app context before sourcing app.R.",
    call. = FALSE
  )
}

context <- get(".curation_context", envir = .GlobalEnv, inherits = FALSE)
if (!is.list(context) || is.null(context$config)) {
  stop(".curation_context must contain an explicit curation config and service context.", call. = FALSE)
}

app <- planscanR.screen::curation_app(
  config = context$config,
  records_provider = context$records_provider,
  review_repository = context$review_repository,
  artifact_store = context$artifact_store,
  actor_resolver = context$actor_resolver,
  request = context$request
)

app
