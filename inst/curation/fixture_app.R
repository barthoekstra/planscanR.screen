# Offline curation-app smoke fixture.
#
# This launcher intentionally creates a synthetic corpus and a temporary
# SQLite review file. It never resolves a cache root, invokes Python, or reads
# personal review data. From this package checkout, run:
#
#   Rscript inst/curation/fixture_app.R
#
# The app listens on 127.0.0.1:43817 so a browser smoke review cannot collide
# with the BIOGAIN or RAG development apps.

script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(script_file) || !nzchar(script_file)) script_file <- "inst/curation/fixture_app.R"
package_root <- normalizePath(file.path(dirname(script_file), "../.."), mustWork = FALSE)
if (file.exists(file.path(package_root, "DESCRIPTION"))) {
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Install pkgload to run this checkout fixture.", call. = FALSE)
  }
  pkgload::load_all(package_root, quiet = TRUE)
} else {
  if (!requireNamespace("planscanR.screen", quietly = TRUE)) {
    stop("Install planscanR.screen to run this fixture.", call. = FALSE)
  }
  library(planscanR.screen)
}

topics <- c(alpha = "alpha topic")
labels <- c(keep = "Keep", drop = "Drop")
attr(labels, "relevant") <- "keep"
stages <- list(
  list(key = "indexed", label = "Indexed", fn = function(records, context) rep(TRUE, nrow(records))),
  list(key = "selected", label = "Heuristic selected", fn = function(records, context) records$selected %in% TRUE),
  list(key = "reviewed", label = "Human reviewed", fn = function(records, context) {
    keys <- paste(records$country, records$document_id, sep = "::")
    reviewed <- context$reviews
    if (is.null(reviewed) || nrow(reviewed) == 0L) {
      return(rep(FALSE, nrow(records)))
    }
    keys %in% paste(reviewed$country, reviewed$document_id, sep = "::")
  })
)
config <- curation_config(
  project_id = "fixture-project",
  corpus_id = "fixture-corpus",
  config_id = "fixture-c1f.v1",
  topics = topics,
  labels = labels,
  lexicon = list(alpha = c("alpha", "renewable")),
  select_fn = function(records, controls) {
    records$selected <- records$cosine_max >= controls$relevance_threshold |
      records$kw_total >= controls$kw_min
    records
  },
  selection_controls = list(
    relevance_threshold = 0.5,
    kw_min = 1L,
    use_classifier = FALSE,
    use_trim = FALSE,
    nonrenewable = character(0),
    nonrenewable_score = 0.5
  ),
  funnel_stages = stages,
  branding = list(title = "Curation fixture", primary = "#1261a0"),
  artifact_paths = list(root = file.path(tempdir(), "planscanR-screen-c1f-fixture")),
  translator = NULL
)

records_provider <- curation_records_provider(
  read_fn = function(corpus_id, countries = NULL) {
    out <- data.frame(
      country = c("nl", "nl", "at", "at"),
      document_id = c("collision-1", "nl-2", "collision-1", "at-2"),
      title = c("Alpha proposal", "Second proposal", "Alpha Austria", "Fourth proposal"),
      summary = c("alpha renewable", "quiet summary", "alpha renewable", "other summary"),
      relevance_score_alpha = c(0.92, 0.21, 0.88, 0.12),
      stringsAsFactors = FALSE
    )
    if (!is.null(countries)) out[out$country %in% countries, , drop = FALSE] else out
  },
  refresh_fn = function(corpus_id, countries = NULL) {
    # Keep refresh deterministic; the app still exercises the artifact write
    # boundary when the user clicks Rebuild snapshot.
    records_provider$read_fn(corpus_id = corpus_id, countries = countries)
  }
)

actor <- curation_actor("fixture-reviewer", roles = "reviewer", display_name = "Offline fixture reviewer")
actor_resolver <- curation_actor_resolver(function(request) actor)
review_db <- file.path(tempdir(), "planscanR-screen-c1f-fixture-reviews.sqlite")
review_repository <- curation_review_repository_sqlite(review_db)

artifact_state <- new.env(parent = emptyenv())
artifact_state$sample <- NULL
artifact_state$sample_revision <- 0L
artifact_state$model <- NULL
artifact_state$model_revision <- 0L
artifact_state$active_model <- NULL
artifact_state$active_revision <- 0L
artifact_store <- curation_artifact_store(
  paths = config$artifact_paths,
  snapshot_read_fn = function(scope, version, actor) NULL,
  snapshot_write_fn = function(scope, snapshot, actor, expected_revision) {
    list(ok = TRUE, snapshot_id = snapshot$snapshot_id, version = snapshot$version, revision = expected_revision + 1L)
  },
  sample_list_fn = function(scope, reviewer_id, actor) {
    if (is.null(artifact_state$sample)) {
      return(data.frame(sample_id = character(0), reviewer_id = character(0), revision = integer(0)))
    }
    data.frame(sample_id = artifact_state$sample$sample_id, reviewer_id = reviewer_id, revision = artifact_state$sample_revision)
  },
  sample_read_fn = function(scope, sample_id, actor) {
    if (is.null(artifact_state$sample)) NULL else list(payload = artifact_state$sample, revision = artifact_state$sample_revision)
  },
  sample_write_fn = function(scope, sample, reviewer_id, actor, expected_revision) {
    if (!identical(as.integer(expected_revision), as.integer(artifact_state$sample_revision))) stop("fixture sample revision conflict")
    artifact_state$sample <- sample
    artifact_state$sample_revision <- artifact_state$sample_revision + 1L
    list(ok = TRUE, sample_id = sample$sample_id, reviewer_id = reviewer_id, revision = artifact_state$sample_revision)
  },
  model_list_fn = function(scope, actor) data.frame(model_id = character(0), revision = integer(0)),
  model_read_fn = function(scope, model_id, actor) NULL,
  model_write_fn = function(scope, model, provenance, actor, expected_revision) {
    artifact_state$model <- model
    artifact_state$model_revision <- artifact_state$model_revision + 1L
    list(ok = TRUE, model_id = paste0("fixture-model-", artifact_state$model_revision), revision = artifact_state$model_revision)
  },
  model_publish_fn = function(scope, model_id, actor, expected_active_revision) {
    artifact_state$active_model <- model_id
    artifact_state$active_revision <- artifact_state$active_revision + 1L
    list(ok = TRUE, active_model_id = model_id, active_revision = artifact_state$active_revision)
  },
  active_model_fn = function(scope, actor) NULL
)

run_curation(
  config = config,
  records_provider = records_provider,
  review_repository = review_repository,
  artifact_store = artifact_store,
  actor_resolver = actor_resolver,
  launch.browser = FALSE,
  host = "127.0.0.1",
  port = 43817
)
