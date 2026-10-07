# Disposable 128k browser fixture; reuses the existing offline service doubles.
# Run from the package root with R_LIBS_USER set to the workspace .Rlib.
pkgload::load_all(".", quiet = TRUE)
source("inst/curation/acq_ux1_characterization.R")
fixture <- parse("inst/curation/fixture_app.R")
# The final expression launches the app; evaluate only its setup expressions.
for (expression in head(fixture, -1L)) eval(expression)
config <- acq_ux1_fixture_config()
records_provider <- acq_ux1_fixture_provider(acq_ux1_fixture_records())
run_curation(
  config = config,
  records_provider = records_provider,
  review_repository = review_repository,
  artifact_store = artifact_store,
  actor_resolver = actor_resolver,
  launch.browser = FALSE,
  host = "127.0.0.1",
  port = 43819
)
