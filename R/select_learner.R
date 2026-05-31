# Pluggable learner framework for the BIOGAIN selection model.
#
# A "selection learner" bundles a parsnip model specification with the engine
# package it needs, so [train_selection_model()] can fit it inside a tidymodels
# workflow (recipe + model) without the rest of the package knowing which
# algorithm is in play. This mirrors the pluggable `embedding_model()` /
# classifier S3 frameworks already in the package: swap the algorithm, keep the
# interface.
#
# The built-in learner is plain logistic regression on the base-R `glm` engine —
# no modelling backend beyond the tidymodels glue (parsnip / recipes / rsample /
# workflows). A custom learner can name extra engine packages in `engine_pkg` so
# training fails early with a helpful message when one isn't installed.

#' Construct a custom selection learner.
#'
#' Wraps a parsnip model specification into an S3 object the trainer
#' understands. Use the built-in constructors ([selection_learner_logistic()]
#' and friends) for the common cases, or this generic constructor to plug in any
#' parsnip classification spec.
#'
#' @param name Character scalar; a stable identifier stored on the fitted model
#'   and shown in the app.
#' @param spec A parsnip model specification in classification mode (e.g. from
#'   `parsnip::logistic_reg()`), OR — preferably — a zero-argument function that
#'   *returns* one. The function form keeps construction lazy, so a learner can
#'   be created (and listed in a UI) without parsnip installed; the spec is only
#'   built at fit time, behind the [train_selection_model()] dependency check.
#' @param engine_pkg Character vector of package names the engine needs at fit
#'   time (besides the tidymodels glue). Checked by [train_selection_model()].
#' @param recipe_fn Optional `function(train, feature_names) -> recipe`
#'   overriding the default preprocessing. `NULL` uses the package default
#'   (impute-via-feature-contract, dummy-encode nominals, drop zero-variance,
#'   normalise numerics).
#' @return An S3 object of class `planscanR_selection_learner`.
#' @export
#' @examples
#' \dontrun{
#' selection_learner(
#'   "my-logit",
#'   function() parsnip::set_engine(parsnip::logistic_reg(), "glm")
#' )
#' }
selection_learner <- function(name, spec, engine_pkg = character(0), recipe_fn = NULL) {
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    cli::cli_abort("{.arg name} must be a single non-empty string.")
  }
  spec_fn <- if (is.function(spec)) spec else function() spec
  structure(
    list(name = name, spec_fn = spec_fn, engine_pkg = engine_pkg, recipe_fn = recipe_fn),
    class = "planscanR_selection_learner"
  )
}

#' Built-in selection learner.
#'
#' Logistic regression on the base-R `glm` engine — the default the BIOGAIN
#' selection model trains. Needs only the tidymodels glue (parsnip / recipes /
#' rsample / workflows), no extra modelling backend.
#'
#' @return A `planscanR_selection_learner`.
#' @name selection_learners_builtin
#' @export
#' @examples
#' selection_learner_logistic()
selection_learner_logistic <- function() {
  spec_fn <- function() parsnip::set_engine(parsnip::logistic_reg(), "glm")
  selection_learner("logistic_glm", spec_fn)
}

#' Registry of the built-in learners.
#'
#' Maps a stable key to a zero-argument constructor, for UIs that let the user
#' pick a learner. `available_only = TRUE` returns an empty list when the
#' tidymodels packages needed to fit any learner aren't installed.
#'
#' @param available_only If `TRUE`, return learners only when they can actually
#'   be trained (the tidymodels packages are installed).
#' @return A named list of constructor functions, keyed by learner key.
#' @export
#' @examples
#' names(selection_learners())
selection_learners <- function(available_only = FALSE) {
  reg <- list(logistic = selection_learner_logistic)
  if (!available_only) {
    return(reg)
  }
  glue <- c("parsnip", "recipes", "rsample", "workflows")
  if (!all(vapply(glue, requireNamespace, logical(1), quietly = TRUE))) {
    return(reg[FALSE]) # no glue -> nothing is trainable
  }
  reg
}

#' @export
format.planscanR_selection_learner <- function(x, ...) {
  sprintf("<planscanR_selection_learner> %s", x$name)
}

#' @export
print.planscanR_selection_learner <- function(x, ...) {
  cat(format(x, ...), "\n", sep = "")
  invisible(x)
}

# Tidymodels packages required to fit ANY learner (the glue), plus the learner's
# own engine package. Aborts with a classed, actionable error if missing.
#' @noRd
require_tidymodels <- function(engine_pkg = character(0)) {
  needed <- c("parsnip", "recipes", "rsample", "workflows", engine_pkg)
  miss <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss) > 0L) {
    cli::cli_abort(
      c(
        "Selection-model training needs {cli::qty(miss)}package{?s}: {.pkg {miss}}.",
        i = "Install {cli::qty(miss)}{?it/them} (e.g. the tidymodels packages) and retry."
      ),
      class = "planscanR_error_missing_dep"
    )
  }
  invisible()
}

# Default preprocessing recipe. Uses the explicit predictor list (not `~ .`) so
# the key columns (document_id, country) are never treated as predictors unless
# the caller opted country in via `include`. Numeric predictors are normalised,
# which is harmless for logistic regression and helps any scale-sensitive
# learner a custom learner might plug in.
#' @noRd
selection_recipe <- function(train, feature_names) {
  rec <- recipes::recipe(
    x = train,
    vars = c("decision", feature_names),
    roles = c("outcome", rep("predictor", length(feature_names)))
  )
  rec <- recipes::step_novel(rec, recipes::all_nominal_predictors())
  rec <- recipes::step_unknown(rec, recipes::all_nominal_predictors())
  rec <- recipes::step_dummy(rec, recipes::all_nominal_predictors())
  rec <- recipes::step_zv(rec, recipes::all_predictors())
  rec <- recipes::step_normalize(rec, recipes::all_numeric_predictors())
  rec
}

# Assemble the workflow (recipe + model) for a learner.
#' @noRd
build_selection_workflow <- function(learner, train, feature_names) {
  rec <- if (!is.null(learner$recipe_fn)) {
    learner$recipe_fn(train, feature_names)
  } else {
    selection_recipe(train, feature_names)
  }
  wf <- workflows::add_recipe(workflows::workflow(), rec)
  workflows::add_model(wf, learner$spec_fn())
}
