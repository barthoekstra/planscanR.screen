# Artifact-store interface. C3 owns concrete snapshot/sample/model persistence
# and atomic active-model publication; this layer only validates and forwards the
# scoped callbacks.

#' Build an injected artifact-store contract.
#'
#' The store receives explicit scope, actor, and revision values. Model payloads
#' retain the existing planscanR selection-model serialization; C3 decides how
#' immutable versions and active references are persisted under these paths.
#'
#' @param paths Explicit named artifact paths, including root.
#' @param snapshot_read_fn Function accepting scope, version, and actor.
#' @param snapshot_write_fn Function accepting scope, snapshot, actor, and
#'   expected_revision.
#' @param sample_list_fn Function accepting scope, reviewer_id, and actor.
#' @param sample_read_fn Function accepting scope, sample_id, and actor.
#' @param sample_write_fn Function accepting scope, sample, reviewer_id, actor,
#'   and expected_revision.
#' @param model_list_fn Function accepting scope and actor.
#' @param model_read_fn Function accepting scope, model_id, and actor.
#' @param model_write_fn Function accepting scope, model, provenance, actor, and
#'   expected_revision.
#' @param model_publish_fn Function accepting scope, model_id, actor, and
#'   expected_active_revision.
#' @param active_model_fn Function accepting scope and actor.
#' @return A planscanR_curation_artifact_store object.
#' @export
curation_artifact_store <- function(
  paths,
  snapshot_read_fn,
  snapshot_write_fn,
  sample_list_fn,
  sample_read_fn,
  sample_write_fn,
  model_list_fn,
  model_read_fn,
  model_write_fn,
  model_publish_fn,
  active_model_fn
) {
  paths <- curation_validate_artifact_paths(paths)
  callbacks <- list(
    snapshot_read_fn = curation_callback_contract(
      snapshot_read_fn,
      c("scope", "version", "actor"),
      "snapshot_read_fn"
    ),
    snapshot_write_fn = curation_callback_contract(
      snapshot_write_fn,
      c("scope", "snapshot", "actor", "expected_revision"),
      "snapshot_write_fn"
    ),
    sample_list_fn = curation_callback_contract(
      sample_list_fn,
      c("scope", "reviewer_id", "actor"),
      "sample_list_fn"
    ),
    sample_read_fn = curation_callback_contract(
      sample_read_fn,
      c("scope", "sample_id", "actor"),
      "sample_read_fn"
    ),
    sample_write_fn = curation_callback_contract(
      sample_write_fn,
      c("scope", "sample", "reviewer_id", "actor", "expected_revision"),
      "sample_write_fn"
    ),
    model_list_fn = curation_callback_contract(
      model_list_fn,
      c("scope", "actor"),
      "model_list_fn"
    ),
    model_read_fn = curation_callback_contract(
      model_read_fn,
      c("scope", "model_id", "actor"),
      "model_read_fn"
    ),
    model_write_fn = curation_callback_contract(
      model_write_fn,
      c("scope", "model", "provenance", "actor", "expected_revision"),
      "model_write_fn"
    ),
    model_publish_fn = curation_callback_contract(
      model_publish_fn,
      c("scope", "model_id", "actor", "expected_active_revision"),
      "model_publish_fn"
    ),
    active_model_fn = curation_callback_contract(
      active_model_fn,
      c("scope", "actor"),
      "active_model_fn"
    )
  )
  structure(c(list(paths = paths), callbacks), class = "planscanR_curation_artifact_store")
}

curation_check_artifact_store <- function(store) {
  if (!inherits(store, "planscanR_curation_artifact_store")) {
    curation_abort(
      "{.arg store} must be a planscanR_curation_artifact_store.",
      class = "planscanR_error_curation_artifact"
    )
  }
  invisible(store)
}

curation_check_artifact_args <- function(store, scope, actor) {
  curation_check_artifact_store(store)
  curation_check_scope(scope, class = "planscanR_error_curation_artifact")
  curation_check_actor(actor)
}

#' Read a versioned corpus snapshot.
#'
#' @param store A planscanR_curation_artifact_store.
#' @param scope A planscanR_curation_scope.
#' @param actor Trusted authenticated actor.
#' @param version Optional immutable snapshot version.
#' @return `NULL` for a never-created version, or a snapshot result containing
#'   `snapshot_id`, scope/config identity, `version`, `revision`, and its
#'   artifact path/payload.
#' @export
curation_artifact_read_snapshot <- function(store, scope, actor, version = NULL) {
  curation_check_artifact_args(store, scope, actor)
  store$snapshot_read_fn(scope = scope, version = version, actor = actor)
}

#' Write a versioned corpus snapshot.
#'
#' @param store A planscanR_curation_artifact_store.
#' @param scope A planscanR_curation_scope.
#' @param snapshot Snapshot payload.
#' @param actor Trusted authenticated actor.
#' @param expected_revision Required current revision, or 0 for a new version.
#' @return A mutation result containing `ok`, `snapshot_id`, `version`, and the
#'   resulting `revision`.
#' @export
curation_artifact_write_snapshot <- function(store, scope, snapshot, actor, expected_revision = NULL) {
  curation_check_artifact_args(store, scope, actor)
  revision <- curation_revision_vector(expected_revision, 1L)
  store$snapshot_write_fn(
    scope = scope,
    snapshot = snapshot,
    actor = actor,
    expected_revision = revision[[1L]]
  )
}

#' List project/reviewer-scoped samples.
#'
#' @param store A planscanR_curation_artifact_store.
#' @param scope A planscanR_curation_scope.
#' @param reviewer_id Optional trusted reviewer ID filter.
#' @param actor Trusted authenticated actor.
#' @return Sample rows containing at least `sample_id`, `reviewer_id`, scope,
#'   `revision`, and sample artifact metadata.
#' @export
curation_artifact_list_samples <- function(store, scope, reviewer_id = NULL, actor) {
  curation_check_artifact_args(store, scope, actor)
  if (!is.null(reviewer_id)) {
    reviewer_id <- curation_scalar_character(
      reviewer_id,
      "reviewer_id",
      class = "planscanR_error_curation_artifact"
    )
  }
  store$sample_list_fn(scope = scope, reviewer_id = reviewer_id, actor = actor)
}

#' Read one immutable sample.
#'
#' @param store A planscanR_curation_artifact_store.
#' @param scope A planscanR_curation_scope.
#' @param sample_id Sample identifier.
#' @param actor Trusted authenticated actor.
#' @return `NULL` or a sample result containing `sample_id`, `reviewer_id`,
#'   scope, `revision`, and sample payload/metadata.
#' @export
curation_artifact_read_sample <- function(store, scope, sample_id, actor) {
  curation_check_artifact_args(store, scope, actor)
  sample_id <- curation_scalar_character(
    sample_id,
    "sample_id",
    class = "planscanR_error_curation_artifact"
  )
  store$sample_read_fn(scope = scope, sample_id = sample_id, actor = actor)
}

#' Write a project/reviewer-scoped sample.
#'
#' @param store A planscanR_curation_artifact_store.
#' @param scope A planscanR_curation_scope.
#' @param sample Sample payload.
#' @param reviewer_id Trusted reviewer ID.
#' @param actor Trusted authenticated actor.
#' @param expected_revision Required current revision, or 0 for a new sample.
#' @return A mutation result containing `ok`, `sample_id`, `reviewer_id`, and
#'   the resulting `revision`.
#' @export
curation_artifact_write_sample <- function(
  store,
  scope,
  sample,
  reviewer_id,
  actor,
  expected_revision = NULL
) {
  curation_check_artifact_args(store, scope, actor)
  reviewer_id <- curation_scalar_character(
    reviewer_id,
    "reviewer_id",
    class = "planscanR_error_curation_artifact"
  )
  revision <- curation_revision_vector(expected_revision, 1L)
  store$sample_write_fn(
    scope = scope,
    sample = sample,
    reviewer_id = reviewer_id,
    actor = actor,
    expected_revision = revision[[1L]]
  )
}

#' List immutable model versions.
#'
#' @param store A planscanR_curation_artifact_store.
#' @param scope A planscanR_curation_scope.
#' @param actor Trusted authenticated actor.
#' @return Model rows containing at least `model_id`, scope/config identity,
#'   `revision`, provenance, and artifact metadata.
#' @export
curation_artifact_list_models <- function(store, scope, actor) {
  curation_check_artifact_args(store, scope, actor)
  store$model_list_fn(scope = scope, actor = actor)
}

#' Read an immutable model version.
#'
#' @param store A planscanR_curation_artifact_store.
#' @param scope A planscanR_curation_scope.
#' @param model_id Model version identifier.
#' @param actor Trusted authenticated actor.
#' @return `NULL` or a model result containing `model_id`, scope/config
#' identity, `revision`, provenance, and model payload/metadata.
#' @export
curation_artifact_read_model <- function(store, scope, model_id, actor) {
  curation_check_artifact_args(store, scope, actor)
  model_id <- curation_scalar_character(
    model_id,
    "model_id",
    class = "planscanR_error_curation_artifact"
  )
  store$model_read_fn(scope = scope, model_id = model_id, actor = actor)
}

#' Write an immutable model version.
#'
#' @param store A planscanR_curation_artifact_store.
#' @param scope A planscanR_curation_scope.
#' @param model Model payload in the existing selection-model format.
#' @param provenance Training-review/configuration provenance.
#' @param actor Trusted authenticated actor.
#' @param expected_revision Required current revision, or 0 for a new version.
#' @return A mutation result containing `ok`, `model_id`, and the resulting
#' `revision`.
#' @export
curation_artifact_write_model <- function(
  store,
  scope,
  model,
  provenance,
  actor,
  expected_revision = NULL
) {
  curation_check_artifact_args(store, scope, actor)
  revision <- curation_revision_vector(expected_revision, 1L)
  store$model_write_fn(
    scope = scope,
    model = model,
    provenance = provenance,
    actor = actor,
    expected_revision = revision[[1L]]
  )
}

#' Atomically publish an immutable model as active.
#'
#' @param store A planscanR_curation_artifact_store.
#' @param scope A planscanR_curation_scope.
#' @param model_id Immutable model version identifier.
#' @param actor Trusted authenticated actor.
#' @param expected_active_revision Required current active-reference revision;
#'   0 means no active model.
#' @return A publication result containing `ok`, `active_model_id`, and the
#' resulting `active_revision`; publication is atomic.
#' @export
curation_artifact_publish_model <- function(
  store,
  scope,
  model_id,
  actor,
  expected_active_revision = NULL
) {
  curation_check_artifact_args(store, scope, actor)
  model_id <- curation_scalar_character(
    model_id,
    "model_id",
    class = "planscanR_error_curation_artifact"
  )
  revision <- curation_revision_vector(expected_active_revision, 1L)
  store$model_publish_fn(
    scope = scope,
    model_id = model_id,
    actor = actor,
    expected_active_revision = revision[[1L]]
  )
}

#' Read the active model reference.
#'
#' @param store A planscanR_curation_artifact_store.
#' @param scope A planscanR_curation_scope.
#' @param actor Trusted authenticated actor.
#' @return `NULL` when no model is active, or an active-model result containing
#' `active_model_id`, `active_revision`, and model provenance.
#' @export
curation_artifact_active_model <- function(store, scope, actor) {
  curation_check_artifact_args(store, scope, actor)
  store$active_model_fn(scope = scope, actor = actor)
}
