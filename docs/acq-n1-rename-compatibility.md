# ACQ-N1: package naming and rename compatibility

**Status: decision confirmed 2026-10-02; implementation deferred.** The existing
`planscanR.screen` framework and repository are planned to become
`planscanR.curation`. ACQ-N1 records the migration plan only; it does not
authorize a repository, package, runtime, or deployment change. The existing
curation contracts in [`curation-contracts.md`](curation-contracts.md) remain the
behavioral specification; this note adds no new contract.

## Selected naming direction

Rename the existing framework repository and installed package to
`planscanR.curation` in a separately authorized future migration. This evolves
the same general-purpose framework; it does not add a package. BIOGAIN policy
and configuration remain in `planscanR.biogain`. Until that migration is
implemented, callers continue to use the current `planscanR.screen` identity.

## Current compatibility surface

| Surface | Current reference and effect |
| --- | --- |
| Repository/package identity and generated docs | `DESCRIPTION`, `README.md`, `_pkgdown.yml`, and generated `man/planscanR.screen-package.Rd` embed the package/repository/site identity. `NAMESPACE` exports APIs but has no `planscanR.screen-package` entry; check roxygen output if package documentation changes. |
| Dependency metadata | `planscanR.biogain/DESCRIPTION` lists `planscanR.screen` in `Imports` and its GitHub repository under `Remotes`. It owns BIOGAIN configuration in `R/curation_config.R`. |
| Callers and documentation | BIOGAIN curation adapters call screen in `R/biogain_review_app.R`, `R/curation_config.R`, `R/curation_wrapper.R`, `R/curation_translation.R`, and `R/curation_import.R`; scoring callers include `R/select_assessments.R` and `R/sync_yoda.R`. Operator/docs callers include `inst/runbook/biogain_acquire.R`, `inst/runbook/biogain_select_download_rag.R`, `README.md`, `vignettes/runbook.Rmd`, and `AGENTS.md`. Parent `CLAUDE.md` and `Makefile`, plus BIOGAIN's `AGENTS.md`, must be reviewed. `planscanR.RAG/AGENTS.md` mentions screen in architecture guidance; RAG imports only `planscanR`. |
| Install and dependency order | Parent `Makefile` lists and installs `planscanR.screen` between `planscanR` and `planscanR.biogain`; its dependency comments and app checks also use that name. |
| Deployment | `planscanR.biogain/inst/deploy/install-native.sh:93` installs the literal package set including `planscanR.screen`; line 100 verifies its namespace/version. `prepare-native.sh` sets `CURATION_R_LIB` (default `.curation-Rlib`) and writes it as `R_LIBS_USER`. Deployment entrypoints also load the package: `biogain-curation-runner.R:106`, `biogain-auth-gate-runner.R:88`, and `enroll-account.R:67`; `R/curation_wrapper.R:236` checks its namespace and line 249 invokes its launcher. Update these with the install instructions and deployment library configuration. |
| Saved model class and API | `train_selection_model()` creates class `planscanR_selection_model`. `save_selection_model()` wraps `saveRDS()`; `load_selection_model()` calls `readRDS()` and checks only that class. It does not validate package version, feature schema, or namespace migration. |
| Saved model contents | A model stores a fitted workflow, learner name, feature names, topics, labels, include fields, `threshold`, training metadata, out-of-fold predictions and metrics. Its serialized R objects may contain closures, formulas, or namespace references. A matching class string alone does not establish that an old model can be loaded and predicted by a renamed package. |
| Curation artifacts | C3 artifact-store contracts retain scoped/versioned snapshots, reviewer samples, model versions, provenance, revisions and active-model pointers. The BIOGAIN wrapper defines the review database as `reviews.sqlite` and artifact root as `artifacts/` (`planscanR.biogain/R/curation_wrapper.R`). The generic store implementation is `planscanR.screen/R/curation_artifact_sqlite.R`; its `curation-artifacts.sqlite` database and `snapshots/`, `samples/`, and `models/` content must keep their scope, paths, schema, and provenance across the transition. |

## Transition required before a rename

Treat the selected rename as a coordinated migration of the existing
repository/package. Before changing a deployed installation:

1. Complete the package-name reference inventory across DESCRIPTION metadata,
   namespaces, `Remotes`, docs/site, scripts, parent install/check targets,
   BIOGAIN callers, deployment installation/entrypoints, and operator
   instructions. Record locations and owners, including external install and
   deployment configuration available to the team.
2. Preserve backups of the installed old package/library and the relevant
   curation state: reviews databases, snapshots, samples, model artifacts and
   active-model metadata. Record their paths, schema/version and scope.
3. Update repository identity, package metadata, dependency metadata and all
   known callers as one release. Provide an agreed installation order and
   deployment procedure so no caller expects the new package while only the
   old one is installed.
4. Test representative saved models from the old installation in a clean
   installation of the renamed package, with the old package unavailable.
   Verify both load and prediction parity on fixed representative records and
   features. Audit serialized objects for old namespace references. If loading
   or parity fails, define and approve a specific conversion path before
   migration; do not infer safety from the S3 class name.
5. Verify that reviews, scoped snapshots, samples, model versions, provenance,
   revisions and active-model publication still resolve from their retained
   paths and schemas. No artifact may silently move to a new scope or be
   discarded as a package rename side effect.
6. Document a rollback that restores the pinned old package/library together
   with compatible state and deployment configuration. Retain the old install
   and backups until the migration gates pass and rollback is no longer needed.

These gates and their acceptance evidence belong to the future, separately
authorized migration; their evidence is not a prerequisite for completing
ACQ-N1. Keep the current package identity in use until that migration is
authorized and its gates pass. No compatibility package is planned.
