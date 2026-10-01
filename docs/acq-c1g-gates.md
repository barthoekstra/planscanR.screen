# ACQ-C1g generic package gates

The generic curation package is verified as an installed package in a library
that contains `planscanR` and the screen dependencies but does not contain
`planscanR.biogain`. The installed smoke supplies every curation boundary
explicitly: configuration, records provider, review repository, artifact
store, and trusted actor resolver. It constructs the exported
`curation_app()` and can serve `run_curation()` on loopback with a deterministic
two-record fixture. No cache, Python model, or remote service is used.

The C1g wrapper smoke runs in a separate installed library that includes
BIOGAIN. It calls `biogain_prepare_curation()` with explicit repository,
artifact, provider, and actor seams, verifies that a partial selection-control
override retains the other BIOGAIN defaults, constructs the generic app, and
serves it through `biogain_run_prepared_curation()`. The existing public
`run_biogain_review()` launcher is not switched by C1g; that remains a C4
cut-over after C3 persistence and full parity.

The durable harness runner is
`.claude/state/acq-c1-evidence/run-acq-c1g-gates.sh` in the family workspace.
It expects a fresh R 4.6.1 `.Rlib`, installs only the local package sources
into temporary smoke libraries, runs both source suites, regenerates roxygen
documentation, checks styler output, and records baseline-matched package
checks. The target baseline is 0 errors / 0 warnings / 0 notes for
`planscanR.screen` and 0 / 2 / 0 for BIOGAIN's two existing warnings. Any
change in those flags is recorded as a gate failure for review; warnings are
not hidden by changing tests or check settings.

The gate is offline. It does not import legacy CSV/RDS rows, mutate cache
files, download models, or contact a portal. Legacy conversion remains an
explicit `biogain_import_legacy_reviews()` operation with caller-supplied
identity mapping.
