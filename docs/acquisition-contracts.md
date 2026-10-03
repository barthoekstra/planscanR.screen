# Acquisition catalog contracts (ACQ-1a)

`planscanR.screen` owns a small, local SQLite catalog for acquisition state.
It is separate from the review database and from the planscanR cache. The
catalog is opened with `acquisition_catalog_open()` and uses SQLite WAL,
foreign keys, a finite busy timeout, and numbered idempotent migrations.
`acquisition_catalog_transaction()` is the short transaction seam used by
importers and later queue workers; callers must keep network, model, and file
work outside that transaction.

The catalog preserves source identity as the composite
`(country, portal, native_project_id)`. A source ID reused by another country
or portal therefore creates a distinct source reference. Attachment records
are scoped to their source reference. An attachment without a trusted native
attachment ID is retained with `identity_status = "ambiguous"` and a stable
source/position key; it is never merged solely because its URL matches another
record.

`acquisition_catalog_import_records()` accepts an offline data frame from the
exported `planscanR::index_cache()` API (or an injected equivalent). It keeps
source URLs, attachment URLs, relative file claims, download status, and
legacy paths as provenance. Existing non-empty files imported from legacy
records are recorded as `verification_status = "unverified"`; only a later
integrity check may mark a file verified. Repeating an import is a no-op when
the `(import_source, import_row_id)` payload fingerprint is unchanged. A
changed payload for that key is rejected instead of silently replacing
history.

Selection rows reference an immutable input snapshot and an immutable model
reference. Model rows retain the artifact path, digest, and metadata; the
selection row records the model reference and serialized selection metadata.
An existing immutable reference cannot be overwritten with different content.
The existing SQLite review repository remains authoritative for review state;
the acquisition catalog never opens or migrates a review database.

The acquisition database, its WAL/SHM files, catalog artifacts, and retained
model files are backed up by the deployment/operator responsible for the
explicit acquisition state root. A backup must use SQLite's consistent backup
operation (or coordinated quiescence) and include model/artifact files. The
planscanR cache remains a separate, rebuildable input and is not the catalog
backup.

## Durable queue and worker contract (ACQ-1b)

The durable queue extends the same acquisition catalog database. It does not
open the review database and it does not use the planscanR cache as a queue.
Queue migrations add `acquisition_jobs`, `acquisition_job_attempts`, and
`acquisition_host_limits`. A job has one stable idempotency key, a kind of
`discover` or `download`, and a persisted target identity (`source_ref_id`,
`document_ref_id`, or `file_ref_id`). Re-enqueuing the same target and kind is
an idempotent no-op.

Claims run through `acquisition_catalog_transaction()` and use a short
`BEGIN IMMEDIATE` critical section. The claim selects one queued, retry-ready,
or expired-running job in deterministic priority order, creates an immutable
attempt row, and writes a fresh unique lease token, worker ID, and expiry. No
network, model, or long-running file work is allowed in this transaction.
Heartbeats and completion require the current job ID and lease token; the
completion update also requires an unexpired lease. A stale or expired worker
therefore receives a zero-row result and cannot update catalog verification or
job state for a newer attempt.

Every attempt records its outcome. Retryable failures move the job to
`retry_wait` with bounded exponential backoff and a persisted retry time;
blocked, exhausted, cancelled, and successful outcomes remain distinguishable.
Host concurrency and cooldown are persisted in `acquisition_host_limits` and
are checked and reserved in the same claim transaction. The initial worker
pool is the ACQ-1d bound of 2--4 supervised R processes on one local host.

The worker entry point is an independent supervised R process, outside Shiny
and without forking a Python-loaded process. Its built-in discovery adapter
calls the leaf `resolve_attachment_inventory()` contract over persisted
catalog identities, preserving `success`, authoritative `empty`, `retryable`,
and `blocked` outcomes. Unknown or failed inventory never becomes an empty
project. A fresh portal resolver may be injected by a supervised worker; its
returned document identities are registered and their download jobs are
enqueued in the same fenced completion transaction.

On Linux, the worker checks `/proc`, Python, and Shiny state before each claim,
then starts a short-lived database-only heartbeat child after the claim. The
child performs an immediate token-fenced renewal, renews at a bounded interval,
and stops on a sentinel or a dead parent; a failed guard prevents handler work
or makes completion retryable.

Download targets use a deterministic identity composed from the persisted
source and document references as the planscanR storage document key, so
`(country, portal, native_project_id)` collisions cannot reuse the legacy
`(country, native_project_id)` path. A per-canonical-path OS lock surrounds
the planscanR download call and uses `overwrite = FALSE`; an interrupted or
stale attempt may leave only an unreferenced staging/verified receipt, while a
later attempt reuses an existing verified copy instead of overwriting it.
Injected handlers remain available for deterministic offline tests.
