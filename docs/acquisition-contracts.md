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


## Reconciliation and sidecar synchronization

Download completion checks the actual leaf verification receipt before
registering bytes. The worker synchronizes attachment outcomes through the
leaf's locked latest-row callback, retaining classification, relevance and
translation fields. Existing sidecars must match the persisted country,
portal and native project ID; ambiguous URL matches are blocked. A newly
discovered attachment appends its outcome to an identity-matched sidecar.
A missing explicit legacy sidecar is blocked. Catalog-only sources without
an existing implicit sidecar return `sync_status = "skipped"`; the worker does
not invent project metadata or claim that a sidecar was updated.

If synchronization fails, verified file metadata is retained with the failed
or retryable attempt. Normal retry reuses the receipt-backed file. If a crash
exhausts the last allowed download attempt before catalog completion, run
`acquisition_queue_recover()` and then
`acquisition_queue_reconcile_job(catalog, job_id, cache_root)`. This explicit
local operation accepts only failed or blocked download jobs, checks the
canonical publication receipt, obtains a new fenced lease, synchronizes the
sidecar and registers the file. It never calls the downloader or resets
`max_attempts`. Its extra attempt is identified by worker and result metadata
as `receipt-only-reconciliation` / `receipt-only`; prior attempts remain in
the audit, and the total attempt count includes local reconciliation.
Missing, partial and unverified bytes cannot enter this route. An active
lease cannot be replaced. If synchronization is interrupted again, the same
explicit local operation can be retried after recovery.

`acquisition_queue_reconcile_file()` checks an already registered verified
claim against its actual receipt and synchronizes its source sidecar.
`acquisition_queue_audit_cache()` delegates to the leaf audit and reports
verified, unverified and partial files without promoting or deleting them.
Imported size/hash claims alone never authorize receipt creation.

## Campaign selection and priority contract (ACQ-2a)

`acquisition_campaign_submit()` records one immutable campaign submission. The
submission references an already registered model and input snapshot, and
retains its threshold, policy version, actor, selected source identities,
selection probabilities, energy evidence, and ordered priority rules. A model
artifact is never copied into a mutable campaign row: the existing immutable
`acquisition_models` reference remains authoritative. Reusing the same
submission identity with different content is rejected.

The caller supplies `priority_rules`; the screen package has no preferred
country list or energy default. A rule may constrain countries and energy tags,
and carries a stable rule ID, explanation, and integer queue priority. Energy
tags are normalized to `wind`, `solar`, `mixed`, or `unknown`; evidence,
confidence, and manual overrides stay with each campaign item. A mixed item
matches a rule when one of its component tags matches. An unknown item only
matches an explicit unknown or unrestricted rule. The matching rule and reason
are persisted so queue order can be explained later.

Submitting a source that already has a discover job reuses that durable job;
the campaign item records the existing job ID and a `reused` outcome. This
keeps equivalent submissions from duplicating work. Discovery-created download
jobs inherit the campaign association and priority when available.

`acquisition_campaign_reprioritize()` records a new priority-rule revision.
It updates only campaign jobs in `queued` or `retry_wait`; running leases and
their active transfers retain their current priority and rule. Every revision
is retained for audit, and `acquisition_campaign_explain()` reports the current
rule, energy evidence, queue state, and whether a reprioritization changed a
pending job. Reprioritization is an operational queue decision and does not
rewrite the immutable selection submission.
