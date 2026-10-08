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

## Independent worker health contract (ACQ-RUN3)

Worker liveness is persisted in `acquisition_worker_health`, separately from
`acquisition_job_attempts`. The generic API bounds configured identities and
reads to 32; BIOGAIN supplies its default two trusted identities.
`acquisition_worker_health_configure()` records expected
identities as `unknown`; `acquisition_worker_health_start()` publishes an
initial `idle` heartbeat; `acquisition_worker_health_heartbeat()` publishes
`idle` or `busy`; and `acquisition_worker_health_stop()` records a graceful
`stopped` exit or a redacted `error` state. A hard crash leaves the last
`idle`/`busy` row in place and is reported as `stale` once its heartbeat is
older than the requested freshness window. An expected identity with no
heartbeat remains `unknown`.

`acquisition_worker_health_status()` reads at most the configured expected rows
(bounded to 32) and
returns `workers`, `counts$expected`, `counts$alive`, and
`counts$available`. Only a fresh `idle` worker is available; fresh `busy`
workers are alive but unavailable. Queue attempts are never used to infer
pool liveness, and health rows contain no byte or completed-file fields.
The CLI accepts `--expected-workers=id1,id2`, publishes lifecycle state across
startup, idle polling, errors, and exit, and remains independent of Shiny.
Dashboard code may consume this same status result without owning worker
lifecycle or granting shell controls.


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

## Verified background ingestion (ACQ-4a)

Document ingestion is a durable `ingest` job in this catalog and remains
separate from the default discovery, download, and storage-migration workers.
Only an immutable file version with a current verified planscanR receipt may
be enqueued or passed to the injected RAG handler. Receipt verification and
SHA-256 checking happen again immediately before the handler call. Missing,
changed, or unverified files are blocked and never reach RAG. Ingestion runs
outside the short queue transaction and uses the existing lease heartbeat and
token-fenced completion, so a worker that loses its lease cannot publish a
successful ingestion outcome.

The screen worker accepts an optional `ingest` callback; it does not import or
require planscanR.RAG. The callback contract is
`ingest_verified_file(file_path, sha256, document_id, country,
version_id = NULL, cache_dir, embedding_model, ...)`, returning
`list(status = "succeeded", result = <one-row summary>, provenance = <list>)`.
It returns `status = "retryable"` with an `error` for retryable processing
failures, or `status = "blocked"` when its independent receipt validation
rejects the file. The caller maps those outcomes through the shared queue's
existing attempt state machine. Job identity includes the immutable version
identity (and content digest), so successful work for one version does not
mark other versions complete; retrying a failed stage uses the same durable
job and attempt history. Ingestion failures are recorded independently and do
not alter download job state.

The RAG handler deduplicates costly document processing by verified content
hash while retaining every distinct document, country, and version association
in its provenance. Backlog and resource reporting comes from persisted queue
states and observed attempt start/finish times and byte counts. Unknown
measurements remain unknown; the dashboard does not infer throughput or
capacity from configured limits alone.

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
## Storage administration and relocation contract (ACQ-2c)

The leaf `planscanR` package owns physical storage. Its exported
`storage_root_validate()`, `storage_path()`, `storage_relative_path()`,
`storage_capacity()`, `storage_free_bytes()`, and
`storage_transfer_cap_bytes()` functions accept canonical absolute roots and
portable relative locations. Relative locations reject absolute paths, dot
segments, and symlink escapes below a registered root. The screen package
persists only the root ID and relative path; it does not persist
machine-specific absolute file locations in file claims.

The screen package keeps storage policy in a separate numbered ledger after
the catalog migrations. `acquisition_storage_roots` records validated roots,
purpose, and fixed or fractional total-volume headroom. Settings are append-only
versions in `acquisition_settings_versions`; storage administration requires an
authenticated actor with the explicit `admin` role, and secret-like setting
keys are rejected. The built-in cache root is registered when the leaf resolver
is available, with a default estimate bounded by the leaf transfer-size cap for
transfers whose size is not known before download.

Download and storage-migration jobs carry an optional byte estimate. During the
same short claim transaction, the queue reads total and free bytes from the
leaf, subtracts all held reservations and the new estimate, and refuses the
claim when the configured headroom would be crossed. Download reservations and
the downloader share the leaf transfer-size cap. The claimed root ID and
validated path stay bound to that lease. Successful,
retryable, failed, and expired attempts release their held reservation. SQLite
transaction serialization makes two worker connections observe the same
reservation ledger.

`acquisition_storage_enqueue_relocation()` records source and target roots in
`acquisition_storage_relocations` and enqueues a distinct `migrate_storage`
job for each source/target pair, targeted at the target root. It never moves
files inline. A deployment may supply a worker handler to copy, verify, and
switch references; the default handler blocks until that explicit migration
policy is configured.

The existing receipt-only reconciliation helpers retain their `planscan-cache`
recovery root behavior; custom-root recovery is deferred to ACQ-5a.

## Acquisition dashboard and controls (ACQ-2b)

`acquisition_dashboard_summary()` and the queue views read the acquisition
SQLite catalog directly. Refreshes use bounded SQL aggregates and paginated
project, file, and job detail queries; they never scan sidecars or take
ownership of a worker. Project, file, and stage counts are derived from
persisted source, document, file, and job rows. A source with a successful
discovery and zero documents is reported as `successful_empty`; a source with
documents is `complete` only when every in-scope document has a verified file,
`pending` while discovered documents await download, `partial` when some have
a file claim, and `blocked` when a document download is blocked or exhausted.
A successful non-empty discovery with no download work is `discovered`. The
authoritative `discovered_count` counts successful non-empty discovery even
while downloads are pending. Missing, retryable, or failed inventory is
reported with its explicit unknown/error state. Unknown totals remain visible
and are never presented as zero. File totals use only verified registered file
claims, while the dashboard reports unknown byte totals when no trustworthy
size is available.

The dashboard exposes active worker IDs, lease expiry, and the latest attempt
heartbeat from the queue database. Progress is a database projection of job
and attempt state (including completed and unknown work); ETA is omitted when
the catalog cannot support a measurement. A small Shiny module may refresh
these projections with `reactivePoll()` or a timer, but it does not start,
stop, supervise, or join workers. Closing a browser session therefore leaves
independent workers and their leases untouched.

The queue UI exposes campaign, country, energy, state, portal, and text
filters with server-side page navigation, so a control target outside the
first page remains addressable. Administrative queue actions are server-side
functions, not editable actor fields in the UI. Each action resolves a trusted
`planscanR_curation_actor_resolver` and requires the `admin` role. The catalog
records the authenticated actor, action, target, prior state, resulting state,
reason, and timestamp in an append-only audit table. Pause prevents new claims
for the selected campaign or job while already-running work is allowed to
finish. Resume removes that claim guard. Retry only requeues an eligible
failed/blocked/cancelled job after preserving its prior attempts; it does not
replace an active lease or reset its attempt audit. Cancel marks pending work
cancelled and requests a fenced safe stop for running work. A running worker
may publish a verified receipt before its completion is recorded as cancelled;
the dashboard retains that verified file claim. Queue claim and completion
guards enforce these states in the database, so a forged browser request
cannot mutate identity or bypass a lease.

## Supervised acquisition deployment contract (ACQ-3a)

The acquisition deployment is additive to the curation deployment. It uses a
dedicated SQLite catalog under the service-owned acquisition state root and
never opens, migrates, or writes the review database, authentication database,
planscanR cache, or workspace R library. Catalog provisioning is idempotent and
must leave the existing curation service and its state available. Catalog,
WAL/SHM files, worker logs, and deployment configuration stay under that
explicit state root; credentials, session cookies, TOTP material, and
credential-bearing URLs are excluded from logs and shared evidence.

The worker entry point supports a supervised idle mode. Each worker is an
independent fresh R process outside Shiny and claims through the existing
SQLite lease protocol. It may sleep between empty claims, but a browser or
dashboard session never owns its lifecycle. The supervisor restarts a worker
after an unexpected exit; an expired lease is recovered by the next claim and
the job remains retryable. The initial single-host pool contains two workers
and may be expanded to four only after measurement.

Worker units carry explicit per-process CPU and memory limits and expose those
limits through systemd cgroup properties. ACQ-3a evidence must measure the
actual worker RSS and cgroup CPU/memory observations using an isolated fixture
catalog and injected handler; a unit template or nominal limit alone is not
evidence. The cap applies to acquisition workers only and must leave the live
curation service uncapped and available.

The dashboard runs as a separate Shiny process against the acquisition catalog
and resolves actors with the existing server-side auth SQLite session resolver.
The public `/app/acquisition/` route is protected by the same nginx auth
subrequest as `/app/`, uses the existing `/app` cookie path, strips client
identity/role headers, and forwards no credentials. Reviewer access remains
read-only; queue controls still require the authenticated `admin` role through
the existing actor resolver. The route must preserve the curation app at
`/app/` and may not duplicate or migrate review persistence.
