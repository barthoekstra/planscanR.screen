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
