# Generic curation contracts

`planscanR.screen` owns the configuration and callback contracts used by the
generic curation workflow. A caller constructs these values once and injects
them into the app or a service. This package does not choose topics, labels,
lexicons, branding, cache roots, reviewers, translation engines, or persistence
backends. In particular, the contracts contain no BIOGAIN defaults and do not
read options or environment variables.

## Identity and configuration

```r
scope <- curation_scope(project_id, corpus_id, config_id)
config <- curation_config(
  project_id, corpus_id, config_id,
  topics, labels, lexicon, select_fn, selection_controls,
  funnel_stages, branding, artifact_paths, translator = NULL
)
```

`project_id` names the curation workspace, `corpus_id` names the source
corpus, and `config_id` names the versioned policy. The complete scope is
passed to repository and artifact callbacks. Artifact identity always includes
all three values, so changing a configuration cannot silently reuse a snapshot
or model from another policy version. Review rows retain their project/corpus
scope while authored decisions remain keyed by country, native document ID,
and reviewer ID.

`topics`, `labels`, and `lexicon` are required. `select_fn(records, controls)`
returns the input rows with a logical `selected` column. Selection controls
are explicit named values; a per-call override is passed to
`curation_apply_selection()`. Each funnel stage is
`list(key, label, fn)`, where `fn(records, context)` returns a logical mask.
Branding requires `title` and `primary`; artifact paths require an explicit
`root`. A translator is either `NULL` or an injected translation provider.

## Records

```r
provider <- curation_records_provider(read_fn, refresh_fn = NULL)
records <- curation_records_read(provider, corpus_id, countries = NULL)
records <- curation_records_refresh(provider, corpus_id, countries = NULL)
```

Both callbacks accept `corpus_id` and `countries`. They return a data frame
with non-empty `country` and `document_id` columns. The convenience adapter
`curation_records_provider_cache(cache_dir, index_fn = planscanR::index_cache)`
receives an explicit cache root and calls the exported `planscanR` index API;
it never resolves a process-wide cache setting or reimplements sidecar I/O.

## Actors, keys, rows, and results

Authentication is deployment-owned:

```r
resolver <- curation_actor_resolver(function(request) authenticated_actor)
actor <- curation_resolve_actor(resolver, request)
```

`authenticated_actor` is a `curation_actor(id, roles, display_name)` created by
a trusted resolver. A UI display-name field or request header is not an actor
credential. The actor is supplied to every repository and artifact operation.

The shared review key is created by
`curation_review_key(country, document_id, reviewer_id)` and has exactly these
identity fields:

| Field | Meaning |
| --- | --- |
| `country` | Source-country namespace for the native ID. |
| `document_id` | Native document ID within `country`. |
| `reviewer_id` | Stable authenticated reviewer ID; display names are metadata only. |

Canonical review rows returned by `read`, `list`, and `export` use these
columns:

```text
country, document_id, reviewer_id, decision, source, note,
reviewed_at, sidecar_path, revision, deleted
```

`decision` is a non-empty configuration-defined label. `source` is a
non-empty provenance value such as `browse`, `random`, `blind`, `stepper`,
`bulk`, or `import`; the generic layer does not select a project vocabulary.
`reviewed_at` is a UTC timestamp, `note` and `sidecar_path` are nullable, and
`revision` is a non-negative integer. `deleted = TRUE` denotes a clear
tombstone; a tombstone keeps its key and revision and is returned by reads.
The review store must not collapse the composite country/document key or
overwrite another reviewer’s row.

Imported rows additionally carry non-empty `import_source` and
`import_row_id` values, plus an optional `import_fingerprint`. The wrapper
validates these fields and preserves them for C2, so a repeat import can be
matched to its source row without losing legacy provenance.

Repository callbacks use these signatures:

```r
curation_review_repository(
  read_fn, list_fn, upsert_fn, clear_fn,
  bulk_upsert_fn, import_fn, export_fn
)
```

The callbacks receive `scope`, a trusted `actor`, and the operation payload.
`list_fn(scope, filter, actor, limit, offset)` is paginated. Every mutation
has a required `expected_revision`; the wrappers reject `NULL`, missing,
fractional, negative, non-finite, and out-of-range values. `0` means the row
has never existed. A clear must preserve a revisioned tombstone, so an old
client cannot recreate a row after clear (the ABA case). `bulk_upsert_fn` and
`import_fn` receive one expected revision per row and are atomic.

Reads return `NULL` for a never-seen key or one canonical row, including a
tombstone. Lists and exports return canonical data frames. Successful writes
return a result with `ok = TRUE`, the composite `key`, the resulting
`revision`, and `deleted`; a conflict is an error of class
`planscanR_error_curation_conflict` carrying expected/current revision
details. Other contract failures use `planscanR_error_curation_repository`
or `planscanR_error_curation_revision`. C2 owns transaction details and the
import rule: a repeated import may be an idempotent no-op only when its mapped
source namespace/row ID and payload still match; it must never overwrite a
later live edit.

The local SQLite implementation is opened with an explicit file path:

```r
repo <- curation_review_repository_sqlite("/data/biogain/reviews.sqlite")
```

It enables WAL and foreign keys, applies numbered migrations in one short
transaction, and configures a finite busy timeout. The database keeps the
`(project_id, corpus_id, country, document_id, reviewer_id)` key; `config_id`
is retained as provenance. `curation_review_clear()` writes a tombstone and
increments its revision, so an old client cannot recreate a cleared row. Bulk
updates and imports validate every revision before writing and roll back as one
transaction. Imported events are matched by `(project_id, corpus_id,
import_source, import_row_id)` and their payload fingerprint. A matching event
is a no-op even after a later edit or clear; a changed payload is a conflict.
CSV export is an in-memory data-frame result (or can be written through the
repository's `export_csv()` helper), so SQLite remains the sole authoritative
review store. Rows also retain a backend `modified_at` audit timestamp; clearing
a row preserves its authored `reviewed_at`, note, source, and import provenance
on the tombstone.

## Snapshot, sample, and model artifacts

```r
store <- curation_artifact_store(
  paths, snapshot_read_fn, snapshot_write_fn,
  sample_list_fn, sample_read_fn, sample_write_fn,
  model_list_fn, model_read_fn, model_write_fn,
  model_publish_fn, active_model_fn
)
```

Every callback receives the complete scope and trusted actor. Snapshot writes,
sample writes, and model writes require `expected_revision`; model publication
uses `expected_active_revision`. Snapshot and sample identities include corpus
and configuration. Model writes carry the existing selection-model payload plus
training-review/configuration/creator provenance. `model_publish_fn` changes
the active reference atomically; a failed publication leaves the prior active
model usable. C3 supplies concrete persistence and retains existing model
serialization. The minimum result fields are:

| Seam | Minimum read/list fields | Minimum write/publication fields |
| --- | --- | --- |
| Snapshot | `snapshot_id`, scope/config identity, `version`, `revision`, path/payload | `ok`, `snapshot_id`, `version`, resulting `revision` |
| Sample | `sample_id`, `reviewer_id`, scope, `revision`, payload/metadata | `ok`, `sample_id`, `reviewer_id`, resulting `revision` |
| Model | `model_id`, scope/config identity, `revision`, provenance, payload/metadata | `ok`, `model_id`, resulting `revision` |
| Active model | `active_model_id`, `active_revision`, provenance | `ok`, `active_model_id`, resulting `active_revision` |

Absent reads return `NULL`.

## Optional translation

```r
provider <- curation_translation_provider(
  translate_fn, service_id,
  source_language_fn = NULL,
  cache_read_fn = NULL, cache_write_fn = NULL,
  write_gate_fn = NULL, write_policy = "disabled"
)
result <- curation_translate(provider, record, key, source_language,
                             target_language, write = FALSE)
```

`NULL` provider support returns original title/summary with
`status = "disabled"`. A provider callback is injected and never imports a
translation runtime, edits Python settings, or selects a model implicitly.
Worker failures, including a `NULL` or classed failure result, return
`status = "error"` and preserve source text. Cache callbacks receive a key with
`country`, `document_id`, and `target_language`, so different target languages
cannot reuse one another's translations. Shared cache writes require
`write_policy = "serialized"`, a cache writer, and a per-record
`write_gate_fn(key, write_fn)`; the gate key excludes `target_language` so
different target writes for one record serialize. Otherwise writes fail closed.

## Enrichment, sampling, and queues

The generic records service reads a corpus through the injected provider and
derives the scalar fields consumed by the review UI:

```r
snapshot <- curation_records_snapshot(provider, config, countries = countries)
```

`curation_records_snapshot()` is pure. It never reads or writes an RDS file;
C3 supplies snapshot persistence through the artifact-store contract or a test
double. `curation_enrich_records()` recomputes `cosine_max` from the topic
columns named by `config$topics` and recomputes all keyword columns from
`config$lexicon`, so a new configuration cannot reuse a previous keyword
total. Attachment and download list columns are reduced to scalar counts and
then removed by default.

Sampling and progress are likewise pure helpers:

```r
sample <- curation_draw_random_sample(snapshot, countries, n_per_country, seed)
queue <- curation_build_review_queue(
  snapshot, reviews, reviewer_id, countries, n_per_country,
  seed = seed, scope = config$identity
)
progress <- curation_country_label_counts(
  snapshot, reviews, countries, reviewer_id, scope = config$identity
)
```

The queue first returns records reviewed by another reviewer in the explicit
project/corpus scope (`attr(queue, "mode") == "validate"`) and then falls back
to an unseen stratified sample (`"fresh"`). Country is part of every queue and
sample identity. Calls with an explicit seed restore the caller's RNG state,
including when `.Random.seed` did not exist before the call; an omitted seed
advances the caller's normal RNG stream. Review rows may use canonical
`reviewer_id` or the legacy `reviewer` column; explicit project/corpus columns
are filtered when present.

## Model and evaluation adapters

Model callers pass one explicit `planscanR_curation_config`; its named
`topics` and `labels` vectors determine the feature columns. The adapters keep
the existing model class and serializer:

```r
model <- curation_train_model(
  snapshot, reviews, config,
  learner_key = "logistic", eval_source = "random", seed = 42
)
predictions <- curation_predict_model(model, snapshot, config)
metrics <- curation_model_metrics(model, config, by_country = TRUE)
curve <- curation_learning_curve(snapshot, reviews, config)
```

Training and learning curves delegate to the existing selection APIs. The
default random source is filtered before unanimous `consensus_reviews()`;
evaluation therefore excludes browse labels from the held-out/random metrics.
`curation_selection_vs_human()` provides the same source filter for heuristic
selection output, and `curation_inter_reviewer_summary()` reports agreement
over keep/drop/unsure decisions. `curation_load_model()` accepts legacy RDS
selection-model artifacts and validates their topic/label vectors when a
configuration is supplied.

Callers may inject a test-double artifact store and use
`curation_publish_model()`, which writes through
`curation_artifact_write_model()` and publishes only after a successful write;
callback errors and explicit failed mutation results propagate to the caller.

### Concrete C3 artifact persistence

The first concrete artifact backend is `curation_artifact_store_sqlite()`.
Callers provide an explicit `paths$root` (and may override `database`,
`snapshots`, `samples`, or `models` with paths that remain below that root) and
an optional bounded SQLite busy timeout. `DBI` and `RSQLite` remain `Suggests`; constructing the
backend reports a classed missing-dependency error when either is unavailable.
The backend uses the same local SQLite file for metadata, revision counters,
and active-model references. It enables WAL and foreign keys, runs numbered
migrations, and holds a write transaction only while registering one already
written artifact or changing the active reference.

Snapshots are immutable rows keyed by the complete project/corpus/config scope
and a content-derived `version`. `curation_snapshot_version()` hashes a
canonical snapshot payload together with a corpus-content fingerprint, the
configuration fingerprint, the enrichment-schema version, and explicit filter
coverage (requested countries and per-country row counts). The records helper
attaches these values to snapshots it builds. A changed title, summary, scored
field, configuration content, enrichment schema, or filter coverage therefore
creates a new version even when native document IDs are unchanged. A read with
`version = NULL` returns the newest matching version; it never assumes that the
latest version is valid for a caller's current corpus without a matching
version assertion.

Samples are immutable RDS payloads keyed by scope, reviewer ID, and sample ID.
The backend filters lists by the supplied reviewer and rejects reads by another
reviewer unless the trusted actor has an administrative role. Country and
document identity remains in the payload; native IDs are never used as a
cross-country key.

Models are written as opaque, uniquely named RDS files under the explicit model
root. The payload remains an ordinary `planscanR_selection_model`, so existing
`curation_load_model()` and `load_selection_model()` can read it. A short
transaction registers the path and provenance only after an atomic file write;
an orphan from a failed registration is invisible and cannot replace an active
version. Model rows are immutable and carry structured provenance including the
review snapshot fingerprint, snapshot version, configuration fingerprint,
corpus snapshot/version, training parameters, creator actor, and creation
time. A conflicting creator or non-zero expected revision is rejected.

Publication validates the scoped model row and loads its RDS before a short
compare-and-swap transaction updates the active reference. The expected active
revision must match, so two independent publishers cannot overwrite one
another. Missing, malformed, or cross-scope targets fail before the reference
changes; the previous active model remains readable after every failed
publication. Legacy `selection_model.rds` files are read as import inputs and
are never replaced automatically.
