# ACQ-1d database decision

## Decision

Use SQLite for the acquisition catalog and durable queue on the target single
Linux host. The bounded operating assumption is 2–4 supervised worker
processes on the planned 8-core/32-GB machine, with the database and WAL on
local disk. Keep the existing SQLite review store authoritative and keep
DuckDB as the rebuildable RAG index. This decision does not add PostgreSQL,
pgvector, a production queue, or a catalog implementation.

The measured probe supports this bound: four independent writers completed the
synthetic workload without duplicate claims, busy failures, or stale
completion. It is evidence for the design gate, not a throughput guarantee for
downloads or portal traffic.

## Evidence and limits

The reproducible probe is
[`inst/bench/acq-1d-sqlite-probe.R`](../inst/bench/acq-1d-sqlite-probe.R), and
the captured output is
[`inst/bench/acq-1d-sqlite-evidence.tsv`](../inst/bench/acq-1d-sqlite-evidence.tsv).
It creates disposable databases under `tempdir()`, starts independent
`Rscript` processes, and writes no cache or production database.

Measured on 2026-10-03 with:

```sh
R_LIBS_USER=/data/biogain-docs/.Rlib \
  Rscript --vanilla inst/bench/acq-1d-sqlite-probe.R \
  --output inst/bench/acq-1d-sqlite-evidence.tsv
```

The runtime was R 4.6.1, DBI 1.3.0, RSQLite 3.53.3, and embedded SQLite
3.53.3. `PRAGMA journal_mode` reported `wal`.

| independent workers | jobs | elapsed | claim p50 / p95 / max | completed | unique tokens | duplicate claims | busy retries |
| ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 1 | 24 | 411.06 ms | 9.78 / 14.38 / 15.57 ms | 24 | 24 | 0 | 0 |
| 2 | 24 | 449.65 ms | 10.25 / 15.46 / 33.73 ms | 24 | 24 | 0 | 0 |
| 4 | 24 | 451.75 ms | 10.37 / 63.33 / 94.35 ms | 24 | 24 | 0 | 0 |

Elapsed time is wall time from barrier release until all child processes finish;
claim p50/p95/max is measured inside the claim path and excludes the 3 ms of
post-claim simulated work. Each claim held `BEGIN IMMEDIATE` for 8 ms to expose
serialized-writer wait. The four-worker run therefore measures contention under
a deliberately visible short critical section. It does not measure network
downloads, review UI writes, storage throughput, or a PostgreSQL instance.

The crash scenario independently claimed a job, killed the worker while a
second transaction was open, reopened SQLite, and waited for the lease to
expire. The uncommitted marker count was 0 after recovery, the expired job was
reclaimed as attempt 2, the stale token update affected 0 rows, the new token
update affected 1 row, and the final state was `succeeded` with two unique
tokens. These results cover rollback after a killed writer, recovery, claim
uniqueness, and stale-completion fencing in the disposable schema.

## Claim protocol for ACQ-1a/b

The later implementation should keep the write critical section short:

1. Open the local database with foreign keys, WAL, and a finite busy timeout.
   Retry a busy transaction with bounded backoff and surface exhaustion.
2. Start `BEGIN IMMEDIATE`; select one eligible `queued` job, or one expired
   `running` lease, in deterministic priority order.
3. In that same transaction, increment the attempt, write a newly generated
   unique lease/attempt token and expiry, record the worker and attempt row,
   then commit. Do not hold the transaction during discovery, download,
   classification, translation, or publication.
4. Heartbeats renew the current token in a short transaction. A worker that
   dies leaves an expired lease for a later claim.
5. Completion uses `UPDATE ... WHERE job_id = ? AND state = 'running' AND
   lease_token = ?` and requires exactly one affected row. A stale worker gets
   zero rows and cannot overwrite the newer attempt. Publication remains
   idempotent and at least once.

The catalog/queue schema must retain a unique source/attachment identity and a
unique attempt token, and must record every attempt, lease, worker, outcome,
and retry state. SQLite WAL is a same-host design; the database and `-wal`/
`-shm` files must stay on local disk rather than a network filesystem. Use a
SQLite-consistent backup operation or coordinated quiescence for backup, never
a live copy of only the main database file.

The measured runtime passes the current WAL-reset safety gate. Before a
production WAL rollout, verify the embedded SQLite version is at least 3.51.3,
or is a patched 3.44.6/3.50.7 build; this task does not install or change
dependencies. The relevant SQLite transaction and WAL constraints are in the
[transaction documentation](https://sqlite.org/lang_transaction.html) and
[WAL documentation](https://sqlite.org/wal.html), including the WAL-reset
compatibility note.

## PostgreSQL comparison and reassessment trigger

PostgreSQL remains the alternative if the workload becomes multi-host, needs
more than the bounded worker pool, or fails a measured lock-wait/throughput
SLO. Its queue claim can use row locks with `FOR UPDATE SKIP LOCKED`, which
PostgreSQL documents as suitable for multiple queue-like consumers
([`SELECT` documentation](https://www.postgresql.org/docs/current/sql-select.html)).

No PostgreSQL benchmark was run because no service was provisioned and this
task is intentionally local and disposable. Its expected operating cost is a
separate service lifecycle: authentication and roles, private networking,
backup/PITR, monitoring, upgrades, migrations, credentials, and deployment
supervision. For the measured 2–4-worker, same-host workload, that recurring
cost is not justified by the observed SQLite contention. Re-run the decision
with a representative pilot before adding a fifth worker, a second host, or a
lock-wait SLO breach.

This choice is for acquisition state only. It does not migrate the review
store or RAG index and does not presume pgvector.

## Review-store migration guard

No review-store move is planned. If a later requirement makes one necessary,
the cut-over must be a separate approved change: make a consistent SQLite
backup and freeze writes; import to a staging backend; verify row counts,
per-key checksums, revisions, tombstones, import-ledger entries, and sampled
round trips; cut over through an explicit backend reference; retain the
verified SQLite copy until acceptance; and roll back by restoring that copy if
any check or post-cut-over invariant fails. Do not introduce an unverified
dual-write path. The existing SQLite review authority and its import/export
contract remain in force.
