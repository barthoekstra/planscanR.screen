# Build an injected review-repository contract.

C2 supplies the SQLite callbacks. Each write callback receives explicit
actor and revision values. expected_revision = 0 means that the decision
or artifact is expected to be absent. A clear must leave a revisioned
tombstone in the implementation so an old client cannot recreate a
decision after an intervening clear (the ABA case).

## Usage

``` r
curation_review_repository(
  read_fn,
  list_fn,
  upsert_fn,
  clear_fn,
  bulk_upsert_fn,
  import_fn,
  export_fn
)
```

## Arguments

- read_fn:

  Function accepting scope, key, and actor.

- list_fn:

  Function accepting scope, filter, actor, limit, and offset.

- upsert_fn:

  Function accepting scope, decision, actor, and expected_revision.

- clear_fn:

  Function accepting scope, key, actor, and expected_revision.

- bulk_upsert_fn:

  Atomic function accepting scope, decisions, actor, and a per-row
  expected_revision vector.

- import_fn:

  Function accepting scope, rows, actor, and a per-row expected_revision
  vector. Import rows carry explicit `import_source` and `import_row_id`
  values; an optional `import_fingerprint` is preserved.

- export_fn:

  Function accepting scope, actor, and format.

## Value

A planscanR_curation_review_repository object.
