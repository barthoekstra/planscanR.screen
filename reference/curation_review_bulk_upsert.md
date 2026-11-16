# Atomically upsert multiple review decisions.

Atomically upsert multiple review decisions.

## Usage

``` r
curation_review_bulk_upsert(
  repo,
  scope,
  decisions,
  actor,
  expected_revision = NULL
)
```

## Arguments

- repo:

  A planscanR_curation_review_repository.

- scope:

  A planscanR_curation_scope.

- decisions:

  Data frame of decisions.

- actor:

  Trusted authenticated actor.

- expected_revision:

  Per-row current revisions; 0 means absent.

## Value

An atomic mutation result containing `ok`, affected keys, resulting
revisions, and `deleted` flags; conflicts use a classed error.
