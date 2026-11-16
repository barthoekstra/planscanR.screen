# Upsert one review decision with optimistic concurrency.

Upsert one review decision with optimistic concurrency.

## Usage

``` r
curation_review_upsert(repo, scope, decision, actor, expected_revision = NULL)
```

## Arguments

- repo:

  A planscanR_curation_review_repository.

- scope:

  A planscanR_curation_scope.

- decision:

  One-row decision data frame.

- actor:

  Trusted authenticated actor.

- expected_revision:

  Required current revision, or 0 for insertion.

## Value

A mutation result containing `ok`, the affected `key`, resulting
`revision`, and `deleted`; conflicts use a classed error.
