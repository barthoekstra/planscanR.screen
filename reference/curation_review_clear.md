# Clear one review decision while preserving its revisioned tombstone.

Clear one review decision while preserving its revisioned tombstone.

## Usage

``` r
curation_review_clear(repo, scope, key, actor, expected_revision = NULL)
```

## Arguments

- repo:

  A planscanR_curation_review_repository.

- scope:

  A planscanR_curation_scope.

- key:

  Record/reviewer key supplied by the caller.

- actor:

  Trusted authenticated actor.

- expected_revision:

  Required current revision.

## Value

An atomic mutation result containing `ok`, affected keys, resulting
revisions, and `deleted` flags; conflicts use a classed error.
