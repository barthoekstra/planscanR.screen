# Read one review decision through the injected repository.

Read one review decision through the injected repository.

## Usage

``` r
curation_review_read(repo, scope, key, actor)
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

## Value

`NULL` for a never-seen key, or one canonical decision row including
`revision` and a possible `deleted` tombstone.
