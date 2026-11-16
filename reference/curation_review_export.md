# Export review rows through the repository.

Export review rows through the repository.

## Usage

``` r
curation_review_export(repo, scope, actor, format = "csv")
```

## Arguments

- repo:

  A planscanR_curation_review_repository.

- scope:

  A planscanR_curation_scope.

- actor:

  Trusted authenticated actor.

- format:

  Export format, such as csv or tibble.

## Value

A canonical decision data frame with provenance, revisions, and
tombstones represented by `deleted` rows.
