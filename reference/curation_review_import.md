# Import legacy or exported review rows through the repository.

Import legacy or exported review rows through the repository.

## Usage

``` r
curation_review_import(repo, scope, rows, actor, expected_revision = NULL)
```

## Arguments

- repo:

  A planscanR_curation_review_repository.

- scope:

  A planscanR_curation_scope.

- rows:

  Review rows to import.

- actor:

  Trusted authenticated actor performing the import.

- expected_revision:

  Per-row current revisions; 0 means absent.

## Value

An atomic mutation result containing `ok`, affected keys, resulting
revisions, and `deleted` flags; conflicts use a classed error.
