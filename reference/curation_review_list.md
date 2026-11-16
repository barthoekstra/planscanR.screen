# List review decisions through the injected repository.

List review decisions through the injected repository.

## Usage

``` r
curation_review_list(
  repo,
  scope,
  filter = list(),
  actor,
  limit = NULL,
  offset = 0L
)
```

## Arguments

- repo:

  A planscanR_curation_review_repository.

- scope:

  A planscanR_curation_scope.

- filter:

  Repository-defined filter list.

- actor:

  Trusted authenticated actor.

- limit:

  Optional page size.

- offset:

  Zero-based page offset.

## Value

A canonical decision data frame with country/document/reviewer keys,
decision provenance, timestamp, sidecar path, revision, and deleted
flag.
