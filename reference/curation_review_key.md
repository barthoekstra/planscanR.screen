# Build the composite key for one reviewer's decision on one record.

The native document ID is scoped by country, and a reviewer's decision
is a separate row. The authenticated actor is passed separately to
repository operations; `reviewer_id` here is the stable row identity,
never a display name supplied by a UI.

## Usage

``` r
curation_review_key(country, document_id, reviewer_id)
```

## Arguments

- country:

  Source-country identifier.

- document_id:

  Native document identifier within `country`.

- reviewer_id:

  Stable authenticated reviewer identifier.

## Value

A `planscanR_curation_review_key` object.
