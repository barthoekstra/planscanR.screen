# Build an injected curation record provider.

Build an injected curation record provider.

## Usage

``` r
curation_records_provider(read_fn, refresh_fn = NULL)
```

## Arguments

- read_fn:

  Function accepting `corpus_id` and optional `countries`, and returning
  a record data frame.

- refresh_fn:

  Optional function with the same signature. It can rebuild a derived
  snapshot; persistence remains owned by a later artifact service.

## Value

A `planscanR_curation_records_provider` object.
