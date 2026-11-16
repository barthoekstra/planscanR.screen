# Wrap an authenticated actor resolver for injection into a curation context.

Wrap an authenticated actor resolver for injection into a curation
context.

## Usage

``` r
curation_actor_resolver(resolve_fn)
```

## Arguments

- resolve_fn:

  Function accepting a deployment request/session context and returning
  a `planscanR_curation_actor`.

## Value

A `planscanR_curation_actor_resolver` object.
