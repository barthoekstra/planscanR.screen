# Evict cached zero-shot pipeline(s) to free memory or for tests.

Evict cached zero-shot pipeline(s) to free memory or for tests.

## Usage

``` r
reset_classifier_cache(model_id = NULL)
```

## Arguments

- model_id:

  Optional model ID to evict (all device variants). `NULL` clears the
  whole cache.

## Value

Nothing, invisibly.
