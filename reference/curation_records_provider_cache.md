# Build a planscanR cache-backed record provider.

When the default
[`planscanR::index_cache()`](https://barthoekstra.github.io/planscanR/reference/index_cache.html)
is used, the provider passes the bounded row-transform seam through to
the leaf package. Its read callback also retains a versioned fingerprint
of every successfully decoded complete row for
[`curation_records_snapshot()`](https://barthoekstra.github.io/planscanR.screen/reference/curation_records_snapshot.md)
provenance metadata.

## Usage

``` r
curation_records_provider_cache(
  cache_dir,
  index_fn = planscanR::index_cache,
  batch_size = 512L
)
```

## Arguments

- cache_dir:

  Explicit planscanR cache root.

- index_fn:

  Injectable equivalent of
  [`planscanR::index_cache()`](https://barthoekstra.github.io/planscanR/reference/index_cache.html)
  for offline fixtures. It must accept `cache_dir` and `country`.

- batch_size:

  Number of cache rows combined per leaf index batch. The default is 512
  and is passed to the leaf row-transform seam when the default
  [`planscanR::index_cache()`](https://barthoekstra.github.io/planscanR/reference/index_cache.html)
  is used.

## Value

A `planscanR_curation_records_provider` object.
