# Audit cache files for verified, unverified, and interrupted publications.

Audit cache files for verified, unverified, and interrupted
publications.

## Usage

``` r
acquisition_queue_audit_cache(cache_dir = NULL, country = NULL)
```

## Arguments

- cache_dir:

  Cache root. Defaults to planscanR's cache root.

- country:

  Optional country filter.

## Value

The tibble returned by
[`planscanR::audit_cache_downloads()`](https://barthoekstra.github.io/planscanR/reference/audit_cache_downloads.html).
