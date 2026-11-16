# Resolve a catalog storage location on this machine

Portable catalog locations contain only a stable storage-root ID and a
relative path. This helper supplies the machine-local root paths to the
planscanR containment resolver. Legacy absolute and cache-relative paths
are also accepted while older sidecars and indexes are being adopted.

## Usage

``` r
acquisition_storage_resolve_location(catalog, location, must_exist = FALSE)
```

## Arguments

- catalog:

  Acquisition catalog.

- location:

  Portable location list or legacy character path.

- must_exist:

  Require the target to exist.

## Value

Canonical absolute path below a registered root.
