# Register or update one validated local storage root.

Register or update one validated local storage root.

## Usage

``` r
acquisition_storage_register_root(
  catalog,
  root_id,
  path,
  actor,
  purpose = "downloads",
  reserve_bytes = 0,
  reserve_fraction = 0
)
```

## Arguments

- catalog:

  Acquisition catalog.

- root_id:

  Stable catalog identifier.

- path:

  Absolute writable directory.

- actor:

  Authenticated administrator.

- purpose:

  Root purpose, normally `downloads` or `staging`.

- reserve_bytes:

  Fixed free-space headroom.

- reserve_fraction:

  Additional fraction of total filesystem capacity to keep free.

## Value

A list containing the root identifier and canonical path.
