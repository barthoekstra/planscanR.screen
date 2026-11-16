# Open the separate SQLite acquisition catalog.

The catalog uses local-disk WAL mode and numbered idempotent migrations.
A file containing the existing review tables is rejected so review state
keeps one authoritative SQLite store.

## Usage

``` r
acquisition_catalog_open(path, busy_timeout = 5000L)
```

## Arguments

- path:

  File path for the acquisition database.

- busy_timeout:

  Bounded busy timeout in milliseconds.

## Value

A `planscanR_acquisition_catalog` object with `conn`, `path`,
`schema_version`, `journal_mode`, `foreign_keys`,
[`close()`](https://rdrr.io/r/base/connections.html), and
`transaction()` members.
