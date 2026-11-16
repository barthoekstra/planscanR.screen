# Open a file-backed SQLite review repository.

The database is initialized on local disk with WAL mode, foreign keys, a
bounded busy timeout, and transactional numbered migrations. `config_id`
is retained as provenance but the review key is
project/corpus/country/document/ reviewer, so a policy revision can
continue to read authored decisions.

## Usage

``` r
curation_review_repository_sqlite(path, busy_timeout = 5000L)

curation_sqlite_review_repository(path, busy_timeout = 5000L)
```

## Arguments

- path:

  Explicit path to a local SQLite database file.

- busy_timeout:

  Milliseconds SQLite may wait for a competing writer.

## Value

A `planscanR_curation_review_repository` with SQLite metadata and a
[`close()`](https://rdrr.io/r/base/connections.html) method.
