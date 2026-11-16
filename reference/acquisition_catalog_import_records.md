# Import offline planscanR records into the acquisition catalog.

With `records = NULL`, the importer calls exported
[`planscanR::index_cache()`](https://barthoekstra.github.io/planscanR/reference/index_cache.html)
using the supplied cache root. No portal or cache writes occur. Legacy
absolute file claims are converted below `file_root` and remain
`unverified`; the catalog itself must live outside that root. Replaying
an unchanged `(import_source, import_row_id)` is a no-op, while a
changed payload is rejected.

## Usage

``` r
acquisition_catalog_import_records(
  catalog,
  records = NULL,
  cache_dir = NULL,
  index_fn = planscanR::index_cache,
  import_source = "planscanR-index-v1",
  file_root = NULL,
  file_root_id = "legacy-root"
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- records:

  A data frame from
  [`planscanR::index_cache()`](https://barthoekstra.github.io/planscanR/reference/index_cache.html),
  or `NULL` to call `index_fn`.

- cache_dir:

  Optional cache root passed to `index_fn`.

- index_fn:

  Exported planscanR-compatible index function.

- import_source:

  Stable source namespace for the import ledger.

- file_root:

  Optional legacy file root used to make paths portable.

- file_root_id:

  Stable identifier for `file_root`.

## Value

A list of counts (`imported`, `replayed`, `sources`, `documents`,
`files`) and imported reference IDs.
