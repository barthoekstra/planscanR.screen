# Write a portable manifest for one catalog source/project

The current catalog models a project as its stable `source_ref_id`; it
does not create a second project UUID. Each logical attachment keeps its
`document_ref_id`, and each registered file claim is emitted as a
version claim identified by its actual `file_ref_id` and content
SHA-256. The manifest contains no machine-specific absolute storage
paths.

## Usage

``` r
acquisition_catalog_write_project_manifest(catalog, source_ref_id, path = NULL)
```

## Arguments

- catalog:

  Acquisition catalog.

- source_ref_id:

  Stable source/project ID.

- path:

  Optional output path. Defaults to
  `projects/<source_ref_id>/manifest.json` beside the catalog database.

## Value

The manifest list, invisibly carrying its written path as an attribute.
