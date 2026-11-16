# Register a portable file claim for a logical attachment.

The path is always relative to the caller-owned `root_id`. Existing
legacy claims should use `verification_status = "unverified"` until a
later check.

## Usage

``` r
acquisition_catalog_register_file(
  catalog,
  document_ref_id,
  root_id,
  relative_path,
  sha256 = NULL,
  size_bytes = NULL,
  verification_status = "unverified",
  provenance = "catalog",
  legacy_path = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- document_ref_id:

  Registered document reference ID.

- root_id:

  Stable storage-root identifier.

- relative_path:

  Portable path below `root_id`.

- sha256:

  Optional content digest.

- size_bytes:

  Optional file size.

- verification_status:

  `unverified` or `verified`.

- provenance:

  Non-empty provenance label.

- legacy_path:

  Optional original path retained for audit.

## Value

A list with `file_ref_id` and `status`.
