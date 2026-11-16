# Register one logical attachment reference.

Attachments without a native ID default to
`identity_status = "ambiguous"` and are keyed by source and position, so
matching URLs cannot collapse unrelated records.

## Usage

``` r
acquisition_catalog_register_document(
  catalog,
  source_ref_id,
  native_attachment_id = NULL,
  attachment_key = NULL,
  attachment_url = NULL,
  section = NULL,
  ordinal = 1L,
  identity_status = NULL,
  legacy_local_path = NULL,
  metadata = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- source_ref_id:

  Registered source reference ID.

- native_attachment_id:

  Optional trusted native attachment ID.

- attachment_key:

  Stable key when no native ID is available.

- attachment_url:

  Optional source URL.

- section:

  Optional source section.

- ordinal:

  One-based source ordering.

- identity_status:

  `identified` or `ambiguous`.

- legacy_local_path:

  Optional legacy path retained as provenance.

- metadata:

  Optional provenance metadata.

## Value

A list with `document_ref_id` and `status`.
