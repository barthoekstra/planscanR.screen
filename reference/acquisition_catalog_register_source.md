# Register one source/project reference.

Source identity is the composite `(country, portal, native_project_id)`.
Repeating the same reference is a no-op.

## Usage

``` r
acquisition_catalog_register_source(
  catalog,
  country,
  portal,
  native_project_id,
  source_url = NULL,
  legacy_sidecar_path = NULL,
  metadata = NULL
)
```

## Arguments

- catalog:

  A catalog from
  [`acquisition_catalog_open()`](https://barthoekstra.github.io/planscanR.screen/reference/acquisition_catalog_open.md).

- country:

  Source-country namespace.

- portal:

  Source portal identifier.

- native_project_id:

  Portal-native project ID.

- source_url:

  Optional canonical source URL.

- legacy_sidecar_path:

  Optional legacy sidecar path.

- metadata:

  Optional provenance metadata.

## Value

A list with `source_ref_id` and `status`.
