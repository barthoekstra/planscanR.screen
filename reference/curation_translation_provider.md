# Build an optional translation provider.

The provider is a dependency-injected boundary around a translation
worker. It has no default engine and does not read options or
environment variables. A serialized write gate is required before
translated values are written to a shared cache or sidecar, so a
deployment can coordinate one record at a time.

## Usage

``` r
curation_translation_provider(
  translate_fn,
  service_id,
  source_language_fn = NULL,
  cache_read_fn = NULL,
  cache_write_fn = NULL,
  write_gate_fn = NULL,
  write_policy = c("disabled", "serialized")
)
```

## Arguments

- translate_fn:

  Function accepting `record`, `source_language`, and `target_language`,
  returning a named list with translated fields such as `title_en` and
  `summary_en`.

- service_id:

  Stable backend/version identifier recorded with results.

- source_language_fn:

  Optional function accepting `record` and returning a source-language
  code.

- cache_read_fn:

  Optional function accepting `key` and `service_id` and returning a
  previously translated result, or `NULL` on a miss. The key contains
  `country`, `document_id`, and `target_language`.

- cache_write_fn:

  Optional function accepting the same target-qualified `key` and a
  `value`.

- write_gate_fn:

  Function accepting the record key and `write_fn`; it must invoke
  `write_fn` while holding the deployment's per-record serialized gate.
  The gate key excludes `target_language`, so two target writes for one
  record cannot overlap.

- write_policy:

  Either `"disabled"` or `"serialized"`.

## Value

A `planscanR_curation_translation_provider` object.
