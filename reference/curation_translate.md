# Translate one record through an optional injected provider.

Translate one record through an optional injected provider.

## Usage

``` r
curation_translate(
  provider,
  record,
  key,
  source_language = NULL,
  target_language = "eng_Latn",
  write = FALSE
)
```

## Arguments

- provider:

  A `planscanR_curation_translation_provider`, or `NULL`.

- record:

  One-row record data frame or named record list.

- key:

  Stable composite record key, conventionally country and document_id.

- source_language:

  Source language code. If `NULL`, the provider's resolver is used.

- target_language:

  Target language code.

- write:

  Whether a successful value may be written through the provider cache
  and serialized gate.

## Value

A named translation result with status, originals, translated fields,
and service; worker failures return `status = "error"` and preserve
source text.
