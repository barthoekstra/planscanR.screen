# Fingerprint a curation configuration's policy content.

The stable configuration identifier is retained as provenance, but it is
not sufficient for invalidation when a caller changes policy content in
place. This helper hashes the configuration values that affect
enrichment, selection, funneling, branding, and translation. Policy
callbacks include their formals, body, and statically referenced
captured bindings; callers should keep those bindings serializable
policy values rather than opaque external state.

## Usage

``` r
curation_config_fingerprint(config)
```

## Arguments

- config:

  A `planscanR_curation_config`.

## Value

One character configuration fingerprint.
