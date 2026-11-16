# Require the exact HTTPS origin for a state-changing request.

The reverse proxy supplies the origin policy as deployment
configuration; this helper compares the server-received `Origin` header
byte-for-byte after normalizing the header's URL encoding. Missing or
foreign origins fail closed. Same-origin browser requests must therefore
include the expected header for mutations.

## Usage

``` r
curation_auth_assert_mutation_origin(request, expected_origin)
```

## Arguments

- request:

  Server-side Shiny/httpuv request list.

- expected_origin:

  Exact origin, for example
  `https://biogain.continental-dat.src.surf-hosted.nl`.

## Value

Invisibly, `TRUE`.
