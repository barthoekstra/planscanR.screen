# Extract an opaque auth cookie from a server request.

Only the cookie value is returned. Identity and role headers are
ignored; the caller must pass this value to
[`curation_auth_resolve_session()`](https://barthoekstra.github.io/planscanR.screen/reference/curation_auth_resolve_session.md).

## Usage

``` r
curation_auth_request_session_token(request, cookie_name = "biogain_session")
```

## Arguments

- request:

  A server-side Shiny/httpuv request list.

- cookie_name:

  Session-cookie name.

## Value

A 64-character session handle, or `NULL` when absent/malformed.
