# Resolve a server-side HTTP request to its current actor.

This is the small seam used by an internal httpuv auth gate and by the
Shiny application. The request's cookie is only a lookup handle; the
SQLite-backed resolver remains the authority.

## Usage

``` r
curation_auth_http_actor(
  auth,
  request,
  cookie_name = "biogain_session",
  touch = FALSE
)
```

## Arguments

- auth:

  A store from
  [`curation_auth_open()`](https://barthoekstra.github.io/planscanR.screen/reference/curation_auth_open.md).

- request:

  A server-side Shiny/httpuv request list or environment.

- cookie_name:

  Session-cookie name.

- touch:

  Update the session idle timestamp.

## Value

A `planscanR_curation_actor`, or `NULL` for an invalid session.
