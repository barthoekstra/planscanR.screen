# Resolve an authenticated session to the current server-owned actor.

Resolve an authenticated session to the current server-owned actor.

## Usage

``` r
curation_auth_resolve_session(
  auth,
  session_token,
  timestamp = Sys.time(),
  touch = TRUE
)

curation_auth_resolve_actor(
  auth,
  session_token,
  timestamp = Sys.time(),
  touch = TRUE
)
```

## Arguments

- auth:

  A store from
  [`curation_auth_open()`](https://barthoekstra.github.io/planscanR.screen/reference/curation_auth_open.md).

- session_token:

  The opaque handle returned by authentication.

- timestamp:

  Current POSIXct or epoch seconds.

- touch:

  Update the idle timestamp. Set to `FALSE` for a reverse-proxy validity
  probe; protected app requests should use `TRUE`.

## Value

A `planscanR_curation_actor`, or `NULL` for an invalid/expired session.
