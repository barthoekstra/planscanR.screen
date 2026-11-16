# Revoke a server-side authentication session.

Revoke a server-side authentication session.

## Usage

``` r
curation_auth_logout(auth, session_token)
```

## Arguments

- auth:

  A store from
  [`curation_auth_open()`](https://barthoekstra.github.io/planscanR.screen/reference/curation_auth_open.md).

- session_token:

  The opaque handle returned by authentication.

## Value

Invisibly, `TRUE` when the request was processed.
