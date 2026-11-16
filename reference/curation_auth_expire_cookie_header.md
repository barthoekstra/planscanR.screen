# Render a deletion header for the auth session cookie.

Render a deletion header for the auth session cookie.

## Usage

``` r
curation_auth_expire_cookie_header(
  cookie_name = "biogain_session",
  path = "/app"
)
```

## Arguments

- cookie_name:

  Session-cookie name.

- path:

  Cookie path used when the session was created.

## Value

A `Set-Cookie` header value.
