# Render the Secure, HttpOnly session cookie used by the auth gate.

Render the Secure, HttpOnly session cookie used by the auth gate.

## Usage

``` r
curation_auth_cookie_header(
  session_token,
  cookie_name = "biogain_session",
  path = "/app",
  max_age = 43200L
)
```

## Arguments

- session_token:

  Opaque token returned by authentication.

- cookie_name:

  Session-cookie name.

- path:

  Cookie path, normally `/app`.

- max_age:

  Session lifetime in seconds.

## Value

A `Set-Cookie` header value.
