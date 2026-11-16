# Build the authenticated actor resolver used by the Shiny app.

The resolver reads the session cookie from the server-side request and
rechecks the SQLite session and current account roles on every call. It
ignores `X-Forwarded-*`, identity, display-name, and role headers.

## Usage

``` r
curation_auth_shiny_actor_resolver(
  auth,
  cookie_name = "biogain_session",
  touch = TRUE
)
```

## Arguments

- auth:

  A store from
  [`curation_auth_open()`](https://barthoekstra.github.io/planscanR.screen/reference/curation_auth_open.md).

- cookie_name:

  Session-cookie name.

- touch:

  Update the session idle timestamp. Use `FALSE` for background validity
  probes such as a timer; user requests should use `TRUE`.

## Value

A `planscanR_curation_actor_resolver`.
