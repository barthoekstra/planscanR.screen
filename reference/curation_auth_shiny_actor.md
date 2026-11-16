# Resolve a Shiny session to its current authenticated actor.

Resolve a Shiny session to its current authenticated actor.

## Usage

``` r
curation_auth_shiny_actor(
  auth,
  session,
  cookie_name = "biogain_session",
  touch = TRUE
)
```

## Arguments

- auth:

  A store from
  [`curation_auth_open()`](https://barthoekstra.github.io/planscanR.screen/reference/curation_auth_open.md).

- session:

  A Shiny session object with a server-side request.

- cookie_name:

  Session-cookie name.

- touch:

  Update the session idle timestamp.

## Value

A `planscanR_curation_actor`.
