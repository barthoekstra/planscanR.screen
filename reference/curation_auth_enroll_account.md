# Enroll one explicit server account.

This is an operator function, not an app route. If `secret` is omitted,
a random secret is generated and returned only to this caller for
private provisioning. Calling this function is the only operation that
can create an account; opening the store never bootstraps a user.

## Usage

``` r
curation_auth_enroll_account(
  auth,
  username,
  actor_id = username,
  display_name = username,
  roles = "reviewer",
  secret = NULL,
  replace = FALSE
)

curation_auth_enroll(...)
```

## Arguments

- auth:

  A store from
  [`curation_auth_open()`](https://barthoekstra.github.io/planscanR.screen/reference/curation_auth_open.md).

- username:

  Login username.

- actor_id:

  Stable server-side actor ID.

- display_name:

  Informational display name.

- roles:

  `reviewer` and/or `admin`.

- secret:

  Optional existing RFC 4648 base32 secret.

- replace:

  Replace an existing username only when explicitly requested.

- ...:

  Arguments forwarded to `curation_auth_enroll_account()`.

## Value

A list containing account metadata and the enrollment secret.
