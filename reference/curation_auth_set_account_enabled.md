# Enable or disable an enrolled account and revoke its sessions.

Enable or disable an enrolled account and revoke its sessions.

## Usage

``` r
curation_auth_set_account_enabled(auth, username, enabled = TRUE)
```

## Arguments

- auth:

  A store from
  [`curation_auth_open()`](https://barthoekstra.github.io/planscanR.screen/reference/curation_auth_open.md).

- username:

  Account username.

- enabled:

  Whether the account may authenticate.

## Value

Invisibly, `TRUE`.
