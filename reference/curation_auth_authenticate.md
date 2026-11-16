# Authenticate a username with one authenticator-app TOTP code.

A successful result contains a session handle and trusted actor. The
handle is secret and must be kept in an HttpOnly Secure cookie; do not
log or show it. Failure always has the same public message for unknown,
disabled, malformed, rate-limited, replayed, and incorrect credentials.

## Usage

``` r
curation_auth_authenticate(
  auth,
  username,
  otp,
  timestamp = Sys.time(),
  skew = 1L,
  digits = 6L,
  period = 30L,
  algorithm = "sha1"
)

curation_auth_login(...)
```

## Arguments

- auth:

  A store from
  [`curation_auth_open()`](https://barthoekstra.github.io/planscanR.screen/reference/curation_auth_open.md).

- username:

  Account username.

- otp:

  Six or eight digit authenticator-app code.

- timestamp:

  POSIXct or epoch seconds used for TOTP verification.

- skew:

  Number of adjacent 30-second steps accepted.

- digits:

  TOTP digit count.

- period:

  TOTP step length in seconds.

- algorithm:

  HMAC algorithm.

- ...:

  Arguments forwarded to `curation_auth_authenticate()`.

## Value

A list with `ok`, `session_token`, `actor`, and `expires_at`.
