# Calculate an RFC 6238 TOTP code from a base32 account secret.

This helper is useful to test an enrollment or to build an operator-only
provisioning tool. Authentication responses never return a TOTP secret.

## Usage

``` r
curation_auth_totp_code(
  secret,
  timestamp = Sys.time(),
  digits = 6L,
  period = 30L,
  algorithm = "sha1"
)
```

## Arguments

- secret:

  RFC 4648 base32 account secret.

- timestamp:

  POSIXct or epoch seconds; defaults to the current time.

- digits:

  Number of decimal digits, six or eight.

- period:

  Time-step length in seconds.

- algorithm:

  HMAC algorithm: `sha1`, `sha256`, or `sha512`.

## Value

A zero-padded character code.
