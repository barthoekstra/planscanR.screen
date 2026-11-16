# Start the loopback HTTP authentication gate.

The gate exposes only the login/logout form, an nginx `auth_request`
validity endpoint, and a minimal health endpoint. It never proxies the
Shiny application. The default routes are rooted at `/app/`; deployment
runners may pass explicit `/auth/*` and `/healthz` routes while keeping
the protected app prefix and cookie policy unchanged.

## Usage

``` r
curation_auth_http_gate(
  auth,
  host = "127.0.0.1",
  port = 43925L,
  expected_origin = NULL,
  app_path = "/app/",
  cookie_name = "biogain_session",
  blocking = TRUE,
  login_path = NULL,
  public_login_path = NULL,
  check_path = NULL,
  logout_path = NULL,
  public_logout_path = NULL,
  health_path = NULL,
  public_origin = NULL,
  trusted_proxy_header = NULL,
  trusted_proxy_value = "1",
  max_body_bytes = 8192L,
  cookie_max_age = 43200L,
  login_title = "Curation login",
  quiet = FALSE,
  ...
)
```

## Arguments

- auth:

  A store from
  [`curation_auth_open()`](https://barthoekstra.github.io/planscanR.screen/reference/curation_auth_open.md).

- host:

  Listener host. Only `127.0.0.1` is accepted.

- port:

  Unprivileged listener port.

- expected_origin:

  Exact public HTTPS origin.

- app_path:

  Canonical protected application prefix, ending in `/`.

- cookie_name:

  Opaque session-cookie name.

- blocking:

  Keep servicing the server until interrupted. Native service runners
  should use the default; tests can set `FALSE` and call `$stop()`.

- login_path:

  Optional internal login-handler route override. The browser-visible
  form action is `public_login_path`.

- public_login_path:

  Browser-visible login route, normally `/app/login`.

- check_path:

  Optional internal auth-request route override.

- logout_path:

  Optional internal logout-handler route override.

- public_logout_path:

  Browser-visible logout route, normally `/app/logout`.

- health_path:

  Optional health route override.

- public_origin:

  Backward-compatible alias for `expected_origin`.

- trusted_proxy_header:

  Optional exact header nginx sets on internal auth subrequests. The
  loopback deployment also requires forwarded URI, method, HTTPS, and
  (for mutations) original origin metadata.

- trusted_proxy_value:

  Required value for `trusted_proxy_header`.

- max_body_bytes:

  Maximum login form body size.

- cookie_max_age:

  Cookie lifetime in seconds.

- login_title:

  Plain-text heading for the login form. Defaults to the generic
  `Curation login`; project deployment runners may supply branding.

- quiet:

  Suppress httpuv startup diagnostics.

- ...:

  Reserved for deployment-runner compatibility; route and origin options
  should be supplied through their named arguments.

## Value

An object with [`stop()`](https://rdrr.io/r/base/stop.html), `server`,
and `config` when `blocking` is `FALSE`; a stopped object invisibly
after an interrupt otherwise.
