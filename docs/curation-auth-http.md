# ACQ-C4 HTTP authentication gate

`curation_auth_http_gate()` is the deployment-owned HTTP boundary around the
generic curation app. It binds to `127.0.0.1` and owns no corpus, review, or
artifact data. nginx calls its internal validity route before forwarding every
HTML, static, download, and WebSocket request to Shiny.

The default constructor is:

```r
curation_auth_http_gate(
  auth,
  host = "127.0.0.1",
  port = 43925L,
  expected_origin = "https://biogain.continental-dat.src.surf-hosted.nl",
  app_path = "/app/",
  cookie_name = "biogain_session"
)
```

Native service runners should leave `blocking = TRUE` (the default). Tests and
supervisors that need a handle can set `blocking = FALSE`; the returned object
has `stop()`, `server`, and `config` members. The function refuses any listener
host other than `127.0.0.1`, validates the HTTPS origin and canonical prefix,
and requires `httpuv` at start-up.

## Routes

With the defaults, the gate exposes these routes. A deployment runner may pass
explicit `login_path`, `check_path`, `logout_path`, and `health_path` values;
the current native profile uses `/auth/login`, `/auth/check`, `/auth/logout`,
and `/healthz` while the protected application prefix remains `/app/`.

| Route | Methods | Behavior |
| --- | --- | --- |
| `/app/login` | `GET`, `HEAD` | Minimal self-contained login form; no account data or redirect target is accepted. |
| `/app/login` | `POST` | Requires the exact configured `Origin`, parses only `application/x-www-form-urlencoded` `username` and `otp`, and returns a generic `401` for every failure. Success returns `303` to `/app/` and a Secure, HttpOnly, SameSite=Strict cookie with `Path=/app`. |
| `/app/logout` | `POST` | Requires the exact configured `Origin`, revokes the opaque session if present, expires the cookie, and redirects to the fixed login path. `GET` is rejected. |
| `/app/_auth/validate` | `GET` | nginx `auth_request` endpoint. It requires forwarded original URI/method metadata, `X-Forwarded-Proto: https`, and (when configured) the exact trusted proxy marker. It returns `200` with server-resolved actor/role headers only after cookie resolution; otherwise it returns `401` or `403`. |
| `/app/health` | `GET`, `HEAD` | Returns only `ok`. It does not read corpus or account rows. |
| other `/app/...` | any protected method | Resolves the cookie server-side. Anonymous requests receive `401`; valid requests receive `200`. WebSocket and state-changing requests additionally require the exact origin. |

The custom route aliases allow the native nginx profile to use `/auth/*` and
`/healthz` without creating an unauthenticated alternate app route. Requests
outside the configured app prefix are `404`; `/app` redirects only to the
canonical `/app/` prefix. No route accepts an arbitrary return URL.

## nginx subrequest envelope

The internal nginx location must overwrite, rather than pass through, these
headers when it calls the check route:

```text
Cookie                $http_cookie
X-Original-URI        $request_uri
X-Original-Method     $request_method
X-Original-Upgrade    $http_upgrade
X-Forwarded-Proto     https
Origin                $http_origin
```

The gate listens only on loopback and requires the forwarded URI, method, and
HTTPS marker before it considers a check request a proxy subrequest. A stricter
deployment may additionally pass `trusted_proxy_header` and
`trusted_proxy_value`; a direct client cannot use that route without the
configured marker. The forwarded URI is reduced to its path, must remain under
the canonical `/app/` prefix, and cannot be an absolute URL.

Validity probes resolve the session with `touch = FALSE`, so repeated nginx
probes cannot keep an idle session alive while the browser is disconnected.
The actual Shiny request resolver performs the touch for an authorized app
request.

For a forwarded WebSocket or state-changing request, the gate compares the
forwarded original `Origin` to the exact configured HTTPS origin. Missing and
foreign origins fail closed. The Shiny app still resolves the opaque cookie on
each request; the actor and role response headers are an auth-subrequest
diagnostic for nginx and are not identity credentials.

## Request and secret handling

The form body is bounded and read from the Rook `rook.input$read()` interface
when running under httpuv. The gate never logs request bodies, OTPs, enrollment
secrets, or session handles. Authentication delegates TOTP verification,
replay prevention, rate limiting, and digest-only session storage to the
SQLite-backed auth store. The gate only receives the successful opaque token to
render the cookie and only receives the cookie handle on subsequent requests.

The service does not enroll users or create a bootstrap account. Account
enrollment remains an explicit operator action against the deployment auth
database.
