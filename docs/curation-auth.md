# ACQ-C4 authentication contract

This document is the authentication contract for the first BIOGAIN team
curation deployment. It is intentionally separate from the generic curation
contracts: the generic package accepts a trusted actor, while a deployment
chooses how that actor is authenticated.

## Chosen deployment

The initial deployment uses a new nginx reverse proxy with a Let's Encrypt
certificate for `https://biogain.continental-dat.src.surf-hosted.nl/app/`. The
Shiny process listens on private loopback port `43924` and is supervised as a
native service. A separate private auth gate listens on loopback only; nginx
checks its session cookie before forwarding every app, WebSocket, and download
request. The example configuration is inactive until DNS, NAT, certificate,
and operator validation are complete.

The application login is username plus a code from an authenticator app
(RFC 6238 TOTP). There is no password flow and no email or SMS dependency.
Installing the package or preparing a service does not create an account or an
enrollment secret. An operator explicitly enrolls each server account and
delivers the enrollment secret through a private channel or a private QR file.
The initial account mapping is deployment data; `barthoekstra` is the current
bootstrap-admin assumption and is not created automatically.

The first release has one shared BIOGAIN curation project. A reviewer may
create and edit only that reviewer's decisions and samples. An administrator
may manage all review rows and artifacts. These capabilities are enforced by
the repository and artifact services from the authenticated actor's
server-owned roles; UI controls are only presentation.

## Server-owned identity

An account row contains a normalized username, an immutable actor ID, an
informational display name, one or more server-assigned roles, an enabled flag,
and the TOTP secret. The account store is a local SQLite file under the
explicit deployment state root, with restrictive file permissions. It is
never the corpus cache, and it is never written under the protected workspace
`data/` directory. Secrets are not placed in settings, manifests, exports,
logs, or error messages.

The app must pass an authenticated session handle to
`curation_auth_resolve_session()`. That function hashes the handle, looks up
the session, checks expiry and enabled status, then reads the current account
roles from SQLite and returns a `curation_actor`. A display-name input, a
reviewer selector, `X-Forwarded-*` header, query parameter, or Shiny client
value cannot select an actor. Role changes therefore apply to the next
request, including requests made by an existing session.

The HTTP gate uses `curation_auth_http_actor(..., touch = FALSE)` for nginx
validity subrequests so a proxy poll cannot keep an idle session alive. The
Shiny resolver uses `touch = TRUE` for user activity; its background validity
timer must use a resolver configured with `touch = FALSE`.

The outer app gate should keep the session handle in a Secure, HttpOnly,
SameSite cookie supplied by the trusted HTTPS deployment. The gate must apply
to the HTML route, Shiny WebSocket route, static/download route, and any
future file-serving route. The R resolver accepts only a server-side session
value; it does not treat a user-controlled header as an identity assertion.

## TOTP and abuse controls

TOTP uses HMAC-SHA1, 30-second steps, and six decimal digits by default, as
specified by RFC 6238. Verification accepts the current step and one adjacent
step on either side to tolerate small clock drift. The comparison is
constant-time. The implementation uses the maintained `openssl` R package for
HMAC and random bytes; it does not implement a cryptographic primitive.

After a successful check, the matched counter is inserted into a unique
account/counter table in the same SQLite write transaction that creates the
session. A counter already consumed is rejected, including when two requests
race. Failed attempts update a persistent per-account bucket and a persistent
global bucket in the same local database. Unknown-user attempts use only the
global bucket, so an attacker cannot create unbounded rows by inventing
usernames. The default is five account failures and twenty global failures in
five minutes, followed by a five-minute block. A successful login clears both
buckets. The caller may add reverse-proxy/IP throttling, but the application
does not trust an IP header for identity or authorization.

Authentication failures have one generic public outcome. They do not reveal
whether a username exists, whether a code was close, or whether an account is
disabled. OTP values, TOTP secrets, session handles, and their raw database
values must never be logged. Session handles are random 256-bit values and the
database stores only their SHA-256 digest. Sessions expire after an explicit
idle/absolute deployment lifetime and can be revoked by logout or account
disablement.

## Operator enrollment

Enrollment is an offline operator action against the explicit auth database,
for example through the package's `curation_auth_enroll_account()` helper. The
operator supplies username, actor ID, display name, and `reviewer` or `admin`
role. If a secret is omitted, the helper generates one using
`openssl::rand_bytes()` and returns it only to that explicit caller. The
operator must store it privately or render a private QR file, verify the file
permissions, and remove temporary enrollment material after provisioning.
The repository does not ship a real secret and does not run enrollment during
installation or service preparation.

The local enrollment action is the proof that the operator possesses the
explicit auth database and state root; there is no public bootstrap or HTTP
enrollment route. The account holder proves possession of the provisioned
authenticator secret at the first TOTP login. `replace = TRUE` is required for
an account replacement and revokes its existing sessions and replay rows.

## Native service preparation

The BIOGAIN deployment profile is a native systemd template. It requires the
explicit writable state root `/data/biogain-docs/.curation-service`, the
explicit read-only corpus cache `/data/biogain-docs/data/biogain/cache`, and a
separate installed R library `/data/biogain-docs/.curation-Rlib` (never the
workspace `.Rlib`). Preparation creates only state subdirectories and a
non-secret environment file; it does not enable a unit, start a listener,
configure nginx, enroll users, or touch the corpus. The service binds to
loopback port `43924` and restarts under systemd. No CPU or memory cap is
applied because the operator did not supply one; a later measured pilot can
add limits.

The example nginx configuration is inert documentation. It includes the
Let's Encrypt certificate paths, the WebSocket upgrade, long-lived download
settings, and an internal auth subrequest for the `/app/` prefix. Activation
waits for certificate issuance, proxy validation, and the explicit account
enrollment command.
