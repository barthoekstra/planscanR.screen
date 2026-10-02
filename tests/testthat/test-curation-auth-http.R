test_that("the HTTP gate protects the app prefix and validates opaque sessions", {
  skip_if_not_installed("openssl")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("httpuv")

  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE), add = TRUE)
  auth <- planscanR.screen:::curation_auth_open(db)
  on.exit(auth$close(), add = TRUE)
  secret <- planscanR.screen:::curation_auth_base32_encode(charToRaw("12345678901234567890"))
  planscanR.screen:::curation_auth_enroll_account(
    auth, "alice", actor_id = "actor-alice", roles = "reviewer", secret = secret
  )
  secret_bob <- planscanR.screen:::curation_auth_base32_encode(charToRaw("ABCDEFGHIJKLMNOPQRST"))
  planscanR.screen:::curation_auth_enroll_account(
    auth, "bob", actor_id = "actor-bob", roles = "reviewer", secret = secret_bob
  )
  origin <- "https://biogain.continental-dat.src.surf-hosted.nl"
  config <- planscanR.screen:::curation_auth_http_config(
    expected_origin = origin,
    app_path = "/app/",
    cookie_name = "biogain_session"
  )
  request <- function(method = "GET", path = "/app/", headers = character(), body = NULL) {
    req <- list(
      REQUEST_METHOD = method,
      PATH_INFO = path,
      QUERY_STRING = "",
      HEADERS = headers
    )
    if (!is.null(body)) req$body <- body
    req
  }

  anonymous <- planscanR.screen:::curation_auth_http_response(
    auth, request(), config
  )
  expect_identical(anonymous$status, 401L)
  expect_identical(
    planscanR.screen:::curation_auth_http_response(
      auth, request(path = "/app/download/file.pdf"), config
    )$status,
    401L
  )

  now <- floor(as.numeric(Sys.time()))
  otp <- planscanR.screen:::curation_auth_totp_code(secret, now)
  logged_in <- planscanR.screen:::curation_auth_authenticate(
    auth, "alice", otp, timestamp = now
  )
  cookie <- paste0("biogain_session=", logged_in$session_token)
  authorized <- planscanR.screen:::curation_auth_http_response(
    auth, request(headers = c(HTTP_COOKIE = cookie)), config
  )
  expect_identical(authorized$status, 200L)
  expect_identical(authorized$headers[["X-Biogain-Actor"]], "actor-alice")
  expect_identical(
    planscanR.screen:::curation_auth_http_response(
      auth,
      request(path = "/app/../secret", headers = c(HTTP_COOKIE = cookie)),
      config
    )$status,
    400L
  )

  websocket <- planscanR.screen:::curation_auth_http_response(
    auth,
    request(
      path = "/app/websocket",
      headers = c(
        HTTP_COOKIE = cookie,
        HTTP_UPGRADE = "websocket",
        HTTP_ORIGIN = "https://evil.example"
      )
    ),
    config
  )
  expect_identical(websocket$status, 403L)

  login_wrong_origin <- planscanR.screen:::curation_auth_http_response(
    auth,
    request(
      method = "POST",
      path = "/app/login",
      headers = c(
        HTTP_ORIGIN = "https://evil.example",
        CONTENT_TYPE = "application/x-www-form-urlencoded"
      ),
      body = "username=alice&otp=000000"
    ),
    config
  )
  expect_identical(login_wrong_origin$status, 403L)

  # Use a second enrolled account so the form request verifies a code from
  # the current timestep without making the HTTP handler accept a caller
  # supplied timestamp.
  fresh_otp <- planscanR.screen:::curation_auth_totp_code(secret_bob, Sys.time())
  login <- planscanR.screen:::curation_auth_http_response(
    auth,
    request(
      method = "POST",
      path = "/app/login",
      headers = c(
        HTTP_ORIGIN = origin,
        CONTENT_TYPE = "application/x-www-form-urlencoded"
      ),
      body = paste0("username=bob&otp=", fresh_otp)
    ),
    config
  )
  expect_identical(login$status, 303L)
  expect_match(
    login$headers[["Set-Cookie"]],
    "^biogain_session=[0-9a-f]{64}; Path=/app; Max-Age=43200; Secure; HttpOnly; SameSite=Strict$"
  )
  expect_identical(login$headers$Location, "/app/")

  oversized <- planscanR.screen:::curation_auth_http_on_headers(
    request(
      method = "POST",
      path = "/app/login",
      headers = c(CONTENT_LENGTH = "9000")
    ),
    config
  )
  expect_identical(oversized$status, 413L)

  replay <- planscanR.screen:::curation_auth_http_response(
    auth,
    request(
      method = "POST",
      path = "/app/login",
      headers = c(
        HTTP_ORIGIN = origin,
        CONTENT_TYPE = "application/x-www-form-urlencoded"
      ),
      body = paste0("username=bob&otp=", fresh_otp)
    ),
    config
  )
  expect_identical(replay$status, 401L)
  expect_match(replay$body, "Authentication failed", fixed = TRUE)
  expect_false(grepl(fresh_otp, replay$body, fixed = TRUE))

  logout_wrong_origin <- planscanR.screen:::curation_auth_http_response(
    auth,
    request(
      method = "POST",
      path = "/app/logout",
      headers = c(HTTP_COOKIE = cookie, HTTP_ORIGIN = "https://evil.example")
    ),
    config
  )
  expect_identical(logout_wrong_origin$status, 403L)
  logout <- planscanR.screen:::curation_auth_http_response(
    auth,
    request(
      method = "POST",
      path = "/app/logout",
      headers = c(HTTP_COOKIE = cookie, HTTP_ORIGIN = origin)
    ),
    config
  )
  expect_identical(logout$status, 303L)
  expect_match(logout$headers[["Set-Cookie"]], "Max-Age=0")
  expect_identical(
    planscanR.screen:::curation_auth_http_response(
      auth, request(headers = c(HTTP_COOKIE = cookie)), config
    )$status,
    401L
  )
})

test_that("the auth subrequest requires trusted forwarded request metadata", {
  skip_if_not_installed("openssl")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE), add = TRUE)
  auth <- planscanR.screen:::curation_auth_open(db)
  on.exit(auth$close(), add = TRUE)
  secret <- planscanR.screen:::curation_auth_base32_encode(charToRaw("12345678901234567890"))
  planscanR.screen:::curation_auth_enroll_account(auth, "alice", secret = secret)
  now <- floor(as.numeric(Sys.time()))
  login <- planscanR.screen:::curation_auth_authenticate(
    auth, "alice", planscanR.screen:::curation_auth_totp_code(secret, now), timestamp = now
  )
  config <- planscanR.screen:::curation_auth_http_config(
    expected_origin = "https://biogain.continental-dat.src.surf-hosted.nl",
    app_path = "/app/",
    cookie_name = "biogain_session"
  )
  cookie <- paste0("biogain_session=", login$session_token)
  check <- function(headers) planscanR.screen:::curation_auth_http_response(
    auth,
    list(
      REQUEST_METHOD = "GET",
      PATH_INFO = "/app/_auth/validate",
      HEADERS = headers
    ),
    config
  )
  expect_identical(check(c(HTTP_COOKIE = cookie))$status, 403L)
  trusted <- check(c(
    HTTP_COOKIE = cookie,
    HTTP_X_ORIGINAL_URI = "/app/",
    HTTP_X_ORIGINAL_METHOD = "GET",
    HTTP_X_FORWARDED_PROTO = "https"
  ))
  expect_identical(trusted$status, 200L)
  expect_identical(trusted$headers[["X-Biogain-Actor"]], "alice")
  expect_identical(
    check(c(
      HTTP_COOKIE = cookie,
      HTTP_X_ORIGINAL_URI = "/app/",
      HTTP_X_ORIGINAL_METHOD = "GET",
      HTTP_X_ORIGINAL_UPGRADE = "websocket",
      HTTP_X_FORWARDED_PROTO = "https",
      HTTP_ORIGIN = "https://evil.example"
    ))$status,
    403L
  )
  expect_identical(
    check(c(
      HTTP_COOKIE = cookie,
      HTTP_X_ORIGINAL_URI = "https://evil.example/redirect",
      HTTP_X_ORIGINAL_METHOD = "GET",
      HTTP_X_FORWARDED_PROTO = "https"
    ))$status,
    403L
  )
})

test_that("the loopback gate starts and stops without owning app state", {
  skip_if_not_installed("openssl")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("httpuv")

  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE), add = TRUE)
  auth <- planscanR.screen:::curation_auth_open(db)
  on.exit(auth$close(), add = TRUE)
  gate <- planscanR.screen:::curation_auth_http_gate(
    auth,
    port = httpuv::randomPort(min = 20000L, max = 40000L, n = 1L),
    expected_origin = "https://biogain.continental-dat.src.surf-hosted.nl",
    blocking = FALSE,
    quiet = TRUE
  )
  on.exit(gate$stop(), add = TRUE)
  expect_s3_class(gate, "planscanR_curation_http_gate")
  expect_identical(gate$host, "127.0.0.1")
  expect_true(is.function(gate$stop))
  expect_invisible(gate$stop())
  expect_true(isTRUE(gate$closed))
})
