test_that("TOTP matches the RFC 6238 SHA-1, SHA-256, and SHA-512 vectors", {
  skip_if_not_installed("openssl")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  secret <- function(value) {
    planscanR.screen:::curation_auth_base32_encode(charToRaw(value))
  }
  timestamps <- c(59, 1111111109, 1111111111, 1234567890, 2000000000, 20000000000)
  expect_identical(
    vapply(timestamps, function(at) {
      planscanR.screen:::curation_auth_totp_code(
        secret("12345678901234567890"), at
      )
    }, character(1)),
    c("287082", "081804", "050471", "005924", "279037", "353130")
  )
  expect_identical(
    vapply(timestamps, function(at) {
      planscanR.screen:::curation_auth_totp_code(
        secret("12345678901234567890123456789012"), at,
        algorithm = "sha256"
      )
    }, character(1)),
    c("119246", "084774", "062674", "819424", "698825", "737706")
  )
  expect_identical(
    vapply(timestamps, function(at) {
      planscanR.screen:::curation_auth_totp_code(
        secret("1234567890123456789012345678901234567890123456789012345678901234"),
        at,
        algorithm = "sha512"
      )
    }, character(1)),
    c("693936", "091201", "943326", "441116", "618901", "863826")
  )
})

test_that("authentication consumes a TOTP timestep exactly once and resolves server roles", {
  skip_if_not_installed("openssl")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE), add = TRUE)

  auth <- planscanR.screen:::curation_auth_open(db)
  on.exit(auth$close(), add = TRUE)
  secret <- planscanR.screen:::curation_auth_base32_encode(charToRaw("12345678901234567890"))
  account <- planscanR.screen:::curation_auth_enroll_account(
    auth,
    username = "Alice",
    actor_id = "actor-alice",
    display_name = "Alice Reviewer",
    roles = "reviewer",
    secret = secret
  )
  expect_identical(account$username, "alice")
  now <- floor(as.numeric(Sys.time()))
  otp <- planscanR.screen:::curation_auth_totp_code(secret, now)
  logged_in <- planscanR.screen:::curation_auth_authenticate(auth, "ALICE", otp, timestamp = now)
  expect_true(isTRUE(logged_in$ok))
  expect_s3_class(logged_in$actor, "planscanR_curation_actor")
  expect_identical(logged_in$actor$id, "actor-alice")
  expect_identical(logged_in$actor$roles, "reviewer")
  expect_match(logged_in$session_token, "^[0-9a-f]{64}$")
  expect_error(
    planscanR.screen:::curation_auth_authenticate(auth, "alice", otp, timestamp = now),
    class = "planscanR_error_curation_authentication"
  )

  # A second SQLite connection observes the committed replay row, so a
  # duplicate request cannot create a second session after the first commits.
  auth2 <- planscanR.screen:::curation_auth_open(db)
  on.exit(auth2$close(), add = TRUE)
  next_step <- now + 120L
  next_otp <- planscanR.screen:::curation_auth_totp_code(secret, next_step)
  first <- planscanR.screen:::curation_auth_authenticate(
    auth, "alice", next_otp,
    timestamp = next_step
  )
  expect_true(isTRUE(first$ok))
  expect_error(
    planscanR.screen:::curation_auth_authenticate(
      auth2, "alice", next_otp,
      timestamp = next_step
    ),
    class = "planscanR_error_curation_authentication"
  )

  resolved <- planscanR.screen:::curation_auth_resolve_session(auth, logged_in$session_token)
  expect_identical(resolved$id, "actor-alice")
  DBI::dbExecute(
    auth$conn,
    "UPDATE auth_accounts SET roles = 'admin' WHERE username = 'alice'"
  )
  expect_identical(
    planscanR.screen:::curation_auth_resolve_session(auth, logged_in$session_token)$roles,
    "admin"
  )
  planscanR.screen:::curation_auth_set_account_enabled(auth, "alice", FALSE)
  expect_null(planscanR.screen:::curation_auth_resolve_session(auth, logged_in$session_token))
  expect_null(planscanR.screen:::curation_auth_resolve_session(auth, paste(rep("0", 64), collapse = "")))
})

test_that("simultaneous login attempts can consume a timestep only once", {
  skip_if_not_installed("openssl")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_on_os("windows")
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE), add = TRUE)
  auth <- planscanR.screen:::curation_auth_open(db, busy_timeout = 10000L)
  secret <- planscanR.screen:::curation_auth_base32_encode(charToRaw("12345678901234567890"))
  planscanR.screen:::curation_auth_enroll_account(auth, "parallel", secret = secret)
  auth$close()
  now <- floor(as.numeric(Sys.time()))
  worker <- function() {
    child <- planscanR.screen:::curation_auth_open(db, busy_timeout = 10000L)
    on.exit(child$close(), add = TRUE)
    otp <- planscanR.screen:::curation_auth_totp_code(secret, now)
    result <- tryCatch(
      planscanR.screen:::curation_auth_authenticate(child, "parallel", otp, timestamp = now),
      error = function(error) NULL
    )
    isTRUE(result$ok)
  }
  jobs <- lapply(seq_len(2L), function(index) parallel::mcparallel(worker()))
  results <- parallel::mccollect(jobs)
  expect_equal(sum(vapply(results, isTRUE, logical(1))), 1L)
})

test_that("failed credentials are rate limited persistently without username enumeration", {
  skip_if_not_installed("openssl")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE), add = TRUE)
  auth <- planscanR.screen:::curation_auth_open(
    db,
    max_failures = 2L,
    global_max_failures = 2L,
    failure_window = 300L,
    block_seconds = 300L
  )
  secret <- planscanR.screen:::curation_auth_base32_encode(charToRaw("12345678901234567890"))
  planscanR.screen:::curation_auth_enroll_account(auth, "reviewer", secret = secret)
  on.exit(auth$close(), add = TRUE)
  now <- floor(as.numeric(Sys.time()))
  wrong <- "000000"
  expect_error(
    planscanR.screen:::curation_auth_authenticate(auth, "unknown", wrong, timestamp = now),
    class = "planscanR_error_curation_authentication"
  )
  expect_identical(
    conditionMessage(expect_error(
      planscanR.screen:::curation_auth_authenticate(auth, "bad username", wrong, timestamp = now)
    )),
    "Authentication failed."
  )
  expect_error(
    planscanR.screen:::curation_auth_authenticate(auth, "reviewer", wrong, timestamp = now),
    class = "planscanR_error_curation_authentication"
  )
  auth$close()
  auth <- planscanR.screen:::curation_auth_open(
    db,
    max_failures = 2L,
    global_max_failures = 2L,
    failure_window = 300L,
    block_seconds = 300L
  )
  on.exit(auth$close(), add = TRUE)
  correct <- planscanR.screen:::curation_auth_totp_code(secret, now)
  expect_error(
    planscanR.screen:::curation_auth_authenticate(auth, "reviewer", correct, timestamp = now),
    class = "planscanR_error_curation_authentication"
  )
  expect_match(
    conditionMessage(expect_error(
      planscanR.screen:::curation_auth_authenticate(auth, "reviewer", correct, timestamp = now)
    )),
    "Authentication failed\\."
  )
})

test_that("enrollment is explicit and secret material is stored only in the auth store", {
  skip_if_not_installed("openssl")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  db <- tempfile(fileext = ".sqlite")
  on.exit(unlink(c(db, paste0(db, "-wal"), paste0(db, "-shm")), force = TRUE), add = TRUE)
  auth <- planscanR.screen:::curation_auth_open(db)
  on.exit(auth$close(), add = TRUE)
  expect_equal(DBI::dbGetQuery(auth$conn, "SELECT count(*) AS n FROM auth_accounts")$n, 0)
  enrolled <- planscanR.screen:::curation_auth_enroll_account(
    auth,
    "barthoekstra",
    actor_id = "barthoekstra",
    display_name = "Bootstrap administrator",
    roles = "admin"
  )
  expect_match(enrolled$secret, "^[A-Z2-7]+$")
  expect_equal(DBI::dbGetQuery(auth$conn, "SELECT count(*) AS n FROM auth_accounts")$n, 1)
  skip_on_os("windows")
  store_files <- c(db, paste0(db, "-wal"), paste0(db, "-shm"))
  store_files <- store_files[file.exists(store_files)]
  expect_true(length(store_files) >= 1L)
  expect_true(all(as.character(file.info(store_files)$mode) %in% c("600", "0600")))
  expect_error(
    planscanR.screen:::curation_auth_enroll_account(auth, "barthoekstra", roles = "admin"),
    "already exists"
  )
})

test_that("the Shiny seam accepts only the opaque cookie and exact mutation origin", {
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
    auth, "alice", planscanR.screen:::curation_auth_totp_code(secret, now),
    timestamp = now
  )
  request <- list(
    HEADERS = c(
      HTTP_COOKIE = paste0("biogain_session=", login$session_token),
      HTTP_X_BIOGAIN_ACTOR = "attacker",
      HTTP_X_BIOGAIN_ROLE = "admin"
    )
  )
  resolver <- planscanR.screen:::curation_auth_shiny_actor_resolver(auth)
  actor <- planscanR.screen:::curation_resolve_actor(resolver, request)
  expect_identical(actor$id, "alice")
  expect_identical(actor$roles, "reviewer")
  expect_identical(
    planscanR.screen:::curation_auth_shiny_actor(auth, list(request = request))$id,
    "alice"
  )
  background_resolver <- planscanR.screen:::curation_auth_shiny_actor_resolver(auth, touch = FALSE)
  expect_identical(
    planscanR.screen:::curation_resolve_actor(background_resolver, request)$id,
    "alice"
  )
  generic_context <- list(
    session = list(request = request$HEADERS),
    input = list(reviewer = "attacker"),
    request = request
  )
  expect_identical(
    planscanR.screen:::curation_resolve_actor(resolver, generic_context)$id,
    "alice"
  )
  expect_identical(
    planscanR.screen:::curation_auth_http_actor(auth, generic_context, touch = FALSE)$id,
    "alice"
  )
  expect_identical(
    planscanR.screen:::curation_auth_cookie_header(login$session_token),
    paste0(
      "biogain_session=", login$session_token,
      "; Path=/app; Max-Age=43200; Secure; HttpOnly; SameSite=Strict"
    )
  )
  expect_identical(
    planscanR.screen:::curation_auth_expire_cookie_header(),
    "biogain_session=; Path=/app; Max-Age=0; Secure; HttpOnly; SameSite=Strict"
  )
  request_env <- list2env(as.list(request$HEADERS), parent = emptyenv())
  expect_identical(
    planscanR.screen:::curation_auth_request_session_token(request_env),
    login$session_token
  )
  expect_null(
    planscanR.screen:::curation_auth_request_session_token(
      list(HEADERS = c(HTTP_COOKIE = "biogain_session=bad"))
    )
  )
  expect_invisible(
    planscanR.screen:::curation_auth_assert_mutation_origin(
      list(HEADERS = c(HTTP_ORIGIN = "https://biogain.continental-dat.src.surf-hosted.nl")),
      "https://biogain.continental-dat.src.surf-hosted.nl"
    )
  )
  expect_error(
    planscanR.screen:::curation_auth_assert_mutation_origin(
      list(HEADERS = c(HTTP_ORIGIN = "https://evil.example")),
      "https://biogain.continental-dat.src.surf-hosted.nl"
    ),
    class = "planscanR_error_curation_csrf"
  )
  expect_error(
    planscanR.screen:::curation_auth_assert_mutation_origin(
      list(HEADERS = c(HTTP_ORIGIN = "https://biogain.continental-dat.src.surf-hosted.nl")),
      "https://biogain.continental-dat.src.surf-hosted.nl/path"
    ),
    class = "planscanR_error_curation_auth"
  )
})
