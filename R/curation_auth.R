# ACQ-C4 deployment-owned username + TOTP authentication.
#
# The generic curation package receives a trusted actor from this seam.  The
# account, replay, rate-limit, and session records live in a separate local
# SQLite database.  This file deliberately has no project or account defaults:
# an operator must explicitly enroll accounts before a deployment can log in.

curation_auth_schema_version <- 1L
curation_auth_base32_alphabet <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZ234567", "", fixed = TRUE)[[1L]]

curation_auth_abort <- function(
  message,
  class = "planscanR_error_curation_auth",
  ...
) {
  rlang::abort(message, class = unique(c(class, "planscanR_error_curation_auth")), ...)
}

curation_auth_require_packages <- function() {
  needed <- c("DBI", "RSQLite", "openssl")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "The curation authenticator needs {cli::qty(missing)} package{?s}: {.pkg {missing}}.",
        i = "Install DBI, RSQLite, and openssl in the deployment/test library and retry."
      ),
      class = "planscanR_error_missing_dep"
    )
  }
  invisible(TRUE)
}

curation_auth_scalar <- function(value, arg, class = "planscanR_error_curation_auth") {
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(trimws(value))) {
    curation_auth_abort(
      paste0("{.arg ", arg, "} must be one non-empty character string."),
      class = class
    )
  }
  trimws(value)
}

curation_auth_integer <- function(value, arg, lower = 0L, upper = 2147483647L) {
  if (
    !is.numeric(value) || length(value) != 1L || !is.finite(value) ||
      value != trunc(value) || value < lower || value > upper
  ) {
    curation_auth_abort(
      paste0("{.arg ", arg, "} must be an integer between ", lower, " and ", upper, ".")
    )
  }
  as.integer(value)
}

curation_auth_normalize_username <- function(username) {
  username <- curation_auth_scalar(username, "username")
  username <- tolower(enc2utf8(username))
  if (!grepl("^[^[:space:]]+$", username, perl = TRUE)) {
    curation_auth_abort("Username must not contain whitespace.")
  }
  if (identical(username, "__global__")) {
    curation_auth_abort("That username is reserved by the authentication store.")
  }
  username
}

curation_auth_roles <- function(roles) {
  if (!is.character(roles) || length(roles) == 0L || anyNA(roles)) {
    curation_auth_abort("Account roles must be a non-empty character vector.")
  }
  roles <- unique(tolower(trimws(roles)))
  if (any(!nzchar(roles)) || any(!roles %in% c("reviewer", "admin"))) {
    curation_auth_abort("Account roles must contain only reviewer and admin.")
  }
  sort(roles)
}

curation_auth_timestamp <- function(value = Sys.time()) {
  if (inherits(value, "POSIXt")) {
    value <- as.numeric(value[[1L]])
  }
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0) {
    curation_auth_abort("Authentication timestamps must be finite POSIXct or epoch seconds.")
  }
  as.numeric(value)
}

curation_auth_now_text <- function(value = Sys.time()) {
  format(as.POSIXct(curation_auth_timestamp(value), origin = "1970-01-01", tz = "UTC"),
    "%Y-%m-%dT%H:%M:%OS6Z",
    tz = "UTC"
  )
}

curation_auth_db <- function(auth) {
  if (!inherits(auth, "planscanR_curation_auth") || isTRUE(auth$closed) ||
    !DBI::dbIsValid(auth$conn)) {
    curation_auth_abort("The curation authenticator is closed.")
  }
  auth$conn
}

curation_auth_db_params <- function(values) unname(as.list(values))

curation_auth_raw_hex <- function(value) {
  value <- as.raw(value)
  paste(sprintf("%02x", as.integer(value)), collapse = "")
}

curation_auth_hex_raw <- function(value) {
  value <- tolower(curation_auth_scalar(value, "hex value"))
  if (nchar(value) %% 2L != 0L || !grepl("^[0-9a-f]+$", value, perl = TRUE)) {
    curation_auth_abort("The cryptographic value is not valid hexadecimal.")
  }
  as.raw(strtoi(substring(
    value, seq(1L, nchar(value), by = 2L),
    seq(2L, nchar(value), by = 2L)
  ), base = 16L))
}

curation_auth_sha256 <- function(value) {
  raw <- if (is.raw(value)) value else charToRaw(as.character(value))
  digest <- openssl::sha256(raw)
  if (is.character(digest)) {
    if (length(digest) != 1L || nchar(digest) != 64L) {
      curation_auth_abort("The openssl SHA-256 result had an unexpected format.")
    }
    digest <- curation_auth_hex_raw(digest)
  }
  as.raw(digest)
}

curation_auth_hmac <- function(key, message, algorithm) {
  key <- if (is.raw(key)) key else charToRaw(as.character(key))
  message <- if (is.raw(message)) message else charToRaw(as.character(message))
  # The maintained openssl package exposes keyed SHA helpers rather than a
  # separately exported hmac() function.  Passing key= delegates HMAC to
  # OpenSSL's implementation; no cryptographic primitive is reimplemented in
  # this package.
  digest <- switch(algorithm,
    sha1 = openssl::sha1(message, key = key),
    sha256 = openssl::sha256(message, key = key),
    sha512 = openssl::sha512(message, key = key),
    curation_auth_abort("TOTP algorithm must be sha1, sha256, or sha512.")
  )
  as.raw(digest)
}

curation_auth_base32_decode <- function(secret) {
  secret <- toupper(gsub("[ =-]", "", curation_auth_scalar(secret, "secret")))
  if (!nzchar(secret) || !grepl("^[A-Z2-7]+$", secret, perl = TRUE)) {
    curation_auth_abort("The TOTP secret must be an RFC 4648 base32 value.")
  }
  values <- match(strsplit(secret, "", fixed = TRUE)[[1L]], curation_auth_base32_alphabet) - 1L
  buffer <- 0L
  bits <- 0L
  bytes <- integer(0L)
  for (value in values) {
    buffer <- bitwOr(bitwShiftL(buffer, 5L), value)
    bits <- bits + 5L
    while (bits >= 8L) {
      bits <- bits - 8L
      bytes <- c(bytes, bitwAnd(bitwShiftR(buffer, bits), 255L))
      if (bits == 0L) {
        buffer <- 0L
      } else {
        buffer <- bitwAnd(buffer, bitwShiftL(1L, bits) - 1L)
      }
    }
  }
  if (bits > 0L && bitwAnd(buffer, bitwShiftL(1L, bits) - 1L) != 0L) {
    curation_auth_abort("The TOTP secret has non-zero padding bits.")
  }
  if (length(bytes) < 10L) {
    curation_auth_abort("The TOTP secret must contain at least 80 bits.")
  }
  as.raw(bytes)
}

curation_auth_base32_encode <- function(value) {
  if (!is.raw(value) || length(value) == 0L) {
    curation_auth_abort("The value to encode must be non-empty raw bytes.")
  }
  buffer <- 0L
  bits <- 0L
  output <- character(0L)
  for (byte in as.integer(value)) {
    buffer <- bitwOr(bitwShiftL(buffer, 8L), byte)
    bits <- bits + 8L
    while (bits >= 5L) {
      bits <- bits - 5L
      index <- bitwAnd(bitwShiftR(buffer, bits), 31L) + 1L
      output <- c(output, curation_auth_base32_alphabet[[index]])
      if (bits == 0L) {
        buffer <- 0L
      } else {
        buffer <- bitwAnd(buffer, bitwShiftL(1L, bits) - 1L)
      }
    }
  }
  if (bits > 0L) {
    index <- bitwAnd(bitwShiftL(buffer, 5L - bits), 31L) + 1L
    output <- c(output, curation_auth_base32_alphabet[[index]])
  }
  paste0(output, collapse = "")
}

curation_auth_random_secret <- function() {
  curation_auth_base32_encode(openssl::rand_bytes(20L))
}

curation_auth_counter_bytes <- function(counter) {
  counter <- curation_auth_timestamp(counter)
  if (counter != floor(counter) || counter > 9007199254740991) {
    curation_auth_abort("The TOTP counter must be a non-negative integer.")
  }
  bytes <- integer(8L)
  for (i in 8:1) {
    bytes[[i]] <- counter %% 256
    counter <- floor(counter / 256)
  }
  as.raw(bytes)
}

curation_auth_totp_code_raw <- function(
  secret_raw,
  timestamp = Sys.time(),
  digits = 6L,
  period = 30L,
  algorithm = "sha1"
) {
  if (!is.raw(secret_raw) || length(secret_raw) < 10L) {
    curation_auth_abort("The decoded TOTP secret must contain at least 80 bits.")
  }
  digits <- curation_auth_integer(digits, "digits", lower = 6L, upper = 8L)
  period <- curation_auth_integer(period, "period", lower = 1L, upper = 3600L)
  algorithm <- tolower(curation_auth_scalar(algorithm, "algorithm"))
  if (!algorithm %in% c("sha1", "sha256", "sha512")) {
    curation_auth_abort("TOTP algorithm must be sha1, sha256, or sha512.")
  }
  seconds <- curation_auth_timestamp(timestamp)
  counter <- floor(seconds / period)
  digest <- curation_auth_hmac(secret_raw, curation_auth_counter_bytes(counter), algorithm)
  offset <- bitwAnd(as.integer(digest[[length(digest)]]), 15L)
  selected <- as.integer(digest[(offset + 1L):(offset + 4L)])
  binary <- sum(selected * c(256^3, 256^2, 256, 1))
  # RFC 4226/6238 uses a signed 31-bit dynamic-truncation value.
  # Keep this as a double: R's bitwAnd coerces values above INT_MAX to NA on
  # some builds, while all four-byte values are exactly representable here.
  binary <- binary %% 2147483648
  modulus <- 10^digits
  sprintf(paste0("%0", digits, "d"), binary %% modulus)
}

#' Calculate an RFC 6238 TOTP code from a base32 account secret.
#'
#' This helper is useful to test an enrollment or to build an operator-only
#' provisioning tool. Authentication responses never return a TOTP secret.
#'
#' @param secret RFC 4648 base32 account secret.
#' @param timestamp POSIXct or epoch seconds; defaults to the current time.
#' @param digits Number of decimal digits, six or eight.
#' @param period Time-step length in seconds.
#' @param algorithm HMAC algorithm: `sha1`, `sha256`, or `sha512`.
#' @return A zero-padded character code.
#' @export
curation_auth_totp_code <- function(
  secret,
  timestamp = Sys.time(),
  digits = 6L,
  period = 30L,
  algorithm = "sha1"
) {
  curation_auth_require_packages()
  curation_auth_totp_code_raw(
    curation_auth_base32_decode(secret), timestamp, digits, period, algorithm
  )
}

curation_auth_constant_time_equal <- function(left, right) {
  if (!is.character(left) || !is.character(right) || length(left) != 1L || length(right) != 1L) {
    return(FALSE)
  }
  left <- charToRaw(left)
  right <- charToRaw(right)
  if (length(left) != length(right)) {
    return(FALSE)
  }
  if (length(left) == 0L) {
    return(TRUE)
  }
  value <- 0L
  for (i in seq_along(left)) {
    value <- bitwOr(value, bitwXor(as.integer(left[[i]]), as.integer(right[[i]])))
  }
  identical(value, 0L)
}

curation_auth_find_totp_counter <- function(
  secret,
  otp,
  timestamp,
  skew,
  digits,
  period,
  algorithm
) {
  otp <- as.character(otp)
  current <- floor(curation_auth_timestamp(timestamp) / period)
  for (offset in c(0L, seq_len(skew), -seq_len(skew))) {
    counter <- current + offset
    if (counter < 0) next
    expected <- curation_auth_totp_code_raw(
      curation_auth_base32_decode(secret), counter * period, digits, period, algorithm
    )
    if (curation_auth_constant_time_equal(expected, otp)) {
      return(as.numeric(counter))
    }
  }
  NULL
}

curation_auth_with_transaction <- function(auth, fn) {
  conn <- curation_auth_db(auth)
  started <- FALSE
  committed <- FALSE
  on.exit(
    if (isTRUE(started) && !isTRUE(committed) && DBI::dbIsValid(conn)) {
      try(DBI::dbRollback(conn), silent = TRUE)
    },
    add = TRUE
  )
  DBI::dbExecute(conn, "BEGIN IMMEDIATE")
  started <- TRUE
  value <- fn(conn)
  DBI::dbCommit(conn)
  committed <- TRUE
  curation_auth_secure_files(auth$path)
  value
}

curation_auth_create_schema <- function(conn) {
  statements <- c(
    paste(
      "CREATE TABLE IF NOT EXISTS auth_accounts (",
      "username TEXT PRIMARY KEY,",
      "actor_id TEXT NOT NULL UNIQUE,",
      "display_name TEXT NOT NULL,",
      "roles TEXT NOT NULL,",
      "secret_base32 TEXT NOT NULL,",
      "enabled INTEGER NOT NULL DEFAULT 1 CHECK (enabled IN (0, 1)),",
      "created_at TEXT NOT NULL,",
      "updated_at TEXT NOT NULL",
      ")"
    ),
    paste(
      "CREATE TABLE IF NOT EXISTS auth_sessions (",
      "token_hash TEXT PRIMARY KEY,",
      "username TEXT NOT NULL,",
      "created_at INTEGER NOT NULL,",
      "last_seen INTEGER NOT NULL,",
      "expires_at INTEGER NOT NULL,",
      "FOREIGN KEY (username) REFERENCES auth_accounts(username) ON DELETE CASCADE",
      ")"
    ),
    "CREATE INDEX IF NOT EXISTS auth_sessions_username_idx ON auth_sessions(username)",
    paste(
      "CREATE TABLE IF NOT EXISTS auth_totp_replay (",
      "username TEXT NOT NULL,",
      "timestep INTEGER NOT NULL,",
      "consumed_at INTEGER NOT NULL,",
      "PRIMARY KEY (username, timestep),",
      "FOREIGN KEY (username) REFERENCES auth_accounts(username) ON DELETE CASCADE",
      ")"
    ),
    paste(
      "CREATE TABLE IF NOT EXISTS auth_rate_limits (",
      "username TEXT PRIMARY KEY,",
      "window_start INTEGER NOT NULL,",
      "failures INTEGER NOT NULL,",
      "blocked_until INTEGER NOT NULL",
      ")"
    ),
    "CREATE TABLE IF NOT EXISTS auth_schema (version INTEGER PRIMARY KEY, applied_at TEXT NOT NULL)"
  )
  for (statement in statements) DBI::dbExecute(conn, statement)
  DBI::dbExecute(
    conn,
    "INSERT OR IGNORE INTO auth_schema(version, applied_at) VALUES (?, ?)",
    params = list(curation_auth_schema_version, curation_auth_now_text())
  )
  invisible(TRUE)
}

curation_auth_secure_files <- function(path) {
  candidates <- c(path, paste0(path, "-wal"), paste0(path, "-shm"))
  candidates <- candidates[file.exists(candidates)]
  if (length(candidates) > 0L) {
    result <- tryCatch(Sys.chmod(candidates, mode = "0600"), error = function(error) NULL)
    if (is.null(result) || !all(result)) {
      curation_auth_abort("The authentication store permissions could not be restricted.")
    }
  }
  invisible(TRUE)
}

#' Open the local SQLite account/session store.
#'
#' The path must be a file outside the corpus cache and protected workspace
#' data root. No account or secret is created by this operation.
#'
#' @param path Explicit local SQLite path for accounts and sessions.
#' @param busy_timeout Bounded SQLite busy timeout in milliseconds.
#' @param max_failures Failed attempts allowed in one rate-limit window.
#' @param global_max_failures Failed attempts allowed in the persistent global
#'   rate-limit bucket. Unknown usernames use this bucket without creating
#'   unbounded per-name rows.
#' @param failure_window Rate-limit window in seconds.
#' @param block_seconds Block duration after the failure threshold.
#' @param session_ttl Absolute session lifetime in seconds.
#' @param idle_timeout Maximum idle session lifetime in seconds.
#' @return A `planscanR_curation_auth` store with a `close()` method.
#' @export
curation_auth_open <- function(
  path,
  busy_timeout = 5000L,
  max_failures = 5L,
  global_max_failures = 20L,
  failure_window = 300L,
  block_seconds = 300L,
  session_ttl = 43200L,
  idle_timeout = 3600L
) {
  curation_auth_require_packages()
  path <- curation_auth_scalar(path, "path")
  if (path %in% c(":memory:", "file::memory:")) {
    curation_auth_abort("The authentication store requires an explicit local SQLite path.")
  }
  busy_timeout <- curation_auth_integer(busy_timeout, "busy_timeout", upper = 60000L)
  max_failures <- curation_auth_integer(max_failures, "max_failures", lower = 1L, upper = 1000L)
  global_max_failures <- curation_auth_integer(
    global_max_failures, "global_max_failures",
    lower = 1L, upper = 100000L
  )
  failure_window <- curation_auth_integer(failure_window, "failure_window", lower = 1L, upper = 86400L)
  block_seconds <- curation_auth_integer(block_seconds, "block_seconds", lower = 1L, upper = 86400L)
  session_ttl <- curation_auth_integer(session_ttl, "session_ttl", lower = 60L, upper = 604800L)
  idle_timeout <- curation_auth_integer(idle_timeout, "idle_timeout", lower = 60L, upper = session_ttl)
  parent <- dirname(path)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    curation_auth_abort("The authentication store parent directory could not be created.")
  }
  if (dir.exists(path)) {
    curation_auth_abort("The authentication store path must be a file, not a directory.")
  }
  path <- normalizePath(path, mustWork = FALSE)
  conn <- tryCatch(
    DBI::dbConnect(RSQLite::SQLite(), dbname = path),
    error = function(error) {
      curation_auth_abort(
        "The authentication store could not be opened.",
        parent = error
      )
    }
  )
  keep_open <- FALSE
  on.exit(
    if (!isTRUE(keep_open) && DBI::dbIsValid(conn)) try(DBI::dbDisconnect(conn), silent = TRUE),
    add = TRUE
  )
  DBI::dbExecute(conn, paste0("PRAGMA busy_timeout = ", busy_timeout))
  journal <- tolower(as.character(DBI::dbGetQuery(conn, "PRAGMA journal_mode = WAL")[[1L]]))
  if (!identical(journal, "wal")) {
    curation_auth_abort("The authentication store requires SQLite WAL mode.")
  }
  DBI::dbExecute(conn, "PRAGMA foreign_keys = ON")
  if (!identical(as.integer(DBI::dbGetQuery(conn, "PRAGMA foreign_keys")[[1L]]), 1L)) {
    curation_auth_abort("The authentication store could not enable foreign keys.")
  }
  curation_auth_create_schema(conn)
  curation_auth_secure_files(path)
  state <- new.env(parent = emptyenv())
  state$conn <- conn
  state$path <- normalizePath(path, mustWork = TRUE)
  state$closed <- FALSE
  state$busy_timeout <- busy_timeout
  state$max_failures <- max_failures
  state$global_max_failures <- global_max_failures
  state$failure_window <- failure_window
  state$block_seconds <- block_seconds
  state$session_ttl <- session_ttl
  state$idle_timeout <- idle_timeout
  class(state) <- c("planscanR_curation_auth", "environment")
  state$close <- function() {
    if (!isTRUE(state$closed) && DBI::dbIsValid(state$conn)) DBI::dbDisconnect(state$conn)
    state$closed <- TRUE
    invisible(TRUE)
  }
  keep_open <- TRUE
  state
}

curation_auth_account <- function(conn, username) {
  rows <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT username, actor_id, display_name, roles, secret_base32, enabled",
      "FROM auth_accounts WHERE username = ?"
    ),
    params = list(username)
  )
  if (nrow(rows) == 0L) {
    return(NULL)
  }
  rows[1L, , drop = FALSE]
}

curation_auth_new_actor <- function(account) {
  curation_actor(
    id = as.character(account$actor_id[[1L]]),
    roles = strsplit(as.character(account$roles[[1L]]), ",", fixed = TRUE)[[1L]],
    display_name = as.character(account$display_name[[1L]])
  )
}

curation_auth_failure <- function() {
  curation_auth_abort(
    "Authentication failed.",
    class = "planscanR_error_curation_authentication"
  )
}

curation_auth_rate_state <- function(conn, username, now) {
  rows <- DBI::dbGetQuery(
    conn,
    "SELECT window_start, failures, blocked_until FROM auth_rate_limits WHERE username = ?",
    params = list(username)
  )
  if (nrow(rows) == 0L) {
    return(NULL)
  }
  rows[1L, , drop = FALSE]
}

curation_auth_record_failure <- function(auth, conn, username, now, account_exists) {
  # The global bucket prevents an attacker from bypassing the throttle by
  # trying an unbounded stream of made-up usernames. Unknown names are never
  # materialized as individual rows; an enrolled name also gets a per-account
  # bucket for a tighter brute-force limit.
  targets <- if (isTRUE(account_exists)) c("__global__", username) else "__global__"
  limits <- if (isTRUE(account_exists)) c(auth$global_max_failures, auth$max_failures) else auth$global_max_failures
  for (i in seq_along(targets)) {
    target <- targets[[i]]
    limit <- limits[[i]]
    row <- curation_auth_rate_state(conn, target, now)
    if (is.null(row) || now >= as.numeric(row$window_start[[1L]]) + auth$failure_window) {
      DBI::dbExecute(
        conn,
        paste(
          "INSERT INTO auth_rate_limits(username, window_start, failures, blocked_until)",
          "VALUES (?, ?, ?, ?) ON CONFLICT(username) DO UPDATE SET",
          "window_start = excluded.window_start, failures = excluded.failures,",
          "blocked_until = excluded.blocked_until"
        ),
        params = list(target, as.numeric(now), 1L, if (limit <= 1L) now + auth$block_seconds else 0)
      )
    } else {
      failures <- as.integer(row$failures[[1L]]) + 1L
      blocked <- as.numeric(row$blocked_until[[1L]])
      if (failures >= limit) blocked <- max(blocked, now + auth$block_seconds)
      DBI::dbExecute(
        conn,
        "UPDATE auth_rate_limits SET failures = ?, blocked_until = ? WHERE username = ?",
        params = list(failures, blocked, target)
      )
    }
  }
  invisible(TRUE)
}

curation_auth_rate_blocked <- function(auth, conn, username, now, account_exists) {
  global <- curation_auth_rate_state(conn, "__global__", now)
  if (!is.null(global) && now < as.numeric(global$blocked_until[[1L]])) {
    return(TRUE)
  }
  if (!isTRUE(account_exists)) {
    return(FALSE)
  }
  row <- curation_auth_rate_state(conn, username, now)
  !is.null(row) && now < as.numeric(row$blocked_until[[1L]])
}

curation_auth_otp <- function(otp) {
  if (!is.character(otp) || length(otp) != 1L || is.na(otp)) {
    return(NULL)
  }
  otp <- trimws(otp)
  if (!grepl("^[0-9]{6,8}$", otp, perl = TRUE)) {
    return(NULL)
  }
  otp
}

#' Authenticate a username with one authenticator-app TOTP code.
#'
#' A successful result contains a session handle and trusted actor. The handle
#' is secret and must be kept in an HttpOnly Secure cookie; do not log or show
#' it. Failure always has the same public message for unknown, disabled,
#' malformed, rate-limited, replayed, and incorrect credentials.
#'
#' @param auth A store from [curation_auth_open()].
#' @param username Account username.
#' @param otp Six or eight digit authenticator-app code.
#' @param timestamp POSIXct or epoch seconds used for TOTP verification.
#' @param skew Number of adjacent 30-second steps accepted.
#' @param digits TOTP digit count.
#' @param period TOTP step length in seconds.
#' @param algorithm HMAC algorithm.
#' @return A list with `ok`, `session_token`, `actor`, and `expires_at`.
#' @export
curation_auth_authenticate <- function(
  auth,
  username,
  otp,
  timestamp = Sys.time(),
  skew = 1L,
  digits = 6L,
  period = 30L,
  algorithm = "sha1"
) {
  curation_auth_require_packages()
  conn <- curation_auth_db(auth)
  username <- tryCatch(
    curation_auth_normalize_username(username),
    error = function(error) NULL
  )
  if (is.null(username)) curation_auth_failure()
  otp <- curation_auth_otp(otp)
  skew <- curation_auth_integer(skew, "skew", upper = 5L)
  digits <- curation_auth_integer(digits, "digits", lower = 6L, upper = 8L)
  period <- curation_auth_integer(period, "period", lower = 1L, upper = 3600L)
  algorithm <- tolower(curation_auth_scalar(algorithm, "algorithm"))
  now <- floor(curation_auth_timestamp())
  verified_at <- curation_auth_timestamp(timestamp)
  result <- curation_auth_with_transaction(auth, function(conn) {
    account <- curation_auth_account(conn, username)
    if (curation_auth_rate_blocked(auth, conn, username, now, !is.null(account))) {
      return(list(ok = FALSE, blocked = TRUE))
    }
    counter <- if (!is.null(account) && as.integer(account$enabled[[1L]]) == 1L && !is.null(otp)) {
      curation_auth_find_totp_counter(
        account$secret_base32[[1L]], otp, verified_at, skew, digits, period, algorithm
      )
    } else {
      NULL
    }
    replayed <- !is.null(counter) && nrow(DBI::dbGetQuery(
      conn,
      "SELECT 1 FROM auth_totp_replay WHERE username = ? AND timestep = ?",
      params = list(username, counter)
    )) > 0L
    if (is.null(account) || as.integer(account$enabled[[1L]]) != 1L || is.null(counter) || replayed) {
      curation_auth_record_failure(auth, conn, username, now, !is.null(account))
      return(list(ok = FALSE, blocked = FALSE))
    }
    DBI::dbExecute(
      conn,
      "INSERT INTO auth_totp_replay(username, timestep, consumed_at) VALUES (?, ?, ?)",
      params = list(username, counter, now)
    )
    token <- curation_auth_raw_hex(openssl::rand_bytes(32L))
    token_hash <- curation_auth_raw_hex(curation_auth_sha256(token))
    DBI::dbExecute(
      conn,
      paste(
        "INSERT INTO auth_sessions(token_hash, username, created_at, last_seen, expires_at)",
        "VALUES (?, ?, ?, ?, ?)"
      ),
      params = list(token_hash, username, now, now, now + auth$session_ttl)
    )
    DBI::dbExecute(
      conn,
      "DELETE FROM auth_rate_limits WHERE username IN (?, '__global__')",
      params = list(username)
    )
    list(
      ok = TRUE,
      session_token = token,
      actor = curation_auth_new_actor(account),
      expires_at = now + auth$session_ttl
    )
  })
  if (!isTRUE(result$ok)) curation_auth_failure()
  result
}

# Short aliases make the seam easy to discover while keeping one implementation.
#' @rdname curation_auth_authenticate
#' @param ... Arguments forwarded to [curation_auth_authenticate()].
#' @export
curation_auth_login <- function(...) curation_auth_authenticate(...)

#' Resolve an authenticated session to the current server-owned actor.
#'
#' @param auth A store from [curation_auth_open()].
#' @param session_token The opaque handle returned by authentication.
#' @param timestamp Current POSIXct or epoch seconds.
#' @param touch Update the idle timestamp. Set to `FALSE` for a reverse-proxy
#'   validity probe; protected app requests should use `TRUE`.
#' @return A `planscanR_curation_actor`, or `NULL` for an invalid/expired session.
#' @export
curation_auth_resolve_session <- function(
  auth,
  session_token,
  timestamp = Sys.time(),
  touch = TRUE
) {
  conn <- curation_auth_db(auth)
  if (!is.character(session_token) || length(session_token) != 1L ||
    is.na(session_token) || !grepl("^[0-9a-f]{64}$", session_token, perl = TRUE)) {
    return(NULL)
  }
  now <- floor(curation_auth_timestamp(timestamp))
  token_hash <- curation_auth_raw_hex(curation_auth_sha256(session_token))
  rows <- DBI::dbGetQuery(
    conn,
    paste(
      "SELECT s.username, s.created_at, s.last_seen, s.expires_at,",
      "a.actor_id, a.display_name, a.roles, a.enabled",
      "FROM auth_sessions s JOIN auth_accounts a ON a.username = s.username",
      "WHERE s.token_hash = ?"
    ),
    params = list(token_hash)
  )
  if (nrow(rows) == 0L || as.integer(rows$enabled[[1L]]) != 1L ||
    now >= as.numeric(rows$expires_at[[1L]]) ||
    now - as.numeric(rows$last_seen[[1L]]) > auth$idle_timeout) {
    if (nrow(rows) > 0L) {
      DBI::dbExecute(conn, "DELETE FROM auth_sessions WHERE token_hash = ?", params = list(token_hash))
    }
    return(NULL)
  }
  if (isTRUE(touch)) {
    DBI::dbExecute(
      conn,
      "UPDATE auth_sessions SET last_seen = ? WHERE token_hash = ?",
      params = list(now, token_hash)
    )
  }
  curation_auth_secure_files(auth$path)
  curation_auth_new_actor(rows[1L, c("actor_id", "display_name", "roles"), drop = FALSE])
}

#' @rdname curation_auth_resolve_session
#' @export
curation_auth_resolve_actor <- function(
  auth,
  session_token,
  timestamp = Sys.time(),
  touch = TRUE
) {
  curation_auth_resolve_session(auth, session_token, timestamp, touch = touch)
}

#' Revoke a server-side authentication session.
#'
#' @param auth A store from [curation_auth_open()].
#' @param session_token The opaque handle returned by authentication.
#' @return Invisibly, `TRUE` when the request was processed.
#' @export
curation_auth_logout <- function(auth, session_token) {
  conn <- curation_auth_db(auth)
  if (is.character(session_token) && length(session_token) == 1L &&
    !is.na(session_token) && grepl("^[0-9a-f]{64}$", session_token, perl = TRUE)) {
    token_hash <- curation_auth_raw_hex(curation_auth_sha256(session_token))
    DBI::dbExecute(conn, "DELETE FROM auth_sessions WHERE token_hash = ?", params = list(token_hash))
  }
  invisible(TRUE)
}

#' Enroll one explicit server account.
#'
#' This is an operator function, not an app route. If `secret` is omitted, a
#' random secret is generated and returned only to this caller for private
#' provisioning. Calling this function is the only operation that can create
#' an account; opening the store never bootstraps a user.
#'
#' @param auth A store from [curation_auth_open()].
#' @param username Login username.
#' @param actor_id Stable server-side actor ID.
#' @param display_name Informational display name.
#' @param roles `reviewer` and/or `admin`.
#' @param secret Optional existing RFC 4648 base32 secret.
#' @param replace Replace an existing username only when explicitly requested.
#' @return A list containing account metadata and the enrollment secret.
#' @export
curation_auth_enroll_account <- function(
  auth,
  username,
  actor_id = username,
  display_name = username,
  roles = "reviewer",
  secret = NULL,
  replace = FALSE
) {
  curation_auth_require_packages()
  conn <- curation_auth_db(auth)
  username <- curation_auth_normalize_username(username)
  actor_id <- curation_auth_scalar(actor_id, "actor_id")
  display_name <- curation_auth_scalar(display_name, "display_name")
  roles <- curation_auth_roles(roles)
  if (!is.null(secret)) {
    secret <- toupper(gsub("[ =-]", "", curation_auth_scalar(secret, "secret")))
    curation_auth_base32_decode(secret)
  } else {
    secret <- curation_auth_random_secret()
  }
  replace <- isTRUE(replace)
  now <- curation_auth_now_text()
  result <- curation_auth_with_transaction(auth, function(conn) {
    existing <- curation_auth_account(conn, username)
    if (!is.null(existing) && !replace) {
      curation_auth_abort("The account already exists; pass replace = TRUE explicitly.")
    }
    if (is.null(existing)) {
      DBI::dbExecute(
        conn,
        paste(
          "INSERT INTO auth_accounts(username, actor_id, display_name, roles, secret_base32,",
          "enabled, created_at, updated_at) VALUES (?, ?, ?, ?, ?, 1, ?, ?)"
        ),
        params = list(username, actor_id, display_name, paste(roles, collapse = ","), secret, now, now)
      )
    } else {
      DBI::dbExecute(
        conn,
        paste(
          "UPDATE auth_accounts SET actor_id = ?, display_name = ?, roles = ?,",
          "secret_base32 = ?, enabled = 1, updated_at = ? WHERE username = ?"
        ),
        params = list(actor_id, display_name, paste(roles, collapse = ","), secret, now, username)
      )
      DBI::dbExecute(conn, "DELETE FROM auth_sessions WHERE username = ?", params = list(username))
      DBI::dbExecute(conn, "DELETE FROM auth_totp_replay WHERE username = ?", params = list(username))
      DBI::dbExecute(conn, "DELETE FROM auth_rate_limits WHERE username = ?", params = list(username))
    }
    list(username = username, actor_id = actor_id, display_name = display_name, roles = roles, secret = secret)
  })
  result$warning <- "Treat secret as enrollment material; never log or commit it."
  result
}

#' @rdname curation_auth_enroll_account
#' @param ... Arguments forwarded to [curation_auth_enroll_account()].
#' @export
curation_auth_enroll <- function(...) curation_auth_enroll_account(...)

#' Enable or disable an enrolled account and revoke its sessions.
#'
#' @param auth A store from [curation_auth_open()].
#' @param username Account username.
#' @param enabled Whether the account may authenticate.
#' @return Invisibly, `TRUE`.
#' @export
curation_auth_set_account_enabled <- function(auth, username, enabled = TRUE) {
  conn <- curation_auth_db(auth)
  username <- curation_auth_normalize_username(username)
  if (!is.logical(enabled) || length(enabled) != 1L || is.na(enabled)) {
    curation_auth_abort("{.arg enabled} must be one non-missing logical value.")
  }
  changed <- DBI::dbExecute(
    conn,
    "UPDATE auth_accounts SET enabled = ?, updated_at = ? WHERE username = ?",
    params = list(as.integer(enabled), curation_auth_now_text(), username)
  )
  if (changed > 0L && !isTRUE(enabled)) {
    DBI::dbExecute(conn, "DELETE FROM auth_sessions WHERE username = ?", params = list(username))
  }
  invisible(TRUE)
}
