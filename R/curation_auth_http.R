# Loopback HTTP authentication gate for the native BIOGAIN deployment.
#
# This service is deliberately small.  It owns the login/logout form and the
# nginx auth_request validity probe; it does not proxy, render, or serve the
# Shiny application.  nginx remains responsible for forwarding an authorized
# request to Shiny after this gate returns 200.

curation_auth_http_require_package <- function(package, message) {
  if (!requireNamespace(package, quietly = TRUE)) {
    curation_auth_abort(
      message,
      class = "planscanR_error_curation_missing_dependency"
    )
  }
  invisible(TRUE)
}

curation_auth_http_path <- function(path, arg, trailing = FALSE) {
  path <- curation_auth_scalar(path, arg)
  if (!startsWith(path, "/") || grepl("[?#\\r\\n]", path, perl = TRUE)) {
    curation_auth_abort(
      paste0("The ", arg, " must be an absolute URL path without a query.")
    )
  }
  if (grepl("//|(?:^|/)\\.(?:/|$)|(?:^|/)\\.\\.(?:/|$)", path, perl = TRUE)) {
    curation_auth_abort(paste0("The ", arg, " contains a non-canonical path segment."))
  }
  if (isTRUE(trailing)) {
    if (identical(path, "/") || !endsWith(path, "/")) {
      curation_auth_abort(paste0("The ", arg, " must end with one slash."))
    }
    if (nchar(path) > 1L && endsWith(substr(path, 1L, nchar(path) - 1L), "/")) {
      curation_auth_abort(paste0("The ", arg, " must not have repeated trailing slashes."))
    }
  } else if (identical(path, "/") || endsWith(path, "/")) {
    curation_auth_abort(paste0("The ", arg, " must be a canonical route without a trailing slash."))
  }
  path
}

curation_auth_http_app_prefix <- function(app_path) {
  app_path <- curation_auth_http_path(app_path, "app_path", trailing = TRUE)
  if (identical(app_path, "/")) {
    curation_auth_abort("The app_path must identify a non-root application prefix.")
  }
  app_path
}

curation_auth_http_config <- function(
  expected_origin,
  app_path = "/app/",
  cookie_name = "biogain_session",
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
  login_title = "Curation login"
) {
  has_expected <- is.character(expected_origin) && length(expected_origin) == 1L &&
    !is.na(expected_origin) && nzchar(expected_origin)
  has_public <- is.character(public_origin) && length(public_origin) == 1L &&
    !is.na(public_origin) && nzchar(public_origin)
  if (!has_expected && has_public) {
    expected_origin <- public_origin
  } else if (has_expected && has_public && !identical(
    curation_auth_expected_origin(expected_origin),
    curation_auth_expected_origin(public_origin)
  )) {
    curation_auth_abort("expected_origin and public_origin must identify the same origin.")
  }
  if (!is.character(expected_origin) || length(expected_origin) != 1L ||
      is.na(expected_origin) || !nzchar(expected_origin)) {
    curation_auth_abort("expected_origin must be an exact HTTPS origin.")
  }
  expected_origin <- curation_auth_expected_origin(expected_origin)
  app_path <- curation_auth_http_app_prefix(app_path)
  cookie_name <- curation_auth_cookie_name(cookie_name)
  route <- function(value, fallback, name) {
    if (is.null(value)) fallback else curation_auth_http_path(value, name)
  }
  login_path <- route(login_path, paste0(app_path, "login"), "login_path")
  public_login_path <- route(
    public_login_path, paste0(app_path, "login"), "public_login_path"
  )
  check_path <- route(check_path, paste0(app_path, "_auth/validate"), "check_path")
  logout_path <- route(logout_path, paste0(app_path, "logout"), "logout_path")
  public_logout_path <- route(
    public_logout_path, paste0(app_path, "logout"), "public_logout_path"
  )
  health_path <- route(health_path, paste0(app_path, "health"), "health_path")
  if (!startsWith(public_login_path, app_path) || !startsWith(public_logout_path, app_path)) {
    curation_auth_abort("The public login and logout paths must be under app_path.")
  }
  max_body_bytes <- curation_auth_integer(
    max_body_bytes, "max_body_bytes", lower = 256L, upper = 1048576L
  )
  cookie_max_age <- curation_auth_integer(
    cookie_max_age, "cookie_max_age", lower = 60L, upper = 604800L
  )
  login_title <- curation_auth_scalar(login_title, "login_title")
  if (grepl("[<>\"&\\r\\n]", login_title, perl = TRUE)) {
    curation_auth_abort("The login_title contains unsafe HTML characters.")
  }
  if (!is.null(trusted_proxy_header)) {
    trusted_proxy_header <- curation_auth_scalar(trusted_proxy_header, "trusted_proxy_header")
    if (!grepl("^[A-Za-z][A-Za-z0-9-]{0,63}$", trusted_proxy_header, perl = TRUE)) {
      curation_auth_abort("The trusted_proxy_header is invalid.")
    }
    trusted_proxy_value <- curation_auth_scalar(trusted_proxy_value, "trusted_proxy_value")
    if (grepl("[\\r\\n]", trusted_proxy_value, perl = TRUE)) {
      curation_auth_abort("The trusted_proxy_value contains a newline.")
    }
  }
  list(
    expected_origin = expected_origin,
    expected_host = sub("^https://", "", expected_origin),
    app_path = app_path,
    cookie_path = sub("/$", "", app_path),
    cookie_name = cookie_name,
    login_path = login_path,
    public_login_path = public_login_path,
    check_path = check_path,
    logout_path = logout_path,
    public_logout_path = public_logout_path,
    health_path = health_path,
    trusted_proxy_header = trusted_proxy_header,
    trusted_proxy_value = trusted_proxy_value,
    max_body_bytes = max_body_bytes,
    cookie_max_age = cookie_max_age,
    login_title = login_title
  )
}

curation_auth_http_header <- function(request, names) {
  for (name in names) {
    value <- curation_auth_request_header(request, name)
    if (!is.null(value)) return(value)
  }
  NULL
}

curation_auth_http_request_method <- function(request) {
  method <- curation_auth_request_member(request, "REQUEST_METHOD")
  if (is.null(method)) method <- curation_auth_request_member(request, "request_method")
  if (!is.character(method) || length(method) != 1L || is.na(method)) return("")
  toupper(method)
}

curation_auth_http_request_path <- function(request) {
  path <- curation_auth_request_member(request, "PATH_INFO")
  if (is.null(path)) path <- curation_auth_request_member(request, "path_info")
  if (is.null(path)) path <- curation_auth_request_member(request, "REQUEST_URI")
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) return("")
  path <- sub("[?#].*$", "", path, perl = TRUE)
  if (!startsWith(path, "/") || grepl("[\\r\\n]", path, perl = TRUE) ||
      grepl("//|(?:^|/)\\.(?:/|$)|(?:^|/)\\.\\.(?:/|$)", path, perl = TRUE) ||
      grepl("%2[eEfFcC]|%5[cC]", path, perl = TRUE)) return("")
  path
}

curation_auth_http_body <- function(request, max_bytes) {
  body <- curation_auth_request_member(request, "body")
  if (is.null(body)) body <- curation_auth_request_member(request, "BODY")
  if (is.null(body)) {
    input <- curation_auth_request_member(request, "rook.input")
    if (is.null(input)) input <- curation_auth_request_member(request, "rook_input")
    reader <- if (is.list(input) || is.environment(input)) {
      curation_auth_request_member(input, "read")
    } else {
      NULL
    }
    if (is.function(reader)) {
      body <- tryCatch(reader(), error = function(error) NULL)
    }
  }
  if (is.null(body)) return("")
  if (is.raw(body)) {
    if (length(body) > max_bytes) return(NULL)
    return(rawToChar(body))
  }
  if (!is.character(body) || length(body) != 1L || is.na(body)) return(NULL)
  if (nchar(body, type = "bytes") > max_bytes) return(NULL)
  body
}

curation_auth_http_url_decode <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      grepl("%(?![0-9A-Fa-f]{2})", value, perl = TRUE)) {
    return(NULL)
  }
  tryCatch(utils::URLdecode(gsub("+", " ", value, fixed = TRUE)), error = function(error) NULL)
}

curation_auth_http_form <- function(body) {
  if (!is.character(body) || length(body) != 1L || !nzchar(body)) return(NULL)
  fields <- strsplit(body, "&", fixed = TRUE)[[1L]]
  parsed <- list()
  for (field in fields) {
    pieces <- strsplit(field, "=", fixed = TRUE)[[1L]]
    if (length(pieces) != 2L) return(NULL)
    key <- curation_auth_http_url_decode(pieces[[1L]])
    value <- curation_auth_http_url_decode(pieces[[2L]])
    if (is.null(key) || is.null(value) || !nzchar(key) || key %in% names(parsed)) return(NULL)
    parsed[[key]] <- value
  }
  if (!identical(sort(names(parsed)), c("otp", "username"))) return(NULL)
  parsed
}

curation_auth_http_headers <- function(headers = list(), request_method = "GET") {
  base <- list(
    `Content-Type` = "text/plain; charset=utf-8",
    `Cache-Control` = "no-store",
    `X-Content-Type-Options` = "nosniff",
    `Referrer-Policy` = "no-referrer"
  )
  if (length(headers) > 0L) {
    if (is.null(names(headers)) || any(!nzchar(names(headers)))) {
      curation_auth_abort("HTTP response headers must be named.")
    }
    base[names(headers)] <- as.list(unname(unlist(headers, use.names = FALSE)))
  }
  if (identical(request_method, "HEAD")) base
  else base
}

curation_auth_http_response_object <- function(
  status,
  body = "",
  headers = list(),
  request_method = "GET"
) {
  status <- as.integer(status)
  if (!is.finite(status) || status < 100L || status > 599L) status <- 500L
  if (!is.null(body) && !is.character(body) && !is.raw(body)) body <- as.character(body)
  if (identical(request_method, "HEAD")) body <- NULL
  list(
    status = status,
    headers = curation_auth_http_headers(headers, request_method),
    body = body
  )
}

curation_auth_http_login_page <- function(action, title = "Curation login") {
  # `action` has already passed the canonical route validator; it is the only
  # deployment value interpolated into this static page.
  action <- gsub("&", "&amp;", gsub("\"", "&quot;", action, fixed = TRUE), fixed = TRUE)
  title <- gsub("&", "&amp;", gsub("\"", "&quot;", title, fixed = TRUE), fixed = TRUE)
  paste0(
    "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"referrer\" content=\"same-origin\"><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
    "<title>", title, "</title></head><body><main><h1>", title, "</h1>",
    "<form method=\"post\" action=\"", action, "\">",
    "<label>Username <input name=\"username\" autocomplete=\"username\" required></label>",
    "<label>Authenticator code <input name=\"otp\" inputmode=\"numeric\" autocomplete=\"one-time-code\" pattern=\"[0-9]{6,8}\" required></label>",
    "<button type=\"submit\">Sign in</button></form></main></body></html>"
  )
}

curation_auth_http_failure <- function(status = 401L, request_method = "GET", headers = list()) {
  curation_auth_http_response_object(
    status,
    "Authentication failed.\n",
    headers = headers,
    request_method = request_method
  )
}

curation_auth_http_forbidden <- function(request_method = "GET") {
  curation_auth_http_response_object(403L, "Request not authorized.\n", request_method = request_method)
}

curation_auth_http_upgrade <- function(request) {
  value <- curation_auth_http_header(request, c("HTTP_UPGRADE", "Upgrade"))
  !is.null(value) && identical(tolower(trimws(value)), "websocket")
}

curation_auth_http_origin_request <- function(request, original = FALSE) {
  # nginx overwrites the canonical Origin header in the internal subrequest.
  # Do not accept X-Original-Origin: clients can send that header themselves
  # unless a deployment explicitly adds a separate trusted metadata contract.
  curation_auth_http_header(request, c("HTTP_ORIGIN", "Origin"))
}

curation_auth_http_assert_origin <- function(request, expected_origin, original = FALSE) {
  origin <- curation_auth_http_origin_request(request, original = original)
  if (is.null(origin)) return(FALSE)
  tryCatch({
    curation_auth_assert_mutation_origin(
      list(HEADERS = c(HTTP_ORIGIN = origin)), expected_origin
    )
    TRUE
  }, error = function(error) FALSE)
}

curation_auth_http_forwarded_uri <- function(request) {
  uri <- curation_auth_http_header(request, c("HTTP_X_ORIGINAL_URI", "X-Original-URI"))
  if (is.null(uri) || !is.character(uri) || length(uri) != 1L ||
      !startsWith(uri, "/") || grepl("[\\r\\n]", uri, perl = TRUE)) return(NULL)
  if (grepl("://", uri, fixed = TRUE)) return(NULL)
  uri <- sub("[?#].*$", "", uri, perl = TRUE)
  if (grepl("//|(?:^|/)\\.(?:/|$)|(?:^|/)\\.\\.(?:/|$)", uri, perl = TRUE) ||
      grepl("%2[eEfFcC]|%5[cC]", uri, perl = TRUE)) return(NULL)
  uri
}

curation_auth_http_trusted_subrequest <- function(request, config) {
  if (!is.null(config$trusted_proxy_header)) {
    value <- curation_auth_http_header(request, c(
      paste0("HTTP_", toupper(gsub("-", "_", config$trusted_proxy_header))),
      config$trusted_proxy_header
    ))
    if (is.null(value) || !identical(value, config$trusted_proxy_value)) return(FALSE)
  }
  original_uri <- curation_auth_http_forwarded_uri(request)
  original_method <- curation_auth_http_header(request, c("HTTP_X_ORIGINAL_METHOD", "X-Original-Method"))
  forwarded_proto <- curation_auth_http_header(request, c("HTTP_X_FORWARDED_PROTO", "X-Forwarded-Proto"))
  if (is.null(original_uri) || is.null(original_method) || is.null(forwarded_proto)) return(FALSE)
  if (!identical(tolower(forwarded_proto), "https")) return(FALSE)
  original_method <- toupper(original_method)
  if (!original_method %in% c("GET", "HEAD", "POST", "PUT", "PATCH", "DELETE", "OPTIONS")) return(FALSE)
  list(
    uri = original_uri,
    method = original_method,
    upgrade = identical(
      tolower(curation_auth_http_header(request, c("HTTP_X_ORIGINAL_UPGRADE", "X-Original-Upgrade")) %||% ""),
      "websocket"
    )
  )
}

curation_auth_http_path_under_app <- function(path, config, allow_prefix = TRUE) {
  if (is.null(path) || !is.character(path) || length(path) != 1L) return(FALSE)
  if (identical(path, sub("/$", "", config$app_path))) return(isTRUE(allow_prefix))
  startsWith(path, config$app_path)
}

curation_auth_http_actor_headers <- function(actor) {
  if (is.null(actor) || !inherits(actor, "planscanR_curation_actor")) return(list())
  roles <- paste(sort(unique(as.character(actor$roles))), collapse = ",")
  if (grepl("[\\r\\n]", actor$id, perl = TRUE) || grepl("[\\r\\n]", roles, perl = TRUE)) {
    return(list())
  }
  list("X-Biogain-Actor" = as.character(actor$id), "X-Biogain-Roles" = roles)
}

curation_auth_http_response <- function(auth, request, config) {
  method <- curation_auth_http_request_method(request)
  path <- curation_auth_http_request_path(request)
  if (!method %in% c("GET", "HEAD", "POST", "PUT", "PATCH", "DELETE", "OPTIONS")) {
    return(curation_auth_http_response_object(400L, "Bad request.\n", request_method = method))
  }
  if (!nzchar(path)) {
    return(curation_auth_http_response_object(400L, "Bad request.\n", request_method = method))
  }

  if (identical(path, sub("/$", "", config$app_path))) {
    return(curation_auth_http_response_object(
      308L, "", headers = list(Location = config$app_path), request_method = method
    ))
  }
  if (identical(path, config$health_path)) {
    if (!method %in% c("GET", "HEAD")) {
      return(curation_auth_http_response_object(405L, "Method not allowed.\n", headers = list(Allow = "GET, HEAD"), request_method = method))
    }
    return(curation_auth_http_response_object(
      200L, "ok\n", headers = list(`Content-Type` = "text/plain; charset=utf-8"), request_method = method
    ))
  }
  if (identical(path, config$login_path)) {
    if (identical(method, "GET") || identical(method, "HEAD")) {
      return(curation_auth_http_response_object(
        200L,
        curation_auth_http_login_page(config$public_login_path, config$login_title),
        headers = list(`Content-Type` = "text/html; charset=utf-8", `Content-Security-Policy` = "default-src 'none'; form-action 'self'; base-uri 'none'", `Referrer-Policy` = "same-origin"),
        request_method = method
      ))
    }
    if (!identical(method, "POST")) {
      return(curation_auth_http_response_object(405L, "Method not allowed.\n", headers = list(Allow = "GET, HEAD, POST"), request_method = method))
    }
    if (!curation_auth_http_assert_origin(request, config$expected_origin)) {
      return(curation_auth_http_forbidden(method))
    }
    content_type <- curation_auth_http_header(request, c("CONTENT_TYPE", "Content-Type")) %||% ""
    if (!startsWith(tolower(content_type), "application/x-www-form-urlencoded")) {
      return(curation_auth_http_failure(request_method = method))
    }
    body <- curation_auth_http_body(request, config$max_body_bytes)
    form <- curation_auth_http_form(body)
    if (is.null(form)) return(curation_auth_http_failure(request_method = method))
    result <- tryCatch(
      curation_auth_authenticate(auth, form$username, form$otp),
      error = function(error) NULL
    )
    if (is.null(result) || !isTRUE(result$ok) || is.null(result$session_token)) {
      return(curation_auth_http_failure(request_method = method))
    }
    return(curation_auth_http_response_object(
      303L,
      "",
      headers = list(
        Location = config$app_path,
        `Set-Cookie` = curation_auth_cookie_header(
          result$session_token,
          cookie_name = config$cookie_name,
          path = config$cookie_path,
          max_age = config$cookie_max_age
        )
      ),
      request_method = method
    ))
  }
  if (identical(path, config$logout_path)) {
    if (!identical(method, "POST")) {
      return(curation_auth_http_response_object(405L, "Method not allowed.\n", headers = list(Allow = "POST"), request_method = method))
    }
    if (!curation_auth_http_assert_origin(request, config$expected_origin)) {
      return(curation_auth_http_forbidden(method))
    }
    token <- curation_auth_request_session_token(request, config$cookie_name)
    if (!is.null(token)) try(curation_auth_logout(auth, token), silent = TRUE)
    return(curation_auth_http_response_object(
      303L,
      "",
      headers = list(
        Location = config$public_login_path,
        `Set-Cookie` = curation_auth_expire_cookie_header(
          cookie_name = config$cookie_name, path = config$cookie_path
        )
      ),
      request_method = method
    ))
  }
  if (identical(path, config$check_path)) {
    if (!identical(method, "GET")) {
      return(curation_auth_http_response_object(405L, "Method not allowed.\n", headers = list(Allow = "GET"), request_method = method))
    }
    forwarded <- curation_auth_http_trusted_subrequest(request, config)
    if (isFALSE(forwarded)) {
      # A missing/invalid auth_request envelope is a proxy configuration
      # failure, not an anonymous session.  Do not authorize a direct caller.
      return(curation_auth_http_response_object(403L, "Request not authorized.\n", request_method = method))
    }
    if (!curation_auth_http_path_under_app(forwarded$uri, config)) {
      return(curation_auth_http_response_object(400L, "Bad request.\n", request_method = method))
    }
    mutation <- forwarded$upgrade || forwarded$method %in% c("POST", "PUT", "PATCH", "DELETE")
    if (mutation && !curation_auth_http_assert_origin(request, config$expected_origin, original = TRUE)) {
      return(curation_auth_http_forbidden(method))
    }
    # nginx may probe a WebSocket or download repeatedly while the browser is
    # idle.  A validity probe must not extend the session idle lifetime; the
    # Shiny request resolver performs the touch for an actual app request.
    actor <- curation_auth_http_actor(auth, request, config$cookie_name, touch = FALSE)
    if (is.null(actor)) return(curation_auth_http_failure(request_method = method))
    return(curation_auth_http_response_object(
      200L,
      "",
      headers = c(list(`Content-Type` = "text/plain; charset=utf-8"), curation_auth_http_actor_headers(actor)),
      request_method = method
    ))
  }

  if (!curation_auth_http_path_under_app(path, config)) {
    return(curation_auth_http_response_object(404L, "Not found.\n", request_method = method))
  }
  actor <- curation_auth_http_actor(auth, request, config$cookie_name, touch = TRUE)
  if (is.null(actor)) return(curation_auth_http_failure(request_method = method))
  mutation <- curation_auth_http_upgrade(request) || method %in% c("POST", "PUT", "PATCH", "DELETE")
  if (mutation && !curation_auth_http_assert_origin(request, config$expected_origin)) {
    return(curation_auth_http_forbidden(method))
  }
  curation_auth_http_response_object(
    200L,
    "",
    headers = c(list(`Content-Type` = "text/plain; charset=utf-8"), curation_auth_http_actor_headers(actor)),
    request_method = method
  )
}

curation_auth_http_on_headers <- function(request, config) {
  method <- curation_auth_http_request_method(request)
  if (!identical(method, "POST")) return(NULL)
  length_header <- curation_auth_http_header(request, c("CONTENT_LENGTH", "Content-Length"))
  if (is.null(length_header)) return(NULL)
  length_value <- suppressWarnings(as.numeric(length_header))
  if (length(length_value) != 1L || is.na(length_value) || length_value < 0) {
    return(curation_auth_http_response_object(400L, "Bad request.\n", request_method = method))
  }
  if (length_value > config$max_body_bytes) {
    return(curation_auth_http_response_object(413L, "Request body too large.\n", request_method = method))
  }
  NULL
}

curation_auth_http_app <- function(auth, config) {
  list(
    onHeaders = function(request) curation_auth_http_on_headers(request, config),
    call = function(request) curation_auth_http_response(auth, request, config)
  )
}

#' Start the loopback HTTP authentication gate.
#'
#' The gate exposes only the login/logout form, an nginx `auth_request`
#' validity endpoint, and a minimal health endpoint.  It never proxies the
#' Shiny application.  The default routes are rooted at `/app/`; deployment
#' runners may pass explicit `/auth/*` and `/healthz` routes while keeping the
#' protected app prefix and cookie policy unchanged.
#'
#' @param auth A store from [curation_auth_open()].
#' @param host Listener host. Only `127.0.0.1` is accepted.
#' @param port Unprivileged listener port.
#' @param expected_origin Exact public HTTPS origin.
#' @param app_path Canonical protected application prefix, ending in `/`.
#' @param cookie_name Opaque session-cookie name.
#' @param blocking Keep servicing the server until interrupted. Native service
#'   runners should use the default; tests can set `FALSE` and call `$stop()`.
#' @param login_path Optional internal login-handler route override. The
#'   browser-visible form action is `public_login_path`.
#' @param public_login_path Browser-visible login route, normally `/app/login`.
#' @param check_path Optional internal auth-request route override.
#' @param logout_path Optional internal logout-handler route override.
#' @param public_logout_path Browser-visible logout route, normally
#'   `/app/logout`.
#' @param health_path Optional health route override.
#' @param public_origin Backward-compatible alias for `expected_origin`.
#' @param trusted_proxy_header Optional exact header nginx sets on internal
#'   auth subrequests. The loopback deployment also requires forwarded URI,
#'   method, HTTPS, and (for mutations) original origin metadata.
#' @param trusted_proxy_value Required value for `trusted_proxy_header`.
#' @param max_body_bytes Maximum login form body size.
#' @param cookie_max_age Cookie lifetime in seconds.
#' @param login_title Plain-text heading for the login form. Defaults to the
#'   generic `Curation login`; project deployment runners may supply branding.
#' @param quiet Suppress httpuv startup diagnostics.
#' @param ... Reserved for deployment-runner compatibility; route and origin
#'   options should be supplied through their named arguments.
#' @return An object with `stop()`, `server`, and `config` when `blocking` is
#'   `FALSE`; a stopped object invisibly after an interrupt otherwise.
#' @export
curation_auth_http_gate <- function(
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
) {
  if (!inherits(auth, "planscanR_curation_auth")) {
    curation_auth_abort("auth must be a store returned by curation_auth_open().")
  }
  host <- curation_auth_scalar(host, "host")
  if (!identical(host, "127.0.0.1")) {
    curation_auth_abort("The authentication gate must bind to 127.0.0.1 only.")
  }
  port <- curation_auth_integer(port, "port", lower = 1024L, upper = 65535L)
  if (!is.logical(blocking) || length(blocking) != 1L || is.na(blocking)) {
    curation_auth_abort("blocking must be one logical value.")
  }
  config <- curation_auth_http_config(
    expected_origin = expected_origin,
    app_path = app_path,
    cookie_name = cookie_name,
    login_path = login_path,
    public_login_path = public_login_path,
    check_path = check_path,
    logout_path = logout_path,
    public_logout_path = public_logout_path,
    health_path = health_path,
    public_origin = public_origin,
    trusted_proxy_header = trusted_proxy_header,
    trusted_proxy_value = trusted_proxy_value,
    max_body_bytes = max_body_bytes,
    cookie_max_age = cookie_max_age,
    login_title = login_title
  )
  curation_auth_http_require_package(
    "httpuv", "The HTTP authentication gate needs the optional httpuv package."
  )
  app <- curation_auth_http_app(auth, config)
  server <- tryCatch(
    httpuv::startServer(host, port, app, quiet = isTRUE(quiet)),
    error = function(error) {
      curation_auth_abort("The HTTP authentication gate could not start.", parent = error)
    }
  )
  state <- new.env(parent = emptyenv())
  state$server <- server
  state$config <- config
  state$host <- host
  state$port <- port
  state$closed <- FALSE
  state$stop <- function() {
    if (!isTRUE(state$closed)) {
      try(httpuv::stopServer(state$server), silent = TRUE)
      state$closed <- TRUE
    }
    invisible(TRUE)
  }
  class(state) <- c("planscanR_curation_http_gate", "environment")
  if (isTRUE(blocking)) {
    on.exit(state$stop(), add = TRUE)
    tryCatch(
      repeat httpuv::service(1000),
      interrupt = function(error) invisible(NULL)
    )
    return(invisible(state))
  }
  state
}
