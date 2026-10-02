# Shiny/request integration for the server-owned auth store.  This file does
# not trust identity or role headers.  The only client credential it extracts
# is an opaque cookie whose digest is checked against the auth SQLite store.

curation_auth_cookie_name <- function(cookie_name = "biogain_session") {
  cookie_name <- curation_auth_scalar(cookie_name, "cookie_name")
  if (!grepl("^[A-Za-z][A-Za-z0-9_-]{0,63}$", cookie_name, perl = TRUE)) {
    curation_auth_abort("The authentication cookie name is invalid.")
  }
  cookie_name
}

curation_auth_request_member <- function(request, name) {
  if (is.list(request) && !is.null(request[[name]])) {
    return(request[[name]])
  }
  if (is.environment(request) && exists(name, envir = request, inherits = FALSE)) {
    return(get(name, envir = request, inherits = FALSE))
  }
  NULL
}

curation_auth_request_headers <- function(request) {
  if (!is.list(request) && !is.environment(request)) {
    return(list())
  }
  headers <- curation_auth_request_member(request, "HEADERS")
  if (is.null(headers)) headers <- curation_auth_request_member(request, "headers")
  nested_request <- curation_auth_request_member(request, "request")
  if (is.null(headers) && (is.list(nested_request) || is.environment(nested_request))) {
    headers <- curation_auth_request_headers(nested_request)
  }
  session <- curation_auth_request_member(request, "session")
  if (is.null(headers) && (is.list(session) || is.environment(session))) {
    session_request <- curation_auth_request_member(session, "request")
    if (is.list(session_request) || is.environment(session_request)) {
      headers <- curation_auth_request_headers(session_request)
    }
  }
  if (is.null(headers)) headers <- request
  if (!is.list(headers) && !is.atomic(headers) && !is.environment(headers)) headers <- list()
  headers
}

curation_auth_request_header <- function(request, name) {
  headers <- curation_auth_request_headers(request)
  if (is.environment(headers)) {
    keys <- ls(envir = headers, all.names = TRUE)
    index <- match(tolower(name), tolower(keys))
    if (is.na(index)) {
      return(NULL)
    }
    value <- get(keys[[index]], envir = headers, inherits = FALSE)
    if (!is.character(value) || length(value) != 1L || is.na(value)) {
      return(NULL)
    }
    return(value)
  }
  keys <- names(headers)
  if (is.null(keys)) {
    return(NULL)
  }
  index <- match(tolower(name), tolower(keys))
  if (is.na(index)) {
    return(NULL)
  }
  value <- headers[[index]]
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    return(NULL)
  }
  value
}

#' Extract an opaque auth cookie from a server request.
#'
#' Only the cookie value is returned. Identity and role headers are ignored;
#' the caller must pass this value to [curation_auth_resolve_session()].
#'
#' @param request A server-side Shiny/httpuv request list.
#' @param cookie_name Session-cookie name.
#' @return A 64-character session handle, or `NULL` when absent/malformed.
#' @export
curation_auth_request_session_token <- function(request, cookie_name = "biogain_session") {
  cookie_name <- curation_auth_cookie_name(cookie_name)
  header <- curation_auth_request_header(request, "HTTP_COOKIE")
  if (is.null(header)) header <- curation_auth_request_header(request, "Cookie")
  if (is.null(header)) {
    return(NULL)
  }
  fields <- strsplit(header, ";", fixed = TRUE)[[1L]]
  fields <- trimws(fields)
  prefix <- paste0(cookie_name, "=")
  value <- fields[startsWith(fields, prefix)]
  if (length(value) != 1L) {
    return(NULL)
  }
  token <- sub(paste0("^", cookie_name, "="), "", value)
  if (!grepl("^[0-9a-f]{64}$", token, perl = TRUE)) {
    return(NULL)
  }
  token
}

#' Resolve a server-side HTTP request to its current actor.
#'
#' This is the small seam used by an internal httpuv auth gate and by the
#' Shiny application. The request's cookie is only a lookup handle; the
#' SQLite-backed resolver remains the authority.
#'
#' @param auth A store from [curation_auth_open()].
#' @param request A server-side Shiny/httpuv request list or environment.
#' @param cookie_name Session-cookie name.
#' @param touch Update the session idle timestamp.
#' @return A `planscanR_curation_actor`, or `NULL` for an invalid session.
#' @export
curation_auth_http_actor <- function(
  auth,
  request,
  cookie_name = "biogain_session",
  touch = FALSE
) {
  token <- curation_auth_request_session_token(request, cookie_name)
  if (is.null(token)) {
    return(NULL)
  }
  curation_auth_resolve_session(auth, token, touch = touch)
}

curation_auth_cookie_token <- function(session_token) {
  if (!is.character(session_token) || length(session_token) != 1L ||
    is.na(session_token) || !grepl("^[0-9a-f]{64}$", session_token, perl = TRUE)) {
    curation_auth_abort("The session token is invalid.")
  }
  session_token
}

#' Render the Secure, HttpOnly session cookie used by the auth gate.
#'
#' @param session_token Opaque token returned by authentication.
#' @param cookie_name Session-cookie name.
#' @param path Cookie path, normally `/app`.
#' @param max_age Session lifetime in seconds.
#' @return A `Set-Cookie` header value.
#' @export
curation_auth_cookie_header <- function(
  session_token,
  cookie_name = "biogain_session",
  path = "/app",
  max_age = 43200L
) {
  session_token <- curation_auth_cookie_token(session_token)
  cookie_name <- curation_auth_cookie_name(cookie_name)
  path <- curation_auth_scalar(path, "path")
  if (!startsWith(path, "/") || grepl("[;\\r\\n]", path, perl = TRUE)) {
    curation_auth_abort("The cookie path is invalid.")
  }
  max_age <- curation_auth_integer(max_age, "max_age", lower = 1L, upper = 604800L)
  paste0(
    cookie_name, "=", session_token,
    "; Path=", path,
    "; Max-Age=", max_age,
    "; Secure; HttpOnly; SameSite=Strict"
  )
}

#' Render a deletion header for the auth session cookie.
#'
#' @param cookie_name Session-cookie name.
#' @param path Cookie path used when the session was created.
#' @return A `Set-Cookie` header value.
#' @export
curation_auth_expire_cookie_header <- function(cookie_name = "biogain_session", path = "/app") {
  cookie_name <- curation_auth_cookie_name(cookie_name)
  path <- curation_auth_scalar(path, "path")
  if (!startsWith(path, "/") || grepl("[;\\r\\n]", path, perl = TRUE)) {
    curation_auth_abort("The cookie path is invalid.")
  }
  paste0(cookie_name, "=; Path=", path, "; Max-Age=0; Secure; HttpOnly; SameSite=Strict")
}

#' Build the authenticated actor resolver used by the Shiny app.
#'
#' The resolver reads the session cookie from the server-side request and
#' rechecks the SQLite session and current account roles on every call. It
#' ignores `X-Forwarded-*`, identity, display-name, and role headers.
#'
#' @param auth A store from [curation_auth_open()].
#' @param cookie_name Session-cookie name.
#' @param touch Update the session idle timestamp. Use `FALSE` for background
#'   validity probes such as a timer; user requests should use `TRUE`.
#' @return A `planscanR_curation_actor_resolver`.
#' @export
curation_auth_shiny_actor_resolver <- function(
  auth,
  cookie_name = "biogain_session",
  touch = TRUE
) {
  cookie_name <- curation_auth_cookie_name(cookie_name)
  if (!is.logical(touch) || length(touch) != 1L || is.na(touch)) {
    curation_auth_abort("{.arg touch} must be one non-missing logical value.")
  }
  curation_actor_resolver(function(request) {
    token <- curation_auth_request_session_token(request, cookie_name)
    actor <- if (is.null(token)) {
      NULL
    } else {
      curation_auth_resolve_session(auth, token, touch = touch)
    }
    if (is.null(actor)) {
      curation_auth_abort(
        "Authentication is required for this request.",
        class = "planscanR_error_curation_authentication"
      )
    }
    actor
  })
}

#' Resolve a Shiny session to its current authenticated actor.
#'
#' @param auth A store from [curation_auth_open()].
#' @param session A Shiny session object with a server-side request.
#' @param cookie_name Session-cookie name.
#' @param touch Update the session idle timestamp.
#' @return A `planscanR_curation_actor`.
#' @export
curation_auth_shiny_actor <- function(
  auth,
  session,
  cookie_name = "biogain_session",
  touch = TRUE
) {
  if (!is.list(session) && !is.environment(session)) {
    curation_auth_abort("The Shiny session is invalid.")
  }
  request <- tryCatch(session$request, error = function(error) NULL)
  resolver <- curation_auth_shiny_actor_resolver(auth, cookie_name, touch = touch)
  curation_resolve_actor(resolver, list(session = session, request = request))
}

curation_auth_expected_origin <- function(origin) {
  origin <- curation_auth_scalar(origin, "expected_origin")
  if (!grepl(
    "^https://(?:[A-Za-z0-9.-]+|\\[[0-9A-Fa-f:.]+\\])(?::[0-9]{1,5})?$",
    origin,
    perl = TRUE
  )) {
    curation_auth_abort("The expected mutation origin must be an exact HTTPS origin.")
  }
  origin
}

#' Require the exact HTTPS origin for a state-changing request.
#'
#' The reverse proxy supplies the origin policy as deployment configuration;
#' this helper compares the server-received `Origin` header byte-for-byte after
#' normalizing the header's URL encoding. Missing or foreign origins fail
#' closed. Same-origin browser requests must therefore include the expected
#' header for mutations.
#'
#' @param request Server-side Shiny/httpuv request list.
#' @param expected_origin Exact origin, for example
#'   `https://biogain.continental-dat.src.surf-hosted.nl`.
#' @return Invisibly, `TRUE`.
#' @export
curation_auth_assert_mutation_origin <- function(request, expected_origin) {
  expected_origin <- curation_auth_expected_origin(expected_origin)
  origin <- curation_auth_request_header(request, "HTTP_ORIGIN")
  if (is.null(origin)) origin <- curation_auth_request_header(request, "Origin")
  if (is.null(origin) || !identical(origin, expected_origin)) {
    curation_auth_abort(
      "The request origin is not authorized.",
      class = "planscanR_error_curation_csrf"
    )
  }
  invisible(TRUE)
}
