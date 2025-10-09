# Silence
globalVariables(
  c(
    'private',
    'self',
    'super'
  )
)

#' Select a random safe port
#'
#' This is a small utility function to get random safe ports to run your
#' application on. It chooses a port within the range that cannot be registeret
#' to IANA and thus is safe to assume are not in use.
#'
#' @return An integer in the range 49152-65535
#'
#' @export
#'
#' @examples
#' random_port()
#'
#' @keywords internal
random_port <- function() {
  low <- 49152
  high <- 65535
  as.integer(sample(high - low, 1) + low)
}

#' Use a session cookie to store the id of the session
#'
#' This function constructs an ID extractor for incoming requests, the return
#' value of which will be passed to the id argument of request, header, message
#' etc handlers. By default, fiery uses this with default arguments.
#'
#' A session id is looked for in the cookie matching the `name` arg. If it is
#' found then the value of the cookie is returned. If it is not found then an
#' id is generated with [reqres::random_key()] and attached to the response as
#' a cookie with the given name. The cookie is set to HTTP only and Strict same
#' site policy. Depending on `secure` it also sets it to only be transferred
#' over HTTPS.
#'
#' @note Session ID should not be considered as authentication. If you are
#' handling sensitive data you should consider a more secure way of identifying
#' users across requests.
#'
#' @param name The name of the cookie to store the session id in
#' @param secure Should the session id only be send over HTTPS? Setting this to
#' TRUE will require setting up a proxy manager with SSL support in front of
#' your fiery server.
#'
#' @return A unary function taking a Request object and returning an ID for it
#'
#' @export
#'
session_id_cookie <- function(name = "fiery_id", secure = FALSE) {
  check_string(name)
  regex <- paste0(name, "=(.+?)(;|$)")
  function(request) {
    cookie <- request$origin$HTTP_COOKIE
    if (!is.null(cookie)) {
      id <- stringi::stri_match_first_regex(cookie, regex)[2]
      if (!is.na(id)) {
        return(id)
      }
    }
    id <- reqres::random_key()
    request$respond()$set_cookie(
      name,
      id,
      http_only = TRUE,
      secure = secure,
      same_site = "Strict"
    )
    request$origin$HTTP_COOKIE <- paste0(
      c(request$origin$HTTP_COOKIE, paste0(name, "=", id)),
      collapse = "; "
    )
    id
  }
}

#' A cheaper version of [rlang::trace_back()] that only provides the info needed
#' for fiery
#' @noRd
cheap_trace_back <- function() {
  frames <- sys.frames()
  idx <- seq_len(sys.parent(2L))
  frames <- frames[idx]
  parents <- sys.parents()[idx]
  calls <- as.list(sys.calls()[idx])
  recursive <- parents == seq_along(parents)
  parents[recursive] <- 0L
  list(call = calls, parent = parents)
}
