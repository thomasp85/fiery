# Silence 
globalVariables(
  c(
    'private',
    'self',
    'super'
  )
)

#' Convert a request to an ID
#' 
#' @param request A Request object
#' 
#' @return A string uniquely linking the request to the client
#' 
#' @noRd
#' 
client_to_id <- function(request) {
  paste0('ID_', request$ip)
}

#' Predefined responses
#' 
#' This is a convenience for not having to type out a typical response every time 
#' they are needed
#' 
#' @noRd
#' 
notFound <- list(
  status = 404L,
  headers = list('Content-Type' = 'text/plain'),
  body = ''
)
serverError <- list(
  status = 500L,
  headers = list('Content-Type' = 'text/plain'),
  body = ''
)

# better try
tri <- function(expr) {
  tryCatch(expr, error = function(e) e)
}
is.condition <- function(x) inherits(x, 'condition')
is.error_cond <- function(x) is.condition(x) && inherits(x, 'error')

`%||%` <- function(l, r) if (is.null(l)) r else l