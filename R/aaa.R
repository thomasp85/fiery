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