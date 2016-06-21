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
#' @param request A rook compliant request object
#' 
#' @return A string uniquely linking the request to the client
#' 
#' @noRd
#' 
client_to_id <- function(request) {
    paste0('ID_', request$REMOTE_ADDR, '_', request$REMOTE_PORT)
}