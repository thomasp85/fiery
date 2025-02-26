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

cheap_trace_back <- function() {
  frames <- sys.frames()
  idx <- seq_len(sys.parent(2L))
  frames <- frames[idx]
  parents <- sys.parents()[idx]
  calls <- as.list(sys.calls()[idx])
  recursive <- parents == seq_along(parents)
  parents[recursive] <- 0L
  list(calls, parents)
}
