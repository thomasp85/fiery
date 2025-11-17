requests <- new.env(parent = emptyenv())
requests$counter <- 0
requests$stack <- rep_len(list(NULL), 500)

get_request <- function(...) {
  if (requests$counter < 1 || requests$counter > 500) {
    req <- reqres::Request$new(...)
  } else {
    req <- requests$stack[[requests$counter]]
    requests$counter <- requests$counter - 1L
    req$initialize(...)
  }
  req
}
put_request <- function(req) {
  req$clear()
  if (requests$counter < 500 && !req$locked) {
    requests$counter <- requests$counter + 1L
    requests$stack[[requests$counter]] <- req
  }
}
