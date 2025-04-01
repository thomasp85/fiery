requests <- new.env(parent = emptyenv())
requests$counter <- 0
requests$stack <- rep_len(list(NULL), 100)

get_request <- function(...) {
  if (requests$counter < 1 || requests$counter > 100) {
    req <- reqres::Request$new(...)
  } else {
    req <- requests$stack[[requests$counter]]
    requests$counter <- requests$counter - 1L
    req$initialize(...)
  }
  req
}
put_request <- function(req) {
  if (requests$counter < 100 && !req$locked) {
    req$clear()
    requests$counter <- requests$counter + 1L
    requests$stack[[requests$counter]] <- req
  }
}
