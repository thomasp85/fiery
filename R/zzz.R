MAY_TRACEBACK <- NULL

.onLoad <- function(...) {
  MAY_TRACEBACK <<- packageVersion("base") >= "4.2.0"
}
