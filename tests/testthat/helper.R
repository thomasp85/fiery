r_session <- function() {
  rs <- callr::r_session$new()
  rs$run(function() library(fiery))

  function(expr, invisible = TRUE) {
    expr <- enquote(expr)
    val <- rs$run(function(x) eval(expr = x), args = list(expr))
    if (invisible) {
      invisible(val)
    } else {
      val
    }
  }
}
