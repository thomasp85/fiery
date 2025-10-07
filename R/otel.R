otel_tracer_name <- "r.package.fiery"

get_tracer <- local({
  tracer <- NULL
  function() {
    if (!is.null(tracer)) {
      return(tracer)
    }
    if (testthat__is_testing()) {
      # Don't cache the tracer in unit tests. It interferes with tracer provider
      # injection in otelsdk::with_otel_record().
      return(otel::get_tracer())
    }
    tracer <<- otel::get_tracer()
    tracer
  }
})

testthat__is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

add_otel_exception_event <- function(e) {
  tracer <- get_tracer()
  if (tracer$is_enabled()) {
    span <- tracer$get_active_span()
    span$record_exception(e)
  }
  e
}
