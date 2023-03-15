check_args <- function(x, arguments, ..., arg = caller_arg(x), call = caller_env()) {
  check_function(x, ..., arg = arg, call = call)
  if (!all(arguments %in% names(formals(x)))) {
    cli::cli_abort("{.arg {arg}} must be a function containing the {cli::qty(arguments)} argument{?s} {.arg {arguments}}", call = call)
  }
}

check_dir <- function(x, ..., allow_null = FALSE, arg = caller_arg(x), call = caller_env()) {
  check_string(x, ..., allow_null = allow_null, arg = arg, call = call)
  if (is.null(x) && allow_null) {
    return(NULL)
  }
  if (!dir.exists(x)) {
    cli::cli_abort("{.arg {arg}} must be a valid directory", call = call)
  }
}

check_scalar <- function(x, ..., allow_null = FALSE, arg = caller_arg(x), call = caller_env()) {
  if (is.null(x) && allow_null) {
    return(NULL)
  }
  if (length(x) != 1L) {
    cli::cli_abort("{.arg {arg}} must be a scalar object, not {obj_type_friendly(letters)}", call = call)
  }
}