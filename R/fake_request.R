#' @include aaa.R
NULL

#' A Class mimicking InputStream from httpuv
#' 
#' @importFrom R6 R6Class
#' 
#' @noRd
#' 
InputStreamFake <- R6Class('InputStreamFake',
  public = list(
    initialize = function(content) {
      if (!is.raw(content)) {
        check_character(content)
        content <- paste(content, collapse = '\n')
        content <- paste0(content, '\n')
        content <- charToRaw(content)
      }
      private$content <- rawConnection(content)
      private$length <- length(content)
      seek(private$content, 0)
    },
    finalize = function() {
      try(close(private$content), silent = TRUE)
    },
    read_lines = function(n = -1L) {
      readLines(private$content, n, warn = FALSE)
    },
    read = function(l = -1L) {
      # l < 0 means read all remaining bytes
      if (l < 0)
        l <- private$length - seek(private$content)
      
      if (l == 0)
        return(raw())
      else
        return(readBin(private$content, raw(), l))
    },
    rewind = function() {
      seek(private$content, 0)
    },
    close = function() {
      try(close(private$content), silent = TRUE)
    }
  ),
  private = list(
    content = NULL,
    length = NULL
  )
)
#' A Class mimicking NullInputStream from httpuv
#' 
#' @importFrom R6 R6Class
#' 
#' @noRd
#' 
NullInputStreamFake <- R6Class('NullInputStreamFake',
  public = list(
    read_lines = function(n = -1L) {
      character()
    },
    read = function(l = -1L) {
      raw()
    },
    rewind = function() invisible(),
    close = function() invisible()
  )
)
#' A Class mimicking ErrorStream from httpuv
#' 
#' @importFrom R6 R6Class
#' 
#' @noRd
#' 
ErrorStreamFake <- R6Class('ErrorStreamFake',
  public = list(
    cat = function(... , sep = " ", fill = FALSE, labels = NULL) {
      base::cat(..., sep=sep, fill=fill, labels=labels, file=stderr())
    },
    flush = function() {
      base::flush(stderr())
    }
  )
)

#' Create a fake request to use in testing
#' 
#' This function creates a new request for a specific resource defined by a 
#' URL. It mimics the format of the requests provided through httpuv, meaning 
#' that it can be used in place for the requests send to the `before-request`, 
#' `request`, and `after-request` handlers. This is only provided so that 
#' handlers can be tested without having to start up a server.
#' 
#' @param url A complete url for the resource the request should ask for
#' 
#' @param method The request type (get, post, put, etc). Defaults to `"get"`
#' 
#' @param appLocation A string giving the first part of the url path that should
#' be stripped from the path
#' 
#' @param content The content of the request, either a raw vector or a string
#' 
#' @param headers A list of name-value pairs that defines the request headers
#' 
#' @param ... Additional name-value pairs that should be added to the request
#' 
#' @return A Rook-compliant environment
#' 
#' @importFrom utils packageVersion
#' 
#' @export
#' @keywords internal
#' 
#' @examples 
#' req <- fake_request(
#'     'http://www.my-fake-website.com/path/to/a/query/?key=value&key2=value2',
#'     content = 'Some important content'
#' )
#' 
#' # Get the main address of the URL
#' req[['SERVER_NAME']]
#' 
#' # Get the query string
#' req[['QUERY_STRING']]
#' 
#' # ... etc.
#' 
#' # Cleaning up connections
#' rm(req)
#' gc()
#' 
fake_request <- function(url, method = 'get', appLocation = '', content = '', headers = list(), ...) {
  rook <- new.env(parent = emptyenv())
  rook$REQUEST_METHOD <- toupper(method)
  
  # Split up URL
  url <- split_url(url)
  rook$rook.url_scheme <- url$scheme
  rook$SERVER_NAME <- url$domain
  rook$SERVER_PORT <- url$port
  if (appLocation != '') {
    appLocation <- sub('/$', '', appLocation)
    appLocReg <- paste0('^', appLocation)
    if (!grepl(appLocReg, url$path)) {
      cli::cli_abort('{.arg appLocation} must correspond to the beginning of the path')
    }
    rook$SCRIPT_NAME <- appLocation
    url$path <- sub(appLocReg, '', url$path)
  } else {
    rook$SCRIPT_NAME <- appLocation
  }
  rook$PATH_INFO <- url$path
  rook$QUERY_STRING <- url$query
  
  # Misc
  rook$httpuv.version <- packageVersion('httpuv')
  rook$rook.version <- "1.1-0"
  rook$REMOTE_PORT <- paste(sample(0:9, 5, TRUE), collapse = '')
  
  # Headers
  if (length(headers) > 0) {
    names(headers) <- paste0('HTTP_', sub('^HTTP_', '', toupper(names(headers))))
    for (i in names(headers)) {
      check_scalar(headers[[i]])
      assign(i, as.character(headers[[i]]), envir = rook)
    }
  }
  
  # Extra
  extra <- list(...)
  for (i in names(extra)) {
    assign(i, extra[[i]], envir = rook)
  }
  
  # Input
  if (length(content) == 1 && content == '') {
    rook$rook.input <- NullInputStreamFake$new()
  } else {
    rook$rook.input <- InputStreamFake$new(content)
  }
  rook$rook.errors <- ErrorStreamFake$new()
  
  rook
}
#' @importFrom stringi stri_match
split_url <- function(url) {
  matches <- stri_match(
    url,
    regex = '^(([^:/?#]+)://)?(([^/?#:]*)(:(\\d+))?)?([^?#]*)(\\?([^#]*))?(#(.*))?'
  )[1,]
  
  parsedURL <- list(
    scheme = if (is.na(matches[3])) 'http' else matches[3],
    domain = matches[5],
    port = matches[7],
    path = if (is.na(matches[8]) | matches[8] == '') '/' else matches[8],
    query = if (is.na(matches[10])) '' else matches[10],
    fragment = if (is.na(matches[12])) '' else matches[12]
  )
  
  if (is.na(parsedURL$port)) {
    parsedURL$port <- if (parsedURL$scheme == 'http') '80' else '443'
  }
  parsedURL
}
