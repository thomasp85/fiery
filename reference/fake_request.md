# Create a fake request to use in testing

This function creates a new request for a specific resource defined by a
URL. It mimics the format of the requests provided through httpuv,
meaning that it can be used in place for the requests send to the
`before-request`, `request`, and `after-request` handlers. This is only
provided so that handlers can be tested without having to start up a
server.

## Usage

``` r
fake_request(
  url,
  method = "get",
  app_location = "",
  content = "",
  headers = list(),
  ...,
  remote_address = "123.123.123.123"
)
```

## Arguments

- url:

  A complete url for the resource the request should ask for

- method:

  The request type (get, post, put, etc). Defaults to `"get"`

- app_location:

  A string giving the first part of the url path that should be stripped
  from the path

- content:

  The content of the request, either a raw vector or a string

- headers:

  A list of name-value pairs that defines the request headers

- ...:

  Additional name-value pairs that should be added to the request

- remote_address:

  The IP address of the presumed sender

## Value

A Rook-compliant environment

## Examples

``` r
req <- fake_request(
    'http://www.my-fake-website.com/path/to/a/query/?key=value&key2=value2',
    content = 'Some important content'
)

# Get the main address of the URL
req[['SERVER_NAME']]
#> [1] "www.my-fake-website.com"

# Get the query string
req[['QUERY_STRING']]
#> [1] "key=value&key2=value2"

# ... etc.

# Cleaning up connections
rm(req)
gc()
#>           used (Mb) gc trigger  (Mb) max used  (Mb)
#> Ncells 1168629 62.5    2160882 115.5  2160882 115.5
#> Vcells 2167893 16.6    8388608  64.0  8388547  64.0
```
