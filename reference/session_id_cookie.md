# Use a session cookie to store the id of the session

This function constructs an ID extractor for incoming requests, the
return value of which will be passed to the id argument of request,
header, message etc handlers. By default, fiery uses this with default
arguments.

## Usage

``` r
session_id_cookie(name = "fiery_id", secure = FALSE)
```

## Arguments

- name:

  The name of the cookie to store the session id in

- secure:

  Should the session id only be send over HTTPS? Setting this to TRUE
  will require setting up a proxy manager with SSL support in front of
  your fiery server.

## Value

A unary function taking a Request object and returning an ID for it

## Details

A session id is looked for in the cookie matching the `name` arg. If it
is found then the value of the cookie is returned. If it is not found
then an id is generated with
[`reqres::random_key()`](https://reqres.data-imaginist.com/reference/random_key.html)
and attached to the response as a cookie with the given name. The cookie
is set to HTTP only and Strict same site policy. Depending on `secure`
it also sets it to only be transferred over HTTPS.

## Note

Session ID should not be considered as authentication. If you are
handling sensitive data you should consider a more secure way of
identifying users across requests.
