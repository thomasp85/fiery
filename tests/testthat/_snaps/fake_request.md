# URL parsing works

    Code
      fake_request("http://www.example.com/a/mount/", appLocation = "/wrong/mount/")
    Condition
      Error in `fake_request()`:
      ! `appLocation` must correspond to the beginning of the path

# Headers are assigned

    Code
      fake_request("http://www.example.com", headers = list(accept_encoding = letters[
        1:4]))
    Condition
      Error in `fake_request()`:
      ! `headers[[i]]` must be a scalar object, not a character vector

