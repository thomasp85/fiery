# Select a random safe port

This is a small utility function to get random safe ports to run your
application on. It chooses a port within the range that cannot be
registeret to IANA and thus is safe to assume are not in use.

## Usage

``` r
random_port()
```

## Value

An integer in the range 49152-65535

## Examples

``` r
random_port()
#> [1] 61976
```
