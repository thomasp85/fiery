
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/fiery.png"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/thomasp85/fiery/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thomasp85/fiery/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/fiery)](https://CRAN.R-project.org/package=fiery)
[![Codecov test
coverage](https://codecov.io/gh/thomasp85/fiery/graph/badge.svg)](https://app.codecov.io/gh/thomasp85/fiery)
<!-- badges: end -->

Fiery is a flexible and lightweight framework for building web servers
in R. It is relatively unopinionated about how you chose to build your
server logic and supports many use cases, from serving static files to
being used as a base for a model-view-controller based setup.

## How to install this

Install the release from CRAN using `install.packages('fiery')` or get
the development version directly from GitHub using `pak`:

``` r
# install.packages('pak')
pak::pak('thomasp85/fiery')
```

## Design

Fiery is designed around a clear server life-cycle with events being
triggered at specific points during the life-cycle that will call the
handlers attached to these events. In addition to the life-cycle events,
it is possible to trigger custom events and attach handlers to these as
well. Fiery is designed with modularity in mind so that plugins can be
developed for different tasks and mixed and matched to suit the specific
project.

While the intro might indicate that fiery is difficult to use, this is
not the case. Much of the hard work of handling http requests has been
encapsulated in the [`reqres`](https://github.com/thomasp85/reqres) that
fiery uses to handle http requests and responses. Further, A plugin that
will often be used is [`routr`](https://github.com/thomasp85/routr),
which provides powerful routing of HTTP requests, thus simplifying the
server logic even more.

## A minimal example

Following is a very *Hello World*-ish example of a fiery app (sans
`routr`), that showcases some of the different life-cycle events:

``` r
library(fiery)

# Create a New App
app <- Fire$new()

# Setup the data every time it starts
app$on('start', function(server, ...) {
    server$set_data('visits', 0)
    server$set_data('cycles', 0)
})

# Count the number of cycles (internal loops)
app$on('cycle-start', function(server, ...) {
    server$set_data('cycles', server$get_data('cycles') + 1)
})

# Count the number of requests
app$on('before-request', function(server, ...) {
    server$set_data('visits', server$get_data('visits') + 1)
})

# Handle requests
app$on('request', function(server, request, ...) {
    response <- request$respond()
    response$status <- 200L
    response$body <- paste0('<h1>This is indeed a test. You are number ', server$get_data('visits'), '</h1>')
    response$type <- 'html'
})

# Show number of requests in the console
app$on('after-request', function(server, ...) {
    message(server$get_data('visits'))
    flush.console()
})

# Terminate the server after 50 cycles
app$on('cycle-end', function(server, ...) {
    if (server$get_data('cycles') > 50) {
        message('Ending...')
        flush.console()
        server$extinguish()
    }
})

# Be polite
app$on('end', function(server) {
    message('Goodbye')
    flush.console()
})

app$ignite(showcase = TRUE)
#> Fire started at <127.0.0.1:8080> (narrow_factional_pygmy)
#> Goodbye
```

In general much of the logic will happen in the `request` and `message`
handlers and you are free to ignore the other life-cycle events if they
are not needed.

## Code of Conduct

Please note that the ‘fiery’ project is released with a [Contributor
Code of Conduct](https://fiery.data-imaginist.com/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
