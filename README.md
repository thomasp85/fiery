
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="man/figures/fiery.png"/>

<!-- badges: start -->

[![R-CMD-check](https://github.com/thomasp85/fiery/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/thomasp85/fiery/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-ago/fiery)](https://cran.r-project.org/package=fiery)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/fiery)](https://cran.r-project.org/package=fiery)
[![Codecov test
coverage](https://codecov.io/gh/thomasp85/fiery/branch/main/graph/badge.svg)](https://app.codecov.io/gh/thomasp85/fiery?branch=main)
<!-- badges: end -->

Fiery is a flexible and lightweight framework for building web servers
in R. It is relatively unopinionated about how you chose to build your
server logic and supports many use cases, from serving static files to
being used as a base for a model-view-controller based setup.

### The shiny elephant in the room

Before going any further I will briefly address what most people are
thinking of when they think R+web:
[Shiny](https://github.com/rstudio/shiny):

*Is this a competing framework to Shiny?*

In a way, yes. Any package that provides functionality for creating web
applications in R will be competing for the developers who wish to make
web apps. This is of course reinforced by the name of the package, which
is a gently jab at Shiny. But mostly no. I believe Shiny and Fiery will
appeal to vastly different use cases, in the same way as automakers and
motorbike makers are in theory competing for the customers who wish to
acquire the means to transport themselves, but would never be seen as
truly competing.

*So what is so different about Fiery?*

Without falling too much into the trap of defining a project by how it
differs from another, there are some very clear differences in approach
between Fiery and Shiny.

- Shiny uses magic to make everything work from R, Fiery lets you do all
  the hard work.
- Shiny wants the main app-logic to be server-side, Fiery don’t care
  what you do.
- Shiny uses a reactive model to define the app-logic, Fiery doesn’t
  care about what you do (see a pattern emerge).
- Shiny wants you to use
  [htmltools](https://github.com/rstudio/htmltools) to build the html,
  Fiery really doesn’t care about what you use.

From the above it is quite clear that Fiery to a higher degree gives you
the choice and responsibility of building up your app at the cost of
higher complexity, but with the goal of giving you more power over what
you can do.

*So how is this different from
[httpuv](https://github.com/rstudio/httpuv)?*

Now we’re getting somewhere! httpuv is sitting in the bottom of the
stack for both Shiny and Fiery, but where Shiny builds an elaborate,
opinionated and complete framework on top of httpuv, Fiery “merely” adds
a lot of convenience to running a httpuv based web server. You could say
that Fiery *sits between* httpuv and Shiny, and that Shiny (or an
alternative framework) could in theory be built on top of Fiery.

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
#> Fire started at 127.0.0.1:8080
#> message: 1
#>  from message(server$get_data("visits"))
#> message: Goodbye
#>  from message("Goodbye")
```

In general much of the logic will happen in the `request` and `message`
handlers and you are free to ignore the other life-cycle events if they
are not needed.

## Feedback

I would love some feedback on this - open an issue or reach out to me on
[twitter](https://twitter.com/thomasp85).

## Code of Conduct

Please note that the ‘fiery’ project is released with a [Contributor
Code of Conduct](https://fiery.data-imaginist.com/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
