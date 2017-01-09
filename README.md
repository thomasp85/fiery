
<!-- README.md is generated from README.Rmd. Please edit that file -->
fiery
=====

[![Travis-CI Build Status](https://travis-ci.org/thomasp85/fiery.svg?branch=master)](https://travis-ci.org/thomasp85/fiery) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/thomasp85/fiery?branch=master&svg=true)](https://ci.appveyor.com/project/thomasp85/fiery) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/fiery)](https://cran.r-project.org/package=fiery) [![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/fiery)](https://cran.r-project.org/package=fiery) [![Coverage Status](https://img.shields.io/codecov/c/github/thomasp85/fiery/master.svg)](https://codecov.io/github/thomasp85/fiery?branch=master)

Fiery is a flexible and lighweight framework for building web servers in R. It is relatively unoppinionated about how you chose to build your server logic and supports any use case, from serving static files to being used as a base for a model-view-controller based setup.

### The shiny elephant in the room

Before going any further I will briefly address what most people are thinking of when they think R+web: [Shiny](https://github.com/rstudio/shiny):

*Is this a competing framework to Shiny?*

In a way, yes. Any package that provides functionality for creating web applications in R will be competing for the developers who wish to make web apps. This is of course reinforced by the name of the package, which is a gently jab at Shiny. But mostly no. I believe Shiny and Fiery will appeal to vastly different use cases, in the same way as automakers and motorbike makers are in theory competing for the customers who wish to acquire the means to transport themselves, but would never be seen as truly competing.

*So what is so different about Fiery?*

Without falling too much into the trap of defining a project by how it differs from another, there are some very clear differences in approach between Fiery and Shiny.

-   Shiny uses magic to make everything work from R, Fiery lets you do all the hard work.
-   Shiny wants the main app-logic to be server-side, Fiery don't care what you do.
-   Shiny uses a reactive model to define the app-logic, Fiery don't care what you do (see a pattern emerge).
-   Shiny wants you to use [htmltools](https://github.com/rstudio/htmltools) to build the html, Fiery really don't care what you use.

From the above it is quite clear that Fiery to a higher degree gives you the choice and responsibility of building up your app at the cost of higher complexity, but with the goal of giving you more power over what you can do.

*So how is this different from [httpuv](https://github.com/rstudio/httpuv)?*

Now we're getting somewhere! httpuv is sitting in the bottom of the stack for both Shiny and Fiery, but where Shiny build an elaborate, oppinionated and complete framework on top of httpuv, Fiery "merely" adds a lot of convenience to running a httpuv based web server. You could say that Fiery "sits between" httpuv and Shiny, and that Shiny (or an alternative framework) could in theory be build on top of Fiery.

How to install this
-------------------

Currently fiery exists on GitHub only and should be installed using devtools:

``` r
if (!require(devtools)) {
    install.packages('devtools')
}
devtools::install_github('thomasp85/fiery')
```

A CRAN submission is intended eventually though...

Design
------

Fiery is designed around a clear server life-cycle with events being triggered at specific points during the life-cycle that will call the handlers attached to these events. In addition to the life-cycle events, it is possible to trigger custom events and attach handlers to these as well. Fiery is designed with modularity in mind so that plugins can be developed for different tasks and mixed and matched to suit the specific project. The first plugin is [routr](https://github.com/thomasp85/routr), which I'm developing in parallel and which provides powerful routing of HTTP requests and WebSocket messages.

A minimal example
-----------------

Following is a very *Hello World*-ish example of a fiery app, that showcases some of the different life-cycle events:

``` r
library(fiery)

# Create a New App
app <- Fire$new()

# Setup the data everytime it starts
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
app$on('request', function(server, ...) {
    list(
        status = 200L,
        headers = list('Content-Type' = 'text/html'),
        body = paste('This is indeed a test. You are number', server$get_data('visits'))
    )
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
#> 1
#> Ending...
#> Goodbye
```

In general much of the logic will happen in the `request` and `message` handlers and you are free to ignore the other life-cycle events if they are not needed.

Feedback
--------

I would love some feedback on this - open an issue or reach out to me on [twitter](https://twitter.com/thomasp85). This is still a work-in-progress so if you want to influence the overall design, this is the moment to shout out.

Roadmap
-------

Fiery is intended to be minimal and lightweight so many features will be delegated to plugins. On top of my list for fiery features are infrastructure that allows for delayed, timed and asynchronous code execution, as well as including some small plugins for standard functionality
