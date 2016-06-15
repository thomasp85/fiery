
<!-- README.md is generated from README.Rmd. Please edit that file -->
fiery
=====

[![Travis-CI Build Status](https://travis-ci.org/thomasp85/fiery.svg?branch=master)](https://travis-ci.org/thomasp85/fiery) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/thomasp85/fiery?branch=master&svg=true)](https://ci.appveyor.com/project/thomasp85/fiery) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/fiery)](http://cran.r-project.org/package=fiery) [![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/grand-total/fiery)](http://cran.r-project.org/package=fiery) [![Coverage Status](https://img.shields.io/codecov/c/github/thomasp85/fiery/master.svg)](https://codecov.io/github/thomasp85/fiery?branch=master)

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

The Fiery way
-------------

Fiery is an event-based framework where the server has a lifecycle, with events being triggered at specific points during the cycle. Handlers can be attached to events and will get called once the event is triggered. New events can be defined and be triggered by other handlers or manually.

Enough talk - lets see how it works:

``` r
library(fiery)

# Create a new app
app <- Fire$new()

# Add a handler to the 'request' event
app$on('request', function(server, ...) {
    list(
        status = 200L,
        headers = list('Content-Type' = 'text/html'),
        content = 'Fight Fire With Fire!'
    )
})
#> [1] "6d1b0d33-90bb-46c8-b041-e7d4dc71d72e"
```

The code above tells the app to return the text `Fight Fire With Fire!` everytime an HTTP request is recieved. You can see that the `on` method returns a string. This string uniquely identifies the added handler and can be used to remove it again using the `off` method.

Let's do something with websockets too:

``` r
app$on('message', function(server, ...) {
    server$trigger('my-very-own-event', ...)
})
#> [1] "c3fe7b19-1888-4381-913a-36040046fabc"
```

What is happening? We've just triggered a custom event is what happened. The effect of this is quite undramatic as no handlers has been attached to this event yet but this is about to change:

``` r
app$on('my-very-own-event', function(...) {
    message('You\'ve got a message')
    flush.console()
})
#> [1] "c859a7c4-98f8-4f11-93cc-067a060b4df2"
```

When we run this we will get a message in the console everytime someone sends a WebSocket message.

More to come...
