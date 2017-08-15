#' A Fiery Framework for R-Based Web Development
#' 
#' `fiery` is a lightweight and flexible framework for web servers build on top of
#' the [httpuv][httpuv::httpuv-package] package. The framework is largely 
#' event-based, letting the developer attach handlers to life-cycle events as
#' well as defining and triggering their own events. This approach to
#' development is common in JavaScript, but might feel foreign to R developers.
#' Thankfully it is a rather simple concept that should be easy to gradually
#' begin to use to greater and greater effect.
#' 
#' @section Read more:
#' - Creation of the server object, along with all its methods and fields, is 
#' described in the documentation of the [`Fire`] class.
#' - An overview of the event model, along with descriptions of the predefined
#' life-cycle events and custom events can be found in the [events]
#' documentation.
#' - A description of the `fiery` plugin interface and how to develop your
#' own plugins is laid out in the [plugins] documentation
#' 
#' @author Thomas Lin Pedersen <thomasp85@gmail.com>
#' 
'_PACKAGE'