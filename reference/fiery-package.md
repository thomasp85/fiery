# fiery: A Lightweight and Flexible Web Framework

A very flexible framework for building server side logic in R. The
framework is unopinionated when it comes to how HTTP requests and
WebSocket messages are handled and supports all levels of app
complexity; from serving static content to full-blown dynamic web-apps.
Fiery does not hold your hand as much as e.g. the shiny package does,
but instead sets you free to create your web app the way you want.

## Details

`fiery` is a lightweight and flexible framework for web servers build on
top of the [httpuv](https://rdrr.io/pkg/httpuv/man/httpuv-package.html)
package. The framework is largely event-based, letting the developer
attach handlers to life-cycle events as well as defining and triggering
their own events. This approach to development is common in JavaScript,
but might feel foreign to R developers. Thankfully it is a rather simple
concept that should be easy to gradually begin to use to greater and
greater effect.

## See also

Useful links:

- <https://fiery.data-imaginist.com>

- <https://github.com/thomasp85/fiery>

- Report bugs at <https://github.com/thomasp85/fiery/issues>

## Author

**Maintainer**: Thomas Lin Pedersen <thomasp85@gmail.com>
([ORCID](https://orcid.org/0000-0002-5147-4711))
