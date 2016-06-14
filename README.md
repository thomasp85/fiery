# Fiery

Fiery is an R package for creating R driven backends for ambituous web apps. It 
has been created out of a joy in using Shiny to create R based web apps, but a
need to couple an R server up to a more powerful framework for creating the 
client side logic and UI. This also means that Fiery is strickly concerned with
the backend side of the web app. Users will need to get aquianted with HTML, CSS
and JavaScript, but I firmly believe that being introduced to these technologies
have never harmed anyone. Fiery is not a competitor to Shiny in any way. For 
probably 99% of all use cases, Shiny will be the better and easier choice. For
the last 1% though, where the complexity of the app makes Shiny's UI model 
unwieldy, this might be just what is needed.

## The fiery server
The server model in fiery is based on 3 main entities. The router handles 
incoming requests, the model handles data logic and the actions sends respones
back to the client. Each run loop consists of message handling, model resolving
and firing of actions, after which queued up responses are send. Actions can be 
triggered both during message handling and model resolving, but will always be 
the last to run.

> Take note that Fiery does not try to by an R equivalent of Django or Ruby on 
Rails or other massive back-end frameworks. It is not designed to be a full
back-end MVC and should not be used as such.

### The router
The router takes in requests from the client and direct them towards the correct
handler. The router containes a list of routes, each of which can also contain
additional routes. The nesting corresponds to a resource URL so a request for 
the resource *'blog/posts/comment'* will be directed to the router at 
blog$posts$comment. Each router have methods corresponding to the different 
message types, such as GET, POST, PUT etc. If a handler is not specified the
router will automatically send a 405 error back. If a router is not existing for
a requested ressource the router will automatically return a 404 error. Apart
from this it is the responsibility of the different handlers to flag errors if 
they occur. Static ressources (pictures etc.) can be handled by the StaticRouter
subclass. StaticRouters can have standard routers attached and will only look 
for static content if there is no subrouter corresponding to the resource. The
root router (corresponding to the '/' ressource) is created automatically as a 
StaticRouter looking for content on the specified server root.

### The model
Fiery was initially concieved out of a wish to couple Ember based frontends to
an R driven backend. Because of this, Fiery's object model is heavily inspired by
the API of Ember.Object. The server logic is build up by adding properties and
computed properties that can be bound to other properties as well as triggering
actions. Overall this model vaguely resembling the reactive model in Shiny, but
is way more declarative. Computed properties are only dependant on other values
if it has been explicitly stated (reverse of Shiny, where you need to explicitly
state if a value is not bound using `isolate()`). Furthermore computed 
properties are eagerly evaluate unless they are explicetly stated as lazy. In
Shiny reactives are only evaluated if they are needed and it is necessary to use
an observer. Computed properties can be set to recalculate at specific interval,
much like `invalidateLater()` works within a reactive environment in Shiny.

# Single- and multisession support
Fiery servers can run in both a single and multisession mode. In multisession
mode each client gets their own model object that defines their current state. 
This works much like in Shiny when you start an app - the current R process is
blocked and each session is walled from each others. On the contrary, when the
server runs in singlesession mode, each session acces the same model. 
Furthermore the R session is not blocked and the user is free to manipulate the
model from R as well. Upon stopping a singlesession server, the current model is
returned and can be fed into a new session to restore the state. Singlesession
mode is generally only useful/advicable for local running apps, rather than apps
accessible over the world wide web.
