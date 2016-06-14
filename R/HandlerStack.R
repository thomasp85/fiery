#' @importFrom R6 R6Class
HandlerStack <- R6Class('HandlerStack',
    public = list(
        # Methods
        initialize = function() {
            private$handleOrder <- new.env(parent = emptyenv())
        },
        add = function(handler, id, pos = NULL) {
            if (is.null(pos)) {
                pos <- length(private$handleOrder)
            }
            assign(id, handler, envir = private$handleEnv)
            private$handleOrder <- append(private$handleOrder, id, after = pos)
        },
        remove = function(id) {
            ind <- which(private$handleOrder == id)
            if (length(ind) != 0) {
                private$handleOrder <- private$handleOrder[-ind]
                rm(id, envir = private$handleEnv)
            }
        },
        dispatch = function(...) {
            lapply(private$handleOrder, function(id) {
                private$handleEnv[[id]](...)
            })
        }
    ),
    private = list(
        # Data
        handleEnv = NULL,
        handleOrder = NULL
    )
)