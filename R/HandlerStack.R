#' @importFrom R6 R6Class
#' @importFrom assertthat is.string is.count assert_that
HandlerStack <- R6Class('HandlerStack',
    public = list(
        # Methods
        initialize = function() {
            private$handleEnv <- new.env(parent = emptyenv())
        },
        add = function(handler, id, pos = NULL) {
            assert_that(
                is.string(id),
                is.function(handler)
            )
            if (is.null(pos)) {
                pos <- length(private$handleOrder)
                assert_that(is.count(pos))
            }
            assign(id, handler, envir = private$handleEnv)
            private$handleOrder <- append(private$handleOrder, id, after = pos)
        },
        remove = function(id) {
            assert_that(is.string(id))
            ind <- which(private$handleOrder == id)
            if (length(ind) != 0) {
                private$handleOrder <- private$handleOrder[-ind]
                rm(list = id, envir = private$handleEnv)
            }
        },
        dispatch = function(...) {
            res <- lapply(private$handleOrder, function(id) {
                private$handleEnv[[id]](...)
            })
            names(res) <- private$handleOrder
            res
        },
        length = function() {
            length(private$handleOrder)
        },
        contains = function(id) {
            assert_that(is.character(id))
            !is.na(self$position(id))
        },
        position = function(id) {
            assert_that(is.character(id))
            match(id, private$handleOrder)
        }
    ),
    private = list(
        # Data
        handleEnv = NULL,
        handleOrder = character()
    )
)