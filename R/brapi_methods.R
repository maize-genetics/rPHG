# === BrAPI general methods =========================================

## Display message ----
#' @title Show method for BrapiCon objects
#'
#' @description Prints out the information from the BrAPI connection object
#'   including server status codes. See this
#'   \href{https://en.wikipedia.org/wiki/List_of_HTTP_status_codes}{Wikipedia link}
#'   for further details about what these codes mean.
#'
#' @param object a \code{\linkS4class{BrapiCon}} object.
#'
#' @docType methods
#' @name show
#' @rdname show
#' @aliases show show,BrapiCon-method
#'
#' @export
setMethod(
    f = "show",
    signature = "BrapiCon",
    definition = function(object) {

        status <- tryCatch(
            expr = {
                httr::GET(brapiURL(object))$status
            },
            error = function(cond) "ERROR"
        )

        cat("A BrAPI connection object\n")
        cat("  Server...........:", host(object), "\n")
        cat("  Port.............:", port(object), "\n")
        cat("  Server status....:", status, "\n")
        cat("  BrAPI version....:", version(object), "\n")
    }
)


## Get taxa ----
#' @title Retrieve samples from BrAPI connection
#'
#' @description Retrieves data from the \code{samples} endpoint of a BrAPI
#'   server.
#'
#' @param object a \code{\linkS4class{BrapiCon}} object.
#'
#' @rdname samples
#'
#' @export
setGeneric("samples", function(object) standardGeneric("samples"))

#' @rdname samples
#' @export
setMethod(
    f = "samples",
    signature = "BrapiCon",
    definition = function(object) {
        json2tibble(object, "samples")
    }
)


## Get calls ----
#' @title Retrieve calls from BrAPI connection
#'
#' @description Retrieves data from the \code{calls} endpoint of a BrAPI
#'   server.
#'
#' @param object a \linkS4class{BrapiCon} object.
#'
#' @rdname calls
#'
#' @export
setGeneric("calls", function(object) standardGeneric("calls"))

#' @rdname calls
#' @export
setMethod(
    f = "calls",
    signature = "BrapiCon",
    definition = function(object) {
        json2tibble(object, "calls")
    }
)


## Get callsets ----
#' @title Retrieve callsets from BrAPI connection
#'
#' @description Retrieves data from the \code{calls} endpoint of a BrAPI
#'   server.
#'
#' @param object A \code{BrapiCon} object.
#'
#' @rdname callsets
#'
#' @export
setGeneric("callsets", function(object) standardGeneric("callsets"))

#' @rdname callsets
#' @export
setMethod(
    f = "callsets",
    signature = "BrapiCon",
    definition = function(object) {
        json2tibble(object, "callsets")
    }
)


## Get graphs ----
#' @title Retrieve graph data from BrAPI connection
#'
#' @description Retrieves data from the \code{graphs} endpoint of a BrAPI
#'   server.
#'
#' @param object A \code{BrapiCon} object.
#' @param dbID A PHG method.
#'
#' @rdname phGraph
#'
#' @export
setGeneric("phGraph", function(object, dbID) standardGeneric("phGraph"))

#' @rdname phGraph
#' @export
setMethod(
    f = "phGraph",
    signature = "BrapiCon",
    definition = function(object, dbID) {
        json2igraph(object, dbID)
    }
)


