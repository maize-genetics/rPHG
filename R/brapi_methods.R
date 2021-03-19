# === BrAPI general methods =========================================

## Display message ----
setMethod(
    f = "show",
    signature = "BrapiCon",
    definition = function(object) {

        status <- httr::GET(brapiURL(object))$status

        cat("A BrAPI connection object\n")
        cat("  Server...........:", host(object), "\n")
        cat("  Port.............:", port(object), "\n")
        cat("  Server status....:", status, "\n")
        cat("  BrAPI version....:", version(object), "\n")
    }
)


## Get samples ----
#' @title Retrieve samples from BrAPI connection
#'
#' @description Retrieves data from the \code{samples} endpoint of a BrAPI
#'   server.
#'
#' @param object A \code{BrapiCon} object.
#'
#' @rdname samples
#'
#' @seealso [calls()] for samples, [callsets()] for callsets
#'
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
#' @param object A \code{BrapiCon} object.
#'
#' @rdname calls
#'
#' @seealso [samples()] for samples, [callsets()] for callsets
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
#' @seealso [samples()], [calls()]
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
#' @rdname callsets
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


