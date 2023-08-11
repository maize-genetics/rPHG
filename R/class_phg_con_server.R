## ----
#' @title An S4 BrapiCon Class
#'
#' @description Class \code{BrapiCon} defines a \code{rPHG}
#'    Class for storing BrAPI connection data.
#'
#' @slot host A URL to a BrAPI server.
#' @slot port The host port.
#' @slot protocol Which protocol must be used to fetch the desired data? Must
#'    be either \code{http} or \code{https}.
#' @slot version BrAPI version number. Must be either \code{"v1"} or
#'    \code{"v2"}.
#' @slot token API authorization token.
#' @slot url BrAPI server URL.
#'
#' @name BrapiCon-class
#' @rdname BrapiCon-class
#' @exportClass BrapiCon
setClass(
    Class = "BrapiCon",
    representation = representation(
        host = "character",
        port = "numeric",
        protocol = "character",
        version = "character",
        token = "character",
        url = "character"
    ),
    prototype = prototype(
        host = NA_character_,
        port = NA_integer_,
        protocol = NA_character_,
        version = NA_character_,
        token = NA_character_,
        url = NA_character_
    )
)


## ----
#' @title BrAPI connection validation
#'
#' @name BrapiCon-validity
#'
#' @description Checks if \code{BrapiCon} class objects are valid.
#'
#' @param object A \code{BrapiCon} object.
#'
#' @importFrom curl has_internet
setValidity("BrapiCon", function(object) {
    errors <- character()

    port <- object@port
    protocol <- object@protocol
    version <- object@version

    if (!curl::has_internet()) {
        msg <- "An internet connection could not be made."
        errors <- c(errors, msg)
    }

    if (!(port %in% 1:65535)) {
        msg <- "Not a valid port number."
        errors <- c(errors, msg)
    }

    if (!(protocol %in% c("http", "https"))) {
        msg <- "Protocols can only be 'http' or 'https'."
        errors <- c(errors, msg)
    }

    if (!(version %in% c("v1", "v2"))) {
        msg <- "Versions 1 or 2 are only allowed."
        errors <- c(errors, msg)
    }

    if (length(errors) == 0) TRUE else errors
})


## ----
#' @title BrapiCon object and constructors
#'
#' @description \code{BrapiCon} is the primary container for housing BrAPI
#'    connection information.
#'
#' @param host A URL to a BrAPI server.
#' @param port The host port. If \code{NULL}, a default port (e.g. \code{80} or
#'    \code{443}) will be used depending on protocol.
#' @param protocol Which protocol must be used to fetch the desired data? Must
#'    be either \code{http} or \code{https}. Defaults to \code{http}.
#' @param version BrAPI version number. Must be either \code{"v1"} or
#'    \code{"v2"}. Defaults to \code{v2}.
#'
#' @return A \code{BrapiCon} object.
#'
#' @export
BrapiCon <- function(
    host,
    port = NULL,
    protocol = c("http", "https"),
    version = c("v2", "v1")
) {

    if (missing(host)) stop("A URL host is needed to make this class.")

    version <- match.arg(version)
    protocol <- match.arg(protocol)

    if (is.null(port) && protocol == "http") port <- 80
    if (is.null(port) && protocol == "https") port <- 443

    if (port %% 1 != 0) stop("Invalid port number. Must be a whole number.")

    url <- sprintf("%s://%s:%d/brapi/%s", protocol, host, port, version)

    new(
        Class = "BrapiCon",
        host = host,
        port = port,
        protocol = protocol,
        version = version,
        url = url
    )
}


## ----
#' @rdname brapiURL
#' @export
setMethod(
    f = "brapiURL",
    signature = signature(object = "BrapiCon"),
    definition = function(object) {
        return(object@url)
    }
)


## ----
#' @rdname host
#' @export
setMethod(
    f = "host",
    signature = signature(object = "BrapiCon"),
    definition = function(object) {
        return(object@host)
    }
)


## ----
#' @rdname serverInfo
#' @export
setMethod(
    f = "serverInfo",
    signature = signature(object = "BrapiCon"),
    definition = function(object) {
        json2tibble(object, "serverinfo", "calls")
    }
)


## ----
#' @rdname showPHGMethods
#' @export
setMethod(
    f = "showPHGMethods",
    signature = signature(object = "BrapiCon"),
    definition = function(object) {
        ## Temp fix to return proper methods
        fullTable <- json2tibble(object, "variantTables")
        filtTable <- fullTable[fullTable$numSamples > 100, ] # arbitrary n
        return(filtTable)
    }
)


