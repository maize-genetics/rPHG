## ----
#' @title An PHGServerCon Class
#'
#' @description Class \code{PHGServerCon} defines a \code{rPHG}
#'    Class for storing BrAPI connection data.
#'
#' @slot port The host port.
#' @slot protocol Which protocol must be used to fetch the desired data? Must
#'    be either \code{http} or \code{https}.
#' @slot version BrAPI version number. Must be either \code{"v1"} or
#'    \code{"v2"}.
#' @slot token API authorization token.
#' @slot url BrAPI server URL.
#'
#' @name PHGServerCon-class
#' @rdname PHGServerCon-class
#' @exportClass PHGServerCon
setClass(
    Class    = "PHGServerCon",
    contains = "PHGCon",
    representation = representation(
        port     = "numeric",
        protocol = "character",
        version  = "character",
        token    = "character",
        url      = "character"
    ),
    prototype = prototype(
        port     = NA_integer_,
        protocol = NA_character_,
        version  = NA_character_,
        token    = NA_character_,
        url      = NA_character_
    )
)


## ----
#' @title BrAPI connection validation
#'
#' @name PHGServerCon-validity
#'
#' @description Checks if \code{PHGServerCon} class objects are valid.
#'
#' @param object A \code{PHGServerCon} object.
#'
#' @importFrom curl has_internet
setValidity("PHGServerCon", function(object) {
    errors <- character()

    port <- object@port
    protocol <- object@protocol
    version <- object@version

    if (!curl::has_internet()) {
        msg <- "An internet connection could not be made."
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
#' @title PHGServerCon object constructor
#'
#' @description \code{PHGServerCon} is the primary container for housing BrAPI
#'    connection information.
#'
#' @param host A user defined URL
#' @param port The host port. If \code{NULL}, a default port (e.g. \code{80} or
#'    \code{443}) will be used depending on protocol.
#' @param protocol Which protocol must be used to fetch the desired data? Must
#'    be either \code{http} or \code{https}. Defaults to \code{http}.
#' @param version BrAPI version number. Must be either \code{"v1"} or
#'    \code{"v2"}. Defaults to \code{v2}.
#'
#' @return A \code{PHGServerCon} object.
#'
#' @export
PHGServerCon <- function(
    host,
    port = NULL,
    protocol = c("https", "http"),
    version = c("v2", "v1")
) {

    version    <- match.arg(version)
    protocol   <- match.arg(protocol)

    # Check for http(s) prefix and update protocol arg if needed
    httpReg    <- "^http:\\/\\/"
    httpsReg   <- "^https:\\/\\/"
    if (grepl(httpReg, host)) {
        protocol <- "http"
        host <- gsub(httpReg, "", host)
    }
    if (grepl(httpsReg, host)) {
        protocol <- "https"
        host <- gsub(httpsReg, "", host)
    }

    # Check for BrAPI suffix - does not check for v1 status - defaults to v2
    brapiStart <- "\\/brapi\\/(v1|v2)$"
    host <- gsub(brapiStart, "", host)

    if (is.null(port) && protocol == "http") port <- 80
    if (is.null(port) && protocol == "https") port <- 443

    if (!(port %in% 1:65535)) {
        stop("Not a valid port number", call. = FALSE)
    }

    url <- sprintf("%s://%s:%d/brapi/%s", protocol, host, port, version)

    if (!brapiEndpointExists(url)) {
        stop("Cannot resolve mandatory endpoint: {serverinfo}", call. = FALSE)
    }

    methods::new(
        Class    = "PHGServerCon",
        phgType  = "server",
        host     = host,
        port     = port,
        protocol = protocol,
        version  = version,
        url      = url
    )
}



# /// Methods (show) ////////////////////////////////////////////////

## ----
#' @title Show methods for PHGServerCon objects
#'
#' @description
#' Prints out information regarding properties from the \code{PHGServerCon}
#' class to the console
#'
#' @param object A \code{\linkS4class{PHGServerCon}} object
#'
#' @docType methods
#' @rdname PHGServerCon-class
#' @aliases show,PHGServerCon-method
setMethod(
    f = "show",
    signature = "PHGServerCon",
    definition = function(object) {
        pointerSymbol <- cli::col_green(cli::symbol$pointer)

        stat <- httpResp(brapiURL(object))
        msg <- c(
            paste0("A ", cli::style_bold("PHGServerCon"), " connection object"),
            paste0(" ", pointerSymbol, " Host............: ", host(object)),
            paste0(" ", pointerSymbol, " Server Status...: ", stat$status, " (", stat$msg, ")")
        )

        cat(msg, sep = "\n")
    }
)



# /// Methods (general) /////////////////////////////////////////////

## ----
#' @rdname brapiURL
#' @export
setMethod(
    f = "brapiURL",
    signature = signature(object = "PHGServerCon"),
    definition = function(object) {
        return(object@url)
    }
)


## ----
#' @rdname brapiVersion
#' @export
setMethod(
    f = "brapiVersion",
    signature = signature(object = "PHGServerCon"),
    definition = function(object) {
        return(object@version)
    }
)


## ----
#' @rdname port
#' @export
setMethod(
    f = "port",
    signature = signature(object = "PHGServerCon"),
    definition = function(object) {
        return(object@port)
    }
)


## ----
#' @rdname httProtocol
#' @export
setMethod(
    f = "httProtocol",
    signature = signature(object = "PHGServerCon"),
    definition = function(object) {
        return(object@protocol)
    }
)


## ----
#' @rdname serverInfo
#' @export
setMethod(
    f = "serverInfo",
    signature = signature(object = "PHGServerCon"),
    definition = function(object) {
        json2tibble(object, "serverinfo", "calls")
    }
)


## ----
#' @rdname showPHGMethods
#' @export
setMethod(
    f = "showPHGMethods",
    signature = signature(object = "PHGServerCon"),
    definition = function(object, showAdvancedMethods) {
        methodTableFromServer(
            brapiURL(object),
            showAdvancedMethods
        )
    }
)


