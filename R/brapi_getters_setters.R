# === BrAPI getters and setters (maybe) =============================

## BrAPI URL ----
#' @title The URL of a \code{BrapiCon} object
#'
#' @description get or set the Uniform Resource Locator (URL) of a
#'   \code{BrapiCon} object.
#'
#' @param x a \linkS4class{BrapiCon} object.
#'
#' @rdname brapiURL
#'
#' @export
setGeneric("brapiURL", function(x) standardGeneric("brapiURL"))

#' @rdname brapiURL
#' @export
setMethod("brapiURL", signature = c(x = "BrapiCon"), function(x) return(x@url))



setGeneric("host", function(x) standardGeneric("host"))
setMethod("host", signature("BrapiCon"), function(x) x@host)

setGeneric("port", function(x) standardGeneric("port"))
setMethod("port", signature = "BrapiCon", function(x) x@port)

setGeneric("protocol", function(x) standardGeneric("protocol"))
setMethod("protocol", signature = "BrapiCon", function(x) x@protocol)

setGeneric("version", function(x) standardGeneric("version"))
setMethod("version", signature = "BrapiCon", function(x) x@version)

setGeneric("token", function(x) standardGeneric("token"))
setMethod("token", signature = "BrapiCon", function(x) x@token)


