#' @title The URL of a \code{BrapiCon} object
#'
#' @description get or set the Uniform Resource Locator (URL) of a
#'   \code{BrapiCon} object.
#'
#' @param x a \linkS4class{BrapiCon}.
#' @param value a URL string of class \code{character}.
#'
#' @rdname brapiURL
#'
#' @aliases
#' brapiURL
#' brapiURL<-
#'
#' @export
setGeneric("brapiURL<-", function(x, value) standardGeneric("brapiURL<-"))

#' @aliases brapiURL
#' @rdname brapiURL
#' @export
setGeneric("brapiURL", function(x) standardGeneric("brapiURL"))

#' @aliases brapiURL
#' @rdname brapiURL
#' @export
setMethod("brapiURL", signature = c(x = "BrapiCon"), function(x) return(x@url))

#' @aliases brapiURL
#' @rdname brapiURL
#' @export
setMethod(
    f = "brapiURL<-",
    signature = "BrapiCon",
    definition = function(x, value) {
        if (length(value) != 1){
            stop("URL must be a single character string", call. = FALSE)
        }
        x@url <- value
        return(x)
    }
)



setGeneric("host", function(x) standardGeneric("host"))
setGeneric("port", function(x) standardGeneric("port"))
setGeneric("protocol", function(x) standardGeneric("protocol"))
setGeneric("version", function(x) standardGeneric("version"))
setGeneric("token", function(x) standardGeneric("token"))


# setGeneric("host<-", function(x) standardGeneric("host<-"))
# setGeneric("port<-", function(x) standardGeneric("port<-"))
# setGeneric("protocol<-", function(x) standardGeneric("protocol<-"))
# setGeneric("version<-", function(x) standardGeneric("version<-"))
# setGeneric("token<-", function(x) standardGeneric("token<-"))



## Getters ----
setMethod("host", signature("BrapiCon"), function(x) x@host)
setMethod("port", signature = "BrapiCon", function(x) x@port)
setMethod("protocol", signature = "BrapiCon", function(x) x@protocol)
setMethod("version", signature = "BrapiCon", function(x) x@version)
setMethod("token", signature = "BrapiCon", function(x) x@token)



## Setters ----
# setMethod("host<-", signature("BrapiCon"), function(x, value) {
#     x@host <- value
#     stopifnot(validObject(x))
#     x
# })
# setMethod("port<-", signature = "BrapiCon", function(x, value) {
#     x@port <- value
#     stopifnot(validObject(x))
#     x
# })
# setMethod("protocol<-", signature = "BrapiCon", function(x, value) {
#     x@protocol <- value
#     stopifnot(validObject(x))
#     x
# })
# setMethod("version<-", signature = "BrapiCon", function(x, value) {
#     x@version <- value
#     stopifnot(validObject(x))
#     x
# })
# setMethod("token<-", signature = "BrapiCon", function(x, value) {
#     x@token <- value
#     stopifnot(validObject(x))
#     x
# })
# setMethod("url<-", signature = "BrapiCon", function(x, value) {
#     x@url <- value
#     stopifnot(validObject(x))
#     x
# })


