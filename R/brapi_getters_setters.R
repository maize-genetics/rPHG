# === BrAPI getters and setters =====================================

## Generics ----
setGeneric("host", function(x) standardGeneric("host"))
setGeneric("port", function(x) standardGeneric("port"))
setGeneric("protocol", function(x) standardGeneric("protocol"))
setGeneric("version", function(x) standardGeneric("version"))
setGeneric("token", function(x) standardGeneric("token"))

#' @rdname brapiURL
#' @export
setGeneric("brapiURL", function(x) standardGeneric("brapiURL"))

# setGeneric("host<-", function(x) standardGeneric("host<-"))
# setGeneric("port<-", function(x) standardGeneric("port<-"))
# setGeneric("protocol<-", function(x) standardGeneric("protocol<-"))
# setGeneric("version<-", function(x) standardGeneric("version<-"))
# setGeneric("token<-", function(x) standardGeneric("token<-"))
# setGeneric("url<-", function(x) standardGeneric("url<-"))



## Getters ----
setMethod("host", signature("BrapiCon"), function(x) x@host)
setMethod("port", signature = "BrapiCon", function(x) x@port)
setMethod("protocol", signature = "BrapiCon", function(x) x@protocol)
setMethod("version", signature = "BrapiCon", function(x) x@version)
setMethod("token", signature = "BrapiCon", function(x) x@token)
#' @export
setMethod("brapiURL", signature = "BrapiCon", function(x) x@url)



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


