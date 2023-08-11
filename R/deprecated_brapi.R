#####################################################################
##
## Overview:
##  This file houses **DEFUNCT** methods and generics related to
## `BrapiCon` and `BrapiConPHG` classes. Keeping these functions
## to revise at a later date...
##
#####################################################################

# ## Get taxa ----
# #' @title Retrieve samples from BrAPI connection
# #'
# #' @description Retrieves data from the \code{samples} endpoint of a BrAPI
# #'   server.
# #'
# #' @param object a \code{\linkS4class{BrapiCon}} object.
# #'
# #' @rdname samples
# #'
# #' @export
# setGeneric("samples", function(object) standardGeneric("samples"))
#
# #' @rdname samples
# #' @export
# setMethod(
#     f = "samples",
#     signature = "BrapiCon",
#     definition = function(object) {
#         json2tibble(object, "samples")
#     }
# )


# ## Get calls ----
# #' @title Retrieve calls from BrAPI connection
# #'
# #' @description Retrieves data from the \code{calls} endpoint of a BrAPI
# #'   server.
# #'
# #' @param object a \linkS4class{BrapiCon} object.
# #'
# #' @rdname calls
# #'
# #' @export
# setGeneric("calls", function(object) standardGeneric("calls"))
#
# #' @rdname calls
# #' @export
# setMethod(
#     f = "calls",
#     signature = "BrapiCon",
#     definition = function(object) {
#         json2tibble(object, "calls")
#     }
# )


# ## Get callsets ----
# #' @title Retrieve callsets from BrAPI connection
# #'
# #' @description Retrieves data from the \code{callsets} endpoint of a BrAPI
# #'   server.
# #'
# #' @param object A \code{BrapiCon} object.
# #'
# #' @rdname callsets
# #'
# #' @export
# setGeneric("callsets", function(object) standardGeneric("callsets"))
#
# #' @rdname callsets
# #' @export
# setMethod(
#     f = "callsets",
#     signature = "BrapiCon",
#     definition = function(object) {
#         json2tibble(object, "callsets")
#     }
# )


# ## Get graphs ----
# #' @title Retrieve graph data from BrAPI connection
# #'
# #' @description Retrieves data from the \code{graphs} endpoint of a BrAPI
# #'   server.
# #'
# #' @param object A \code{BrapiCon} object.
# #' @param dbID A PHG method.
# #'
# #' @rdname phGraph
# #'
# #' @export
# setGeneric("phGraph", function(object, dbID) standardGeneric("phGraph"))
#
# #' @rdname phGraph
# #' @export
# setMethod(
#     f = "phGraph",
#     signature = "BrapiCon",
#     definition = function(object, dbID) {
#         json2igraph(object, dbID)
#     }
# )


# ## Get studies ----
# #' @title Retrieve study data from BrAPI connection
# #'
# #' @description Retrieves data from the \code{studies} endpoint of a BrAPI
# #'   server.
# #'
# #' @param object A \code{BrapiCon} object.
# #'
# #' @rdname studies
# #'
# #' @export
# setGeneric("studies", function(object) standardGeneric("studies"))
#
# #' @rdname studies
# #' @export
# setMethod(
#     f = "studies",
#     signature = "BrapiCon",
#     definition = function(object) {
#         json2tibble(object, "studies")
#     }
# )









