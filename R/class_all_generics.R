## ----
#' @title Return URL path
#'
#' @description
#' Returns the Uniform Resource Locator (URL) of a \code{BrapiCon} object.
#'
#' @param object an \code{rPHG} local or server connection object.
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname brapiURL
#' @export
setGeneric("brapiURL", function(object, ...) standardGeneric("brapiURL"))


## ----
#' @title Return host data
#'
#' @description
#' Returns the host information for a given object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname host
#' @export
setGeneric("host", function(object, ...) standardGeneric("host"))


## ----
#' @title Return port value
#'
#' @description
#' Returns the port information for a given object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname port
#' @export
setGeneric("port", function(object, ...) standardGeneric("port"))


## ----
#' @title Return BrAPI version ID
#'
#' @description
#' Returns the version ID for a BrAPI-compliant PHG server
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname brapiVersion
#' @export
setGeneric("brapiVersion", function(object, ...) standardGeneric("brapiVersion"))


## ----
#' @title Return name of DB
#'
#' @description
#' Returns the name for a given PHG database
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname dbName
#' @export
setGeneric("dbName", function(object, ...) standardGeneric("dbName"))


## ----
#' @title Return type of DB
#'
#' @description
#' Returns the type (e.g. postgres or sqlite) for a given PHG database
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname dbType
#' @export
setGeneric("dbType", function(object, ...) standardGeneric("dbType"))


## ----
#' @title Return file path of configuration file
#'
#' @description
#' Returns the file path for a configuration file to a PHG database
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname configFilePath
#' @export
setGeneric("configFilePath", function(object, ...) standardGeneric("configFilePath"))


## ----
#' @title Return available PHG methods
#'
#' @description
#' Returns a collection of available PHG methods and metadata
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname showPHGMethods
#' @export
setGeneric("showPHGMethods", function(object, ...) standardGeneric("showPHGMethods"))


## ----
#' @title Return server information
#'
#' @description
#' Get avaiable BrAPI calls from BrAPI compliant PHG server
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname serverInfo
#' @export
setGeneric("serverInfo", function(object, ...) standardGeneric("serverInfo"))


## ----
#' @title Return reference ranges
#'
#' @description
#' Get reference range data for a given PHG method
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname readRefRanges
#' @export
setGeneric("readRefRanges", function(object, ...) standardGeneric("readRefRanges"))


## ----
#' @title Return samples IDs
#'
#' @description
#' Gets sample ID data for a given PHG method
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname readSamples
#' @export
setGeneric("readSamples", function(object, ...) standardGeneric("readSamples"))


## ----
#' @title Return haplotype IDs
#'
#' @description
#' Gets haplotype ID for given samples and reference ranges for PHG method
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname readHaplotypeIds
#' @export
setGeneric("readHaplotypeIds", function(object, ...) standardGeneric("readHaplotypeIds"))


## ----
#' @title Return a PHGDataSet
#'
#' @description
#' Creates a \code{\linkS4class{PHGDataSet}} for a given PHG method. This will
#' return all 3 primary sources of data (samples, reference ranges, and
#' haplotype IDs).
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname readPHGDataSet
#' @export
setGeneric("readPHGDataSet", function(object, ...) standardGeneric("readPHGDataSet"))


