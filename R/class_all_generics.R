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
#' @title Calculate mutual information for a given PHG data set
#'
#' @description
#' Calculates mutual information from an rPHG data set object containing
#' haplotype id and reference range information. Mutual information quantifies
#' the "amount of information" obtained about one random variable through
#' observing the other random variable. This will calcuate the the mutual
#' information across all pairs of reference ranges.
#'
#' @param object an \code{rPHG} dataset containing haplotype and reference
#'    range information
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname calcMutualInfo
#' @export
setGeneric("calcMutualInfo", function(object, ...) standardGeneric("calcMutualInfo"))


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
#' @title Return protocol value
#'
#' @description
#' Returns the protocol information for a given object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname httProtocol
#' @export
setGeneric("httProtocol", function(object, ...) standardGeneric("httProtocol"))


## ----
#' @title Return \code{rJava} reference object
#'
#' @description
#' Returns the \code{rJava} memory reference for a given \code{rPHG} object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname javaMemoryAddress
#' @export
setGeneric("javaMemoryAddress", function(object, ...) standardGeneric("javaMemoryAddress"))


## ----
#' @title Return \code{rJava} reference object
#'
#' @description
#' Returns the \code{rJava} memory reference for a given \code{rPHG} object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname javaRefObj
#' @export
setGeneric("javaRefObj", function(object, ...) standardGeneric("javaRefObj"))


## ----
#' @title Return number of chromosomes
#'
#' @description
#' Returns the number of chromosomes for a given object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname numberOfChromosomes
#' @export
setGeneric("numberOfChromosomes", function(object, ...) standardGeneric("numberOfChromosomes"))


## ----
#' @title Return number of nodes
#'
#' @description
#' Returns the number of nodes for a given object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname numberOfNodes
#' @export
setGeneric("numberOfNodes", function(object, ...) standardGeneric("numberOfNodes"))


## ----
#' @title Return number of reference ranges
#'
#' @description
#' Returns the number of reference ranges for a given object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname numberOfRefRanges
#' @export
setGeneric("numberOfRefRanges", function(object, ...) standardGeneric("numberOfRefRanges"))


## ----
#' @title Return number of taxa
#'
#' @description
#' Returns the number of taxa for a given object
#'
#' @param object an \code{rPHG} local or server connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname numberOfTaxa
#' @export
setGeneric("numberOfTaxa", function(object, ...) standardGeneric("numberOfTaxa"))


## ----
#' @title Return number of haplotypes per reference range
#'
#' @description
#' Returns the number of unique haplotype IDs per reference range in an rPHG
#' dataset
#'
#' @param object an \code{rPHG} dataset containing haplotype and reference
#'    range information
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname numHaploPerRefRange
#' @export
setGeneric("numHaploPerRefRange", function(object, ...) standardGeneric("numHaploPerRefRange"))


## ----
#' @title Return a PHG connection object
#'
#' @description
#' Returns an \code{rPHG} connection object
#'
#' @param object an \code{rPHG} method object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname phgConObj
#' @export
setGeneric("phgConObj", function(object, ...) standardGeneric("phgConObj"))


## ----
#' @title Return method ID
#'
#' @description
#' Returns a method ID string for a given \code{rPHG} method class
#'
#' @param object an \code{rPHG} method object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname phgMethodId
#' @export
setGeneric("phgMethodId", function(object, ...) standardGeneric("phgMethodId"))


## ----
#' @title Return method ID type
#'
#' @description
#' Returns a method ID type for a given \code{rPHG} method class
#'
#' @param object an \code{rPHG} method object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname phgMethodType
#' @export
setGeneric("phgMethodType", function(object, ...) standardGeneric("phgMethodType"))


## ----
#' @title Return type of PHG connection
#'
#' @description
#' Returns the PHG type for a given \code{rPHG} local or server connection object
#'
#' @param object an \code{rPHG} connection object
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname phgType
#' @export
setGeneric("phgType", function(object, ...) standardGeneric("phgType"))


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
#' @param verbose should retrieval information be printed? Defaults to
#'    \code{FALSE}
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname readPHGDataSet
#' @export
setGeneric("readPHGDataSet", function(object, verbose = FALSE, ...) standardGeneric("readPHGDataSet"))


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
#' @title Return available PHG methods
#'
#' @description
#' Returns a collection of available PHG methods and metadata
#'
#' @param object an \code{rPHG} local or server connection object
#' @param showAdvancedMethods Do you want to return all possible method IDs
#'    from the database? Defaults to \code{FALSE}.
#' @param ... Additional arguments, for use in specific methods
#'
#' @rdname showPHGMethods
#' @export
setGeneric("showPHGMethods", function(object, showAdvancedMethods = FALSE, ...) standardGeneric("showPHGMethods"))


