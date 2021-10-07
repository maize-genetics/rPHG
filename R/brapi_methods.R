#####################################################################
##
## Overview:
##  This file houses methods and generics related to `BrapiCon` and
##  `BrapiConPHG` classes
##
#####################################################################

# === BrapiCon general methods ======================================

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
#' @description Retrieves data from the \code{callsets} endpoint of a BrAPI
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


## Get server information ----
#' @title Retrieve server info data from BrAPI connection
#'
#' @description Retrieves data from the \code{serverinfo} endpoint of a BrAPI
#'   server.
#'
#' @param object A \code{BrapiCon} object.
#'
#' @rdname serverInfo
#'
#' @export
setGeneric("serverInfo", function(object) standardGeneric("serverInfo"))

#' @rdname serverInfo
#' @export
setMethod(
    f = "serverInfo",
    signature = "BrapiCon",
    definition = function(object) {
        json2tibble(object, "serverinfo", "calls")
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


## Get references ----
#' @title Retrieve reference data from BrAPI connection
#'
#' @description Retrieves data from the \code{references} endpoint of a BrAPI
#'   server.
#'
#' @param object A \code{BrapiCon} object.
#'
#' @rdname references
#'
#' @export
setGeneric("references", function(object) standardGeneric("references"))

#' @rdname references
#' @export
setMethod(
    f = "references",
    signature = "BrapiCon",
    definition = function(object) {
        json2tibble(object, "references")
    }
)


## Get studies ----
#' @title Retrieve study data from BrAPI connection
#'
#' @description Retrieves data from the \code{studies} endpoint of a BrAPI
#'   server.
#'
#' @param object A \code{BrapiCon} object.
#'
#' @rdname studies
#'
#' @export
setGeneric("studies", function(object) standardGeneric("studies"))

#' @rdname studies
#' @export
setMethod(
    f = "studies",
    signature = "BrapiCon",
    definition = function(object) {
        json2tibble(object, "studies")
    }
)


## Get reference sets ----
#' @title Retrieve reference set data from BrAPI connection
#'
#' @description Retrieves data from the \code{referenceSets} endpoint of a BrAPI
#'   server.
#'
#' @param object A \code{BrapiCon} object.
#'
#' @rdname referenceSets
#'
#' @export
setGeneric("referenceSets", function(object) standardGeneric("referenceSets"))

#' @rdname referenceSets
#' @export
setMethod(
    f = "referenceSets",
    signature = "BrapiCon",
    definition = function(object) {
        json2tibble(object, "referencesets")
    }
)


## Get available PHG methods ----
#' @title Retrieve available PHG method data from BrAPI connection
#'
#' @description Retrieves data from the \code{variantTables} endpoint of a BrAPI
#'   server.
#'
#' @param object A \code{BrapiCon} object.
#'
#' @rdname availablePHGMethods
#'
#' @export
setGeneric("availablePHGMethods", function(object) standardGeneric("availablePHGMethods"))

#' @rdname availablePHGMethods
#' @export
setMethod(
    f = "availablePHGMethods",
    signature = "BrapiCon",
    definition = function(object) {
        json2tibble(object, "variantTables")
    }
)





# === BrapiConPHG general methods ===================================

## Display message ----
#' @title Show method for BrapiConPHG objects
#'
#' @description Prints out the information from the BrAPI connection object
#'   including server status codes. See this
#'   \href{https://en.wikipedia.org/wiki/List_of_HTTP_status_codes}{Wikipedia link}
#'   for further details about what these codes mean.
#'
#' @param object a \code{\linkS4class{BrapiConPHG}} object.
#'
#' @docType methods
#' @name show
#' @rdname show
#' @aliases show show,BrapiCon-method
#'
#' @export
setMethod(
    f = "show",
    signature = "BrapiConPHG",
    definition = function(object) {
        rrCheck <- ifelse(is.na(object@refRangeFilter), "[ ]", "[x]")
        sampleCheck <- ifelse(is.na(object@sampleFilter), "[ ]", "[x]")

        cat("<BrapiConPHG: BrAPI <-> PHG pointer object>\n")
        cat("  method:         ", object@methodID, "\n")
        cat("  variant filter: ", rrCheck, "\n")
        cat("  sample filter:  ", sampleCheck, "\n")
    }
)


## (2A) Filter columns (ref ranges) ----
#' @export
filterRefRanges <- function(
    x,
    gr = NULL,
    chromosome = NULL,
    start = NULL,
    end = NULL
) {
    if (class(x) != "BrapiConPHG") {
        stop("A `BrapiConPHG` object is needed for the LHS argument", call. = FALSE)
    }

    if (!is.null(gr)) {
        if (inherits(gr, "GRanges")) {
            if (is.null(chromosome)) {
                grDF <- as.data.frame(gr)
                seqString <- paste0(
                    grDF$seqnames, ":",
                    grDF$start, "-", grDF$end,
                    collapse = ","
                )
                rrString <- paste0("ranges=", seqString)
            } else {
                grSub <- dropSeqlevels(gr, chromosome, pruning.mode = "coarse")
                grDF <- as.data.frame(grSub)
                seqStringGR <- paste0(
                    grDF$seqnames, ":",
                    grDF$start, "-", grDF$end,
                    collapse = ","
                )
                seqStringChr <- paste0(chromosome, collapse = ",")
                rrString <- paste0("ranges=", seqStringChr, ",", seqStringGR)
            }

        } else {
            stop("Not a valid GRanges object", call. = FALSE)
        }
    } else {
        if (!is.null(chromosome) && is.null(start) && is.null(end)) {
            rrString <- paste0("ranges=", paste0(chromosome, collapse = ","))
        } else if (!is.null(chromosome) && !is.null(start) && !is.null(end)) {
            if (length(unique(sapply(list(chromosome, start, end), length))) == 1) {
                seqString <- paste0(
                    chromosome, ":",
                    start, "-", end,
                    collapse = ","
                )
                rrString <- paste0("ranges=", seqString)
            } else {
                stop("Range vectors do not have the same length", call. = FALSE)
            }
        } else {
            stop("Incorrect filtration parameters", call. = FALSE)
        }
    }

    # Add filter on `refRangeFilter` slot
    x@refRangeFilter <- rrString

    return(x)
}


## (2A) Filter rows (samples)
#' @export
filterSamples <- function(x, samples) {
    if (class(x) != "BrapiConPHG") {
        stop("A `BrapiConPHG` object is needed for the LHS argument", call. = FALSE)
    }

    if (is.vector(samples) && is.atomic(samples)) {
        sampleString <- paste0("sampleNames=", paste0(samples, collapse = ","))
    } else {
        stop("`samples` argument must be an atomic vector", call. = FALSE)
    }

    x@sampleFilter <- sampleString

    return(x)
}


## Get ref ranges for given PHG method ----
#' @title Retrieve available ref range data from a given PHG method
#'
#' @description Retrieves reference range information from a given PHG method.
#'   Data returned is (1) chromosome, (2) start, and (3) stop coordinates.
#'
#' @param object A \code{BrapiConPHG} object.
#'
#' @rdname readRefRanges
#'
#' @export
setGeneric("readRefRanges", function(object) standardGeneric("readRefRanges"))

#' @rdname readRefRanges
#'
#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#'
#' @export
setMethod(
    f = "readRefRanges",
    signature = "BrapiConPHG",
    definition = function(object) {
        urls <- getVTList(object)

        rrDF <- parseJSON(urls$rangeURL)
        rrDF <- rrDF$result$data

        gr <- GenomicRanges::GRanges(
            seqnames = rrDF$referenceName,
            ranges = IRanges::IRanges(rrDF$start, rrDF$end),
            variantDbId = as.numeric(rrDF$variantDbId)
        )
        return(gr)
    }
)


## Get samples for given PHG method ----
#' @title Retrieve available sample data from a given PHG method
#'
#' @description Retrieves sample information from a given PHG method.
#'   Data returned is (1) sample name, (2) sample DB ID, (3) description,
#'   and (4) additional information.
#'
#' @param object A \code{BrapiConPHG} object.
#'
#' @rdname readSamples
#'
#' @export
setGeneric("readSamples", function(object) standardGeneric("readSamples"))

#' @rdname readSamples
#'
#' @importFrom tibble as_tibble
#'
#' @export
setMethod(
    f = "readSamples",
    signature = "BrapiConPHG",
    definition = function(object) {
        urls <- getVTList(object)

        sampleDF <- parseJSON(urls$sampleURL)
        sampleDF <- sampleDF$result$data

        return(tibble::as_tibble(sampleDF))
    }
)


## Get table for given PHG method ----
#' @title Retrieve available table data from a given PHG method
#'
#' @description Retrieves table information from a given PHG method.
#'   Data returned is a \code{matrix} object.
#'
#' @param object A \code{BrapiConPHG} object.
#'
#' @rdname readTable
#'
#' @export
setGeneric("readTable", function(object) standardGeneric("readTable"))

#' @rdname readTable
#'
#' @export
setMethod(
    f = "readTable",
    signature = "BrapiConPHG",
    definition = function(object) {
        urls <- getVTList(object)

        tableDF <- parseJSON(urls$tableURL)
        tableDF <- tableDF$result$genotypes

        return(tableDF)
    }
)
