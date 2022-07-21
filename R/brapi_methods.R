#####################################################################
##
## Overview:
##  This file houses methods and generics related to `BrapiCon` and
##  `BrapiConPHG` classes
##
#####################################################################

# === BrapiCon general methods ======================================

## ----
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
setMethod(
    f = "show",
    signature = "BrapiCon",
    definition = function(object) {

        status <- tryCatch(
            expr = {
                httr::GET(paste0(brapiURL(object), "/serverinfo"))$status
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


## ----
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


## ----
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


## ----
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


## ----
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

## ----
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
#' @aliases show show,BrapiConPHG-method
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


## ----
#' @title Filter reference ranges from given PHG method
#'
#' @description Filters reference ranges for a given PHG method by
#'   manipulation of BrAPI samples URL call. For a given query, reference
#'   ranges will be returned if they overlap with a user-defined range.
#'   Uses 1-based coordinate information.
#'
#' @param x A \code{BrapiConPHG} object.
#' @param gr A \code{GRanges} object. Houses genomic range information for
#'   filter.
#' @param chromosome A vector of chromosome ids of type \code{character}. Can
#'   be of length one to size \code{n}. If used, this will return all reference
#'   ranges within a given chromosome.
#' @param start A vector of start positions of type \code{numeric}. If used,
#'   an equal number of \code{end} elements will be needed to avoid error.
#' @param end A vector of end positions of type \code{numeric}. These will
#'   link up with the \code{start} positions. Must be equal to the \code{start}
#'   parameter.
#'
#' @importFrom GenomeInfoDb dropSeqlevels
#'
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
                grSub <- GenomeInfoDb::dropSeqlevels(gr, chromosome, pruning.mode = "coarse")
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


## ----
#' @title Filter samples from given PHG method
#'
#' @description Filters samples for a given PHG method by manipulation of BrAPI
#'   samples URL call. Returns exact matches only. If query is not exact match,
#'   no data will be returned for that given sample.
#'
#' @param x A \code{BrapiConPHG} object.
#' @param samples A vector of taxa ID of type \code{character}.
#'
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


## ----
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
#' @importFrom rJava .jevalArray
#' @importFrom rJava .jnew
#'
#' @export
setMethod(
    f = "readRefRanges",
    signature = "BrapiConPHG",
    definition = function(object) {
        urls <- getVTList(object)

        rJC <- rJava::.jnew("net/maizegenetics/pangenome/api/RPHGMethodsKotlin")

        rrArray <- rJC$getRefRangesFromBrapi(
            urls$rangeURL,
            as.integer(1000)
        )
        rrArray <- rJava::.jevalArray(rrArray, simplify = TRUE)

        gr <- GenomicRanges::GRanges(
            seqnames = rrArray[1, ],
            ranges = IRanges::IRanges(
                start = as.numeric(rrArray[2, ]),
                end = as.numeric(rrArray[3, ])
            ),
            variantDbId = as.numeric(rrArray[4, ])
        )
        return(gr)
    }
)


## ----
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


## ----
#' @title Retrieve available table data from a given PHG method
#'
#' @description Retrieves table information from a given PHG method.
#'   Data returned is a \code{matrix} object.
#'
#' @param object A \code{BrapiConPHG} object.
#' @param ... Additional arguments to be passed.
#'
#' @rdname readTable
#'
#' @export
setGeneric("readTable", function(object, ...) standardGeneric("readTable"))

#' @rdname readTable
#'
#' @param index Should index values be returned? If \code{FALSE}, will return
#'   haplotype IDs.
#' @param verbose Show messages to console?
#' @param transpose Should data be transposed upon return?
#'
#' @export
setMethod(
    f = "readTable",
    signature = "BrapiConPHG",
    definition = function(object, index = FALSE, verbose = TRUE, transpose = TRUE) {
        if (verbose) message("Downloading table data...")
        urls <- getVTList(object)

        rJC <- rJava::.jnew("net/maizegenetics/pangenome/api/RPHGMethodsKotlin")

        rrArray <- rJC$getRefRangesFromBrapi(
            urls$rangeURL,
            60000L
        )
        rrArray <- rJava::.jevalArray(rrArray, simplify = TRUE)

        if (index) {
            tableArray <- rJC$getHapIndexArrayFromBrapi(
                urls$tableURL,
                1000L
            )
        } else {
            tableArray <- rJC$getHapIdArrayFromBrapi(
                urls$tableURL,
                urls$rangeURL,
                1000L
            )
        }
        tableArray <- rJava::.jevalArray(tableArray, simplify = TRUE)

        sampleNames <- parseJSON(urls$sampleURL)
        sampleNames <- sampleNames$result$data$sampleName

        colnames(tableArray) <- sampleNames
        rownames(tableArray) <- rrArray[4, ]

        if (transpose) {
            ta <- t(tableArray)
        } else {
            ta <- tableArray
        }

        gc <- base::gc()
        return(ta)
    }
)


## ----
#' @title Read PHGDataset object from BrAPI PHG method
#'
#' @description Creates a \code{PHGDataset} object by reading sample,
#'   reference range, and feature data information.
#'
#' @param object A \code{BrapiConPHG} object.
#' @param ... Additional arguments to be passed.
#'
#' @rdname readPHGDatasetFromBrapi
#'
#' @export
setGeneric("readPHGDatasetFromBrapi", function(object, ...) {
    standardGeneric("readPHGDatasetFromBrapi")
})

#' @rdname readTable
#'
#' @export
setMethod(
    f = "readPHGDatasetFromBrapi",
    signature = "BrapiConPHG",
    definition = function(object, verbose = TRUE) {
        if (verbose) message("Downloading PHG data...")

        urls <- getVTList(object)

        rr <- readRefRanges(object)
        hapArray <- readTable(object, transpose = FALSE, verbose = FALSE)
        samples <- readSamples(object)

        phgSE <- SummarizedExperiment::SummarizedExperiment(
            assays = list(hapID = hapArray),
            rowRanges = rr,
            colData = samples
        )

        return(methods::new(Class = "PHGDataSet", phgSE))
    }
)




