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

        if (is.numeric(status) && status >= 200 && status <= 299) {
            statusMsg <- "(OK)"
        } else {
            statusMsg <- ""
        }

        # cat("A BrAPI connection object\n")
        # cat("  Server...........:", host(object), "\n")
        # cat("  Port.............:", port(object), "\n")
        # cat("  Server status....:", status, statusMsg, "\n")
        # cat("  BrAPI version....:", version(object), "\n")

        pointerSymbol <- cli::col_green(cli::symbol$pointer)
        msg <- c(
            paste0("A ", cli::style_bold("BrAPI"), " connection object"),
            paste0(" ", pointerSymbol, " Server...........: ", host(object)),
            paste0(" ", pointerSymbol, " Port.............: ", port(object)),
            paste0(" ", pointerSymbol, " Server status....: ", statusMsg),
            paste0(" ", pointerSymbol, " BrAPI version....: ", version(object))
        )

        cat(msg, sep = "\n")
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
        ## Temp fix to return proper methods
        fullTable <- json2tibble(object, "variantTables")
        filtTable <- fullTable[fullTable$numSamples > 100, ] # arbitrary n
        return(filtTable)
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
        # cli::cli_div(theme = list(ul = list(`margin-left` = 2, before = "")))

        # activeSlotMsg   <- cli::symbol$square_small_filled
        # inactiveSlotMsg <- cli::symbol$square_small
        activeSlotMsg <- "[x]"
        inactiveSlotMsg <- "[ ]"

        rrCheck <- ifelse(
            test = is.na(object@refRangeFilter),
            yes  = inactiveSlotMsg,
            no   = activeSlotMsg
        )
        sampleCheck <- ifelse(
            test = is.na(object@sampleFilter),
            yes  = inactiveSlotMsg,
            no   = activeSlotMsg
        )

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
# #' @export
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
# #' @export
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

        # rJC <- rJava::.jnew("net/maizegenetics/pangenome/api/RMethodsKotlin")
        # rrArray <- rJC$getRefRangesFromBrapi(
        #     urls$rangeURL,
        #     as.integer(1000)
        # )
        # rrArray <- rJava::.jevalArray(rrArray, simplify = TRUE)

        pageSize <- ifelse(
            grepl("variants$", urls$rangeURL),
            "?pageSize=",
            "&pageSize="
        )

        if (object@methodID == "DEMO") {
            rrDF <- parseJSON(paste0(urls$rangeURL, pageSize, "1000"))
        } else {
            rrDF <- parseJSON(paste0(urls$rangeURL, pageSize, "150000"))
        }
        rrDF <- rrDF$result$data

        gr <- GenomicRanges::GRanges(
            seqnames = rrDF$referenceName,
            ranges = IRanges::IRanges(
                start = rrDF$start,
                end = rrDF$end
            ),
            variantDbId = rrDF$variantDbId
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

        if (object@methodID == "DEMO") {
            return(utils::head(tibble::as_tibble(sampleDF), n = 25))
        } else{
            return(tibble::as_tibble(sampleDF))
        }
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
setGeneric("readTable", function(object, ...) {
    standardGeneric("readTable")
})

#' @rdname readTable
#'
#' @param numCores Number of processing cores for faster processing times.
#' @param transpose Do you want to transpose table?
#'
#' @importFrom cli cli_progress_bar
#' @importFrom cli cli_progress_done
#' @importFrom cli cli_progress_step
#' @importFrom cli cli_progress_update
#' @importFrom httr content
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom parallel mclapply
#'
#' @export
setMethod(
    f = "readTable",
    signature = "BrapiConPHG",
    definition = function(object, numCores = NULL, transpose = TRUE) {
        # Logic checks
        if (is.null(numCores)) {
            numCores <- 1
        }
        if (!is.numeric(numCores)) {
            stop("numCores parameter must be numeric or NULL")
        }

        # Get URLs
        urls <- getVTList(object)

        # Calculate total pages

        if (object@methodID == "DEMO") {
            totalVariants <- 1000
            totalPages <- ceiling(totalVariants / 250)
        } else {
            methods <- availablePHGMethods(object)
            totalVariants <- methods[which(methods$variantTableDbId == object@methodID), ]$numVariants
            totalPages <- ceiling(totalVariants / 10000)
        }

        # Download each page (iterative)
        # TODO - can we async this? (e.g. futures)
        allResp <- vector("list", totalPages)
        # cli::cli_progress_step("Establishing connection")
        message("Establishing connection")
        # cli::cli_progress_bar("   - Downloading: ", total = totalPages)
        message("Downloading:")
        pb <- utils::txtProgressBar(
            style = 3,
            char  = "=",
            min = 1,
            max = totalPages
        )
        for (i in seq_len(totalPages)) {
            currentUrl <- sprintf(urls$tableURL, i - 1, 0)
            allResp[[i]] <- httr::GET(currentUrl)
            utils::setTxtProgressBar(pb, i)
            # cli::cli_progress_update()
        }
        close(pb)
        # cli::cli_progress_done()

        # F1 - Convert hap ID string to integer (e.g. "21/21" -> 21)
        brapiHapIdStringToInt <- function(x) {
            id <- strsplit(x, "/")[[1]][1]
            ifelse(id == ".", return(NA), return(as.integer(id)))
        }

        # F2 - process matrix slices (convert from JSON to int matrix)
        processMatrix <- function(x) {
            xNew <- httr::content(x, as = "text", encoding = "ISO-8859-1")
            xNew <- jsonlite::fromJSON(xNew)
            xMat <- xNew$result$dataMatrices$dataMatrix[[1]]
            colnames(xMat) <- xNew$result$callSetDbIds
            rownames(xMat) <- xNew$result$variants
            xMat <- apply(xMat, c(1, 2), brapiHapIdStringToInt)
            return(xMat)
        }

        # Clean up data (parallel)
        # cli::cli_progress_step("Cleaning data")
        message("Cleaning data")
        finalMatrices <- parallel::mclapply(allResp, processMatrix, mc.cores = numCores)

        # Bind all data into one matrix and return
        # cli::cli_progress_step("Combining responses")
        message("Combining responses")
        if (transpose) {
            unionMatrix <- t(do.call(rbind, finalMatrices))
        } else {
            unionMatrix <- do.call(rbind, finalMatrices)
        }

        return(unionMatrix)
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
    definition = function(object, ...) {

        urls <- getVTList(object)

        hapArray <- readTable(object, transpose = FALSE)

        # cli::cli_progress_step("Getting ref range data")
        message("Getting ref range data")
        rr <- readRefRanges(object)
        # cli::cli_progress_step("Getting sample data")
        message("Getting sample data")
        samples <- readSamples(object)

        colnames(hapArray) <- samples$sampleName

        phgSE <- SummarizedExperiment::SummarizedExperiment(
            assays = list(hapID = hapArray),
            rowRanges = rr,
            colData = samples
        )

        return(methods::new(Class = "PHGDataSet", phgSE))
    }
)


