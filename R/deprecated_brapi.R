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


## ## ----
## #' @rdname readHaplotypeIds
## #'
## #' @param numCores Number of processing cores for faster processing times.
## #' @param transpose Do you want to transpose table?
## #'
## #' @importFrom cli cli_progress_bar
## #' @importFrom cli cli_progress_done
## #' @importFrom cli cli_progress_step
## #' @importFrom cli cli_progress_update
## #' @importFrom httr content
## #' @importFrom httr GET
## #' @importFrom jsonlite fromJSON
## #' @importFrom parallel mclapply
## #'
## #' @export
## setMethod(
##     f = "readHaplotypeIds",
##     signature = "BrapiConPHG",
##     definition = function(object, numCores = NULL, transpose = TRUE) {
##         # Logic checks
##         if (is.null(numCores)) {
##             numCores <- 1
##         }
##         if (!is.numeric(numCores)) {
##             stop("numCores parameter must be numeric or NULL")
##         }
## 
##         # Get URLs
##         urls <- getVTList(object)
## 
##         # Calculate total pages
## 
##         if (object@methodID == "DEMO") {
##             totalVariants <- 1000
##             totalPages <- ceiling(totalVariants / 250)
##         } else {
##             methods <- availablePHGMethods(object)
##             totalVariants <- methods[which(methods$variantTableDbId == object@methodID), ]$numVariants
##             totalPages <- ceiling(totalVariants / 10000)
##         }
## 
##         # Download each page (iterative)
##         # TODO - can we async this? (e.g. futures)
##         allResp <- vector("list", totalPages)
##         # cli::cli_progress_step("Establishing connection")
##         message("Establishing connection")
##         # cli::cli_progress_bar("   - Downloading: ", total = totalPages)
##         message("Downloading:")
##         pb <- utils::txtProgressBar(
##             style = 3,
##             char  = "=",
##             min = 1,
##             max = totalPages
##         )
##         for (i in seq_len(totalPages)) {
##             currentUrl <- sprintf(urls$tableURL, i - 1, 0)
##             allResp[[i]] <- httr::GET(currentUrl)
##             utils::setTxtProgressBar(pb, i)
##             # cli::cli_progress_update()
##         }
##         close(pb)
##         # cli::cli_progress_done()
## 
##         # F1 - Convert hap ID string to integer (e.g. "21/21" -> 21)
##         brapiHapIdStringToInt <- function(x) {
##             id <- strsplit(x, "/")[[1]][1]
##             ifelse(id == ".", return(NA), return(as.integer(id)))
##         }
## 
##         # F2 - process matrix slices (convert from JSON to int matrix)
##         processMatrix <- function(x) {
##             xNew <- httr::content(x, as = "text", encoding = "ISO-8859-1")
##             xNew <- jsonlite::fromJSON(xNew)
##             xMat <- xNew$result$dataMatrices$dataMatrix[[1]]
##             colnames(xMat) <- xNew$result$callSetDbIds
##             rownames(xMat) <- xNew$result$variants
##             xMat <- apply(xMat, c(1, 2), brapiHapIdStringToInt)
##             return(xMat)
##         }
## 
##         # Clean up data (parallel)
##         # cli::cli_progress_step("Cleaning data")
##         message("Cleaning data")
##         finalMatrices <- parallel::mclapply(allResp, processMatrix, mc.cores = numCores)
## 
##         # Bind all data into one matrix and return
##         # cli::cli_progress_step("Combining responses")
##         message("Combining responses")
##         if (transpose) {
##             unionMatrix <- t(do.call(rbind, finalMatrices))
##         } else {
##             unionMatrix <- do.call(rbind, finalMatrices)
##         }
## 
##         return(unionMatrix)
##     }
## )

## 
## 
## ## ----
## #' @rdname readPHGDataSet
## #'
## #' @export
## setMethod(
##     f = "readPHGDataSet",
##     signature = "BrapiConPHG",
##     definition = function(object, ...) {
## 
##         urls <- getVTList(object)
## 
##         hapArray <- readTable(object, transpose = FALSE)
## 
##         # cli::cli_progress_step("Getting ref range data")
##         message("Getting ref range data")
##         rr <- readRefRanges(object)
##         # cli::cli_progress_step("Getting sample data")
##         message("Getting sample data")
##         samples <- readSamples(object)
## 
##         colnames(hapArray) <- samples$sampleName
## 
##         phgSE <- SummarizedExperiment::SummarizedExperiment(
##             assays = list(hapID = hapArray),
##             rowRanges = rr,
##             colData = samples
##         )
## 
##         return(methods::new(Class = "PHGDataSet", phgSE))
##     }
## )
## 
## 




