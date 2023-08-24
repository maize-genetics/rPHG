## ----
#' @title A PHGMethod Class
#'
#' @description 
#' Class \code{PHGMethod} defines a \code{rPHG} Class for storing 
#' a "committed" PHG method to return data against.
#'
#' @slot methodID A PHG method identifier.
#' @slot phgConObj A \code{\linkS4class{PHGCon}} object
#'
#' @name PHGMethod-class
#' @rdname PHGMethod-class
#' @exportClass PHGMethod
setClass(
    Class = "PHGMethod",
    slots = c(
        methodID  = "character",
        phgConObj = "PHGCon"
    ),
    prototype = list(
        methodID = "test",
        phgConObj = new("PHGCon", phgType = "local", host = "localHost")
    )
)


## ----
#' @title Helper function to construct PHGMethod object
#'
#' @description 
#' Creates a \code{\linkS4class{PHGMethod}} object to be used to read and
#' filter data from a given PHG connection object using a verified PHG method.
#'
#' @param phgConObj A \code{\linkS4class{PHGCon}} object.
#' @param methodID A PHG method identifier.
#'
#' @export
PHGMethod <- function(phgConObj, methodID) {

    # # For demo purposes only! (useful for workshops)
    # if (methodID == "DEMO") methodID <- "NAM_GBS_Alignments_PATHS"
    
    methodIDs <- showPHGMethods(phgConObj)$method_name
    if (!methodID %in% methodIDs) {
        stop("Method ID not found in database", call. = FALSE)
    }

    methods::new(
        Class     = "PHGMethod",
        methodID  = methodID,
        phgConObj = phgConObj
    )
}



# /// Methods (show) ////////////////////////////////////////////////

## ----
#' @title Show method for PHGMethod objects
#'
#' @description
#' Prints out information regarding properties from the \code{PHGMethod}
#' class to the console
#'
#' @param object a \code{\linkS4class{PHGMethod}} object.
#'
#' @docType methods
#' @name show
#' @rdname show
#' @aliases show,PHGMethod-method
setMethod(
    f = "show",
    signature = "PHGMethod",
    definition = function(object) {
        conType <- phgType(phgConObj(object))
        
        conMsg <- switch (conType,
            "server" = cli::style_bold(cli::col_green("PHGServerCon")),
            "local"  = cli::style_bold(cli::col_green("PHGLocalCon"))
        )
        
        methodId <- cli::style_bold(cli::col_blue(phgMethod(object)))
        
        msg <- c(
            paste0("A ", cli::style_bold("PHGMethod"), " promise object:"),
            paste0("  <", conMsg, "> --- <", methodId, ">")
        )
        
        cat(msg, sep = "\n")
    }
)



# /// Methods (general) /////////////////////////////////////////////

## ----
#' @rdname phgConObj
#' @export
setMethod(
    f = "phgConObj",
    signature = signature(object = "PHGMethod"),
    definition = function(object) {
        return(object@phgConObj)
    }
)


## ----
#' @rdname phgMethod
#' @export
setMethod(
    f = "phgMethod",
    signature = signature(object = "PHGMethod"),
    definition = function(object) {
        return(object@methodID)
    }
)


## ----
#' @rdname readRefRanges
#' @export
setMethod(
    f = "readRefRanges",
    signature = signature(object = "PHGMethod"),
    definition = function(object) {
        conType <- object |> phgConObj() |> phgType()
        
        if (conType == "local") {
            cat("WIP for reading reference ranges from LOCAL connection\n")
        } else if (conType == "server") {
            cat("WIP for reading reference ranges from SERVER connection\n")
        }
    }
)



## ## ----
## #' @rdname readRefRanges
## #'
## #' @importFrom GenomicRanges GRanges
## #' @importFrom IRanges IRanges
## #' @importFrom rJava .jevalArray
## #' @importFrom rJava .jnew
## #'
## #' @export
## setMethod(
##     f = "readRefRanges",
##     signature = "BrapiConPHG",
##     definition = function(object) {
##         urls <- getVTList(object)
## 
##         pageSize <- ifelse(
##             grepl("variants$", urls$rangeURL),
##             "?pageSize=",
##             "&pageSize="
##         )
## 
##         if (object@methodID == "DEMO") {
##             rrDF <- parseJSON(paste0(urls$rangeURL, pageSize, "1000"))
##         } else {
##             rrDF <- parseJSON(paste0(urls$rangeURL, pageSize, "150000"))
##         }
##         rrDF <- rrDF$result$data
## 
##         gr <- GenomicRanges::GRanges(
##             seqnames = rrDF$referenceName,
##             ranges = IRanges::IRanges(
##                 start = rrDF$start,
##                 end = rrDF$end
##             ),
##             variantDbId = rrDF$variantDbId
##         )
## 
##         return(gr)
## 
##     }
## )
## 
## 
## ## ----
## #' @rdname readSamples
## #'
## #' @importFrom tibble as_tibble
## #'
## #' @export
## setMethod(
##     f = "readSamples",
##     signature = "BrapiConPHG",
##     definition = function(object) {
##         urls <- getVTList(object)
## 
##         sampleDF <- parseJSON(urls$sampleURL)
##         sampleDF <- sampleDF$result$data
## 
##         if (object@methodID == "DEMO") {
##             return(utils::head(tibble::as_tibble(sampleDF), n = 25))
##         } else{
##             return(tibble::as_tibble(sampleDF))
##         }
##     }
## )
## 
## 
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