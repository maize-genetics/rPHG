## ----
# Build "alle matrix" URL strings
#
# @description
# Builds URL strings for "alleleMatrix" (i.e. path method table) BrAPI
# endpoints
#
# @param methodId
# Method ID for given path/graph in PHG
# @rrPageSize
# Max allowed number of ref ranges for a given web page
# @rrPage
# Current page for ref range page collection
# @samplePageSize
# Max allowed number of samples for a given web page
# @samplePage
# Current page for samples page collection
amUrlContextStringBuilder <- function(
    methodId,
    rrPageSize,
    rrPage,
    samplePageSize,
    samplePage
) {
    amContextString <- sprintf(
        paste0(
            BRAPI_ENDPOINTS$METHOD_TABLE,    # allelematrix
            BRAPI_PARAMS$REST_QUERY,         # ?
            BRAPI_PARAMS$METHOD_ID_KEY,      # variantSetDbId=
            BRAPI_PARAMS$REST_KV_SEP,
            BRAPI_PARAMS$METHOD_RR_SIZE,
            BRAPI_PARAMS$REST_KV_SEP,
            BRAPI_PARAMS$METHOD_SAMPLE_SIZE,
            BRAPI_PARAMS$REST_KV_SEP,
            BRAPI_PARAMS$METHOD_RR_PAGE,
            BRAPI_PARAMS$REST_KV_SEP,
            BRAPI_PARAMS$METHOD_SAMPLE_PAGE
        ),
        methodId, rrPageSize, samplePageSize, rrPage, samplePage
    )

    return(amContextString)
}


## ----
# Check if BrAPI `serverinfo` endpoint exists
#
# @description
# Checks if BrAPI compliant `serverinfo` endpoint can be reached. This
# presumption will imply that we can at least connect to this "mandatory"
# endpoint for the PHG Ktor server.
#
# @param url Host URL for PHG server
# @param endpoint What endpoint to append to URL
brapiEndpointExists <- function(url, endpoint = BRAPI_ENDPOINTS$SERVER_INFO) {
    # Check specified BrAPI endpoint
    status <- tryCatch(
        expr = {
            httr::GET(file.path(url, endpoint))$status
        },
        error = function(cond) NA
    )

    # NOTE: test currently negates `httResp` check for all status codes. Will
    #       keep in codebase for possible future debugging tests
    ifelse(
        test = !is.na(status) && status >= 200 && status <= 299,
        yes  = return(TRUE),
        no   = return(FALSE)
    )
}


## ----
# Get HTTP response status codes from PHG server
#
# @description
# By default, this will ping the `serverinfo` BrAPI endpoint on the server.
# NOTE: `url` needs `brapi/v2` or `brapi/v1` suffix.
#
# @param url Host URL for PHG server
# @param endpoint What endpoint to append to URL? Can be `""` for non BrAPI
#                 tests.
httpResp <- function(url, endpoint = BRAPI_ENDPOINTS$SERVER_INFO) {

    status <- httr::GET(file.path(url, endpoint))$status

    statusMsg <- switch(
        EXPR = floor(status / 100),
        `1`  = cli::col_yellow("Information"),
        `2`  = cli::col_green("OK"),
        `3`  = cli::col_blue("Redirection"),
        `4`  = cli::col_red("Client Error"),
        `5`  = cli::col_red("Server Error")
    )

    return(list(status = status, msg = cli::style_bold(statusMsg)))
}


## ----
# Parse JSON response to native R object
#
#
# @param url A BrAPI URL endpoint.
# @param verbose Do you want messages shown?
parseJSON <- function(url, verbose = FALSE) {
    res <- tryCatch(
        expr = {
            if (verbose) message("Attempting to read endpoint...")
            x <- httr::GET(url)
            x <- httr::content(x, as = "text", encoding = "ISO-8859-1")
            x <- jsonlite::fromJSON(x)
            return(x)
        },
        error = function(cond) {
            if (verbose) message("URL could not be processed; see below: ")
            if (verbose) message(cond, "\n")
            return(NULL)
        }
    )

    return(res)
}


## ----
#' @title JSON to tibble converter
#'
#' @description Converts a requested JSON object to a \code{tibble}-based
#'   \code{data.frame} object.
#'
#' @param object A \code{BrapiCon} object.
#' @param ep A specified endpoint to request.
#' @param returnCall What JSON section should be returned? Defaults to
#'   \code{data}.
#'
#' @return A \code{tibble} object.
#'
#' @importFrom tibble as_tibble
json2tibble <- function(object, ep, returnCall = "data") {
    endPoint <- paste0(brapiURL(object), "/", ep)
    endPoint <- parseJSON(endPoint)

    # This will most likely always be "data" in BrAPI spec...
    return(tibble::as_tibble(endPoint[["result"]][[returnCall]]))

    # if (is.null(endPoint[["result"]][[returnCall]])) {
    #     return(tibble::as_tibble(endPoint))
    # } else {
    #     return(tibble::as_tibble(endPoint[["result"]][[returnCall]]))
    # }
}


## ----
#' @title Retrieve variant table BrAPI URLs
#'
#' @description Returns a list of three BrAPI endpoints: (1) sample, (2)
#'   variants (i.e. reference ranges), and (3) table info.
#'
#' @param x A \code{BrapiConPHG} object.
#' @export
getVTList <- function(x) {
    if (class(x) != "BrapiConPHG") {
        stop("A `BrapiConPHG` object is needed for the LHS argument", call. = FALSE)
    }

    if (x@methodID == "DEMO") {
        baseURL <- paste0(x@url, "/variantTables/", "282_GBS_Alignments_PATHS")
    } else {
        baseURL <- paste0(x@url, "/variantTables/", x@methodID)
    }

    ranges <- x@refRangeFilter
    samples <- x@sampleFilter

    rangeURL <- paste0(
        baseURL,
        "/variants",
        ifelse(is.na(ranges), "", paste0("?", ranges))
    )

    sampleURL <- paste0(
        baseURL,
        "/samples",
        ifelse(is.na(samples), "", paste0("?", samples))
    )

    # tableURL <- paste0(
    #     baseURL, "/table", "?",
    #     ifelse(is.na(ranges), "", paste0(ranges)), "&",
    #     ifelse(is.na(samples), "", paste0(samples))
    # )
    # tableURL <- gsub("\\?$|\\?&$", "", tableURL)
    # tableURL <- gsub("\\?&", "?", tableURL)

    if (x@methodID == "DEMO") {
        tableURL <- paste0(
            brapiURL(x),
            "/allelematrix",
            "?variantSetDbId=", "282_GBS_Alignments_PATHS",
            "&dimensionVariantPageSize=250", # currently hardcoded
            "&dimensionCallSetPageSize=25", # currently hardcoded
            "&dimensionVariantPage=%i",
            "&dimensionCallSetPage=%i"
        )
    } else {
        tableURL <- paste0(
            brapiURL(x),
            "/allelematrix",
            "?variantSetDbId=", x@methodID,
            "&dimensionVariantPageSize=10000", # currently hardcoded
            "&dimensionCallSetPageSize=5000", # currently hardcoded
            "&dimensionVariantPage=%i",
            "&dimensionCallSetPage=%i"
        )
    }


    return(
        list(
            rangeURL = rangeURL,
            sampleURL = sampleURL,
            tableURL = tableURL
        )
    )
}


