# === BrAPI utility and house-keeping methods =======================

#' @title URL checker
#'
#' @description Checks and parses URL inputs to list data from JSON text.
#'
#' @param url A BrAPI URL endpoint.
#' @param verbose Do you want messages shown?
#'
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
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

    if (is.null(res)) {
        stop("BrAPI endpoint could not be parsed.", call. = FALSE)
    } else {
        return(res)
    }
}


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

    if (is.null(endPoint[["result"]][[returnCall]])) {
        return(tibble::as_tibble(endPoint))
    } else {
        return(tibble::as_tibble(endPoint[["result"]][[returnCall]]))
    }
}


#' @title Parse graph data
#'
#' @description Parses graph information from JSON structures
#'
#' @param object A \code{BrapiCon} object.
#' @param dbID A PHG method.
#'
#' @importFrom httr content
#' @importFrom igraph graph_from_data_frame
json2igraph <- function(object, dbID) {
    if (missing(dbID)) stop("PHG method required", call. = FALSE)

    endPoint <- paste0(brapiURL(object), "/graphs/", dbID)
    res <- parseJSON(endPoint)

    nodes <- res$result$nodes
    edges <- res$result$edges
    taxaList <- nodes$additionalInfo$taxaList
    taxaList <- unlist(lapply(taxaList, paste, collapse = "; "))

    edges <- data.frame(
        from = edges$leftNodeDbId,
        to = edges$rightNodeDbId,
        weight = edges$weight
    )
    nodes <- data.frame(
        id = nodes$nodeDbId,
        label = taxaList
    )

    igraph::graph_from_data_frame(
        d = edges,
        vertices = nodes,
        directed = TRUE
    )
}


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

    baseURL <- paste0(x@url, "/variantTables/", x@methodID)

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

    tableURL <- paste0(
        baseURL, "/table", "?",
        ifelse(is.na(ranges), "", paste0(ranges)), "&",
        ifelse(is.na(samples), "", paste0(samples))
    )
    tableURL <- gsub("\\?$|\\?&$", "", tableURL)
    tableURL <- gsub("\\?&", "?", tableURL)

    return(
        list(
            rangeURL = rangeURL,
            sampleURL = sampleURL,
            tableURL = tableURL
        )
    )
}


