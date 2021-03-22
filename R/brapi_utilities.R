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
#'
#' @return A \code{tibble} object.
#'
#' @importFrom tibble as_tibble
json2tibble <- function(object, ep) {
    endPoint <- paste0(brapiURL(object), "/", ep)
    endPoint <- parseJSON(endPoint)

    if (is.null(endPoint[["result"]][["data"]])) {
        return(tibble::as_tibble(endPoint))
    } else {
        return(tibble::as_tibble(endPoint[["result"]][["data"]]))
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


