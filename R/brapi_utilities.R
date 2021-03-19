# === BrAPI utility and house-keeping methods =======================

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
#' @importFrom httr content
#' @importFrom httr GET
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
json2tibble <- function(object, ep) {
    endPoint <- paste0(brapiURL(object), "/", ep)
    res <- jsonlite::fromJSON(httr::content(httr::GET(endPoint), "text"))
    return(tibble::as_tibble(res$result$data))
}


#' @title Parse graph data
#'
#' @description Parses graph information from JSON structures
#'
#' @param object A \code{BrapiCon} object.
#' @param dbID A PHG method.
#'
#' @importFrom httr content
json2igraph <- function(object, dbID = NULL) {

    if (missing(dbID)) stop("PHG method required", call. = FALSE)
    endPoint <- paste0(brapiURL(object), "/graphs/", dbID)
    res <- jsonlite::fromJSON(httr::content(httr::GET(endPoint), "text"))

    nodes <- res$result$nodes
    edges <- res$result$edges
    edges <- data.frame(
        from = edges$leftNodeDbId,
        to = edges$rightNodeDbId,
        weight = edges$weight
    )
    nodes <- data.frame(
        id = nodes$nodeDbId,
        label = nodes$additionalInfo$taxaList %>%
            lapply(paste, collapse = "; ") %>%
            unlist()
    )
    igraph::graph_from_data_frame(
        d = edges,
        vertices = nodes,
        directed = TRUE
    )
}

