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
    return(tibble::as_tibble(res))
}


#' @title Parse graph data
#'
#' @description Parses graph information from JSON structures
#'
#' @param object A \code{BrapiCon} object.
#' @param dbID A PHG method.
#'
#' @importFrom httr content


