# === BrAPI general methods =========================================

## Display message ----
setMethod(
    f = "show",
    signature = "BrapiCon",
    definition = function(object) {

        status <- httr::GET(brapiURL(object))$status

        cat("A BrAPI connection object\n")
        cat("  Server...........:", host(object), "\n")
        cat("  Port.............:", port(object), "\n")
        cat("  Server status....:", status, "\n")
        cat("  BrAPI version....:", version(object), "\n")
    }
)


## Get samples ----
#' @export
setGeneric("samples", function(object) standardGeneric("samples"))

#' @export
setMethod(
    f = "samples",
    signature = "BrapiCon",
    definition = function(object) {
        surl <- paste0(brapiURL(object), "/samples")
        res <- jsonlite::fromJSON(httr::content(httr::GET(surl), "text"))
        return(tibble::as_tibble(res))
    }
)

