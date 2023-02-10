#' @title Generate a reference range table
#'
#' @description Generates a reference range table from a PHG object.
#'
#' @author Brandon Monier
#' @author Peter Bradbury
#'
#' @param phgObject A PHG object.
#'
#' @importFrom rJava is.jnull
#' @importFrom rJava J
#' @importFrom tibble tibble
#'
#' @export
refRangeTable <- function(phgObject) {

    ## Get reference range object from PHG object
    refRangeObj <- rJava::J(
        "net.maizegenetics.pangenome.api/RMethods",
        "referenceRanges",
        phgObject
    )

    ## Get data vectors and convert to tibble
    refranges <- data.frame(
        lapply(X = seq_along(refRangeObj$columnNames) - 1, function(i) {
            refRangeObj$dataVectors$get(as.integer(i))
        })
    )
    names(refranges) <- refRangeObj$columnNames
    refranges <- tibble::as_tibble(refranges)

    ## Return the tibble refrange object
    return(refranges)
}
