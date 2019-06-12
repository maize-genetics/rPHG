#' @title Generate a haplotype ID matrix
#'
#' @description Generates a haplotype ID matrix from a PHG object.
#'
#' @author Brandon Monier
#' @author Peter Bradbury
#'
#' @param phgObject A PHG object.
#'
#' @importFrom rJava is.jnull
#' @importFrom rJava J
#'
#' @export
hapIDMatrix <- function(phgObject) {

    ## Pull hap ID matrix from phg object
    # J("net.maizegenetics.pangenome.api/RMethods", "hapidTableAsMatrix", phgObject)
    hapids <- rJava::J(
        "net.maizegenetics.pangenome.api/RMethods",
        "hapidTableAsMatrix",
        phgObject
    )
    hapidMatrix <- hapids$matrix

    ## Get row and column names (if available)
    if(!rJava::is.jnull(hapids$rowNames)) {
        rownames(hapidMatrix) <- hapids$rowNames
    }
    if(!rJava::is.jnull(hapids$columnNames)) {
        colnames(hapidMatrix) <- hapids$columnNames
    }

    ## Return the matrix
    return(hapidMatrix)
}
