#' @title Generate a path matrix
#'
#' @description Generates a matrix of paths from a directory path of
#'   \code{path.txt} files
#'
#' @author Brandon Monier
#' @author Peter Bradbury
#'
#' @param configFile Path to a configuration file for your graph database.
#' @param pathDir A directory to path files.
#'
#' @importFrom rJava .jarray
#' @importFrom rJava is.jnull
#' @importFrom rJava J
#'
#' @export
pathMatrix <- function(configFile, pathDir) {

    pathFiles <- list.files(pathDir, full.names = TRUE)

    jfiles <- rJava::.jarray(pathFiles)
    pathHapids <- rJava::J(
        "net.maizegenetics.pangenome.api/RMethods",
        "pathHapids",
        configFile,
        jfiles
    )
    pathMatrix <- pathHapids$matrix
    if(!rJava::is.jnull(pathHapids$rowNames)) {
        rownames(pathMatrix) <- pathHapids$rowNames
    }
    if(!rJava::is.jnull(pathHapids$columnNames)) {
        colnames(pathMatrix) <- pathHapids$columnNames
    }
    return(pathMatrix)
}
