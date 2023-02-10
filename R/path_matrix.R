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

    configCatcher(configFile)

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



#' @title Generate a matrix for all the paths for \code{pathMethod}
#'
#' @description Returns a \code{matrix} object of haplotype ids with taxa name
#'   for row names and reference range id for the column name.
#'
#' @author Brandon Monier
#' @author Peter Bradbury
#'
#' @param configFile Path to a configuration file for your graph database.
#' @param pathMethod The name of the path method in the PHG DB
#'
#' @importFrom rJava J
#'
#' @export
pathsForMethod <- function(configFile, pathMethod) {

    configCatcher(configFile)

    # Retrieve Java matrix object
    pathObj <- rJava::J(
        "net.maizegenetics.pangenome.api/RMethods",
        "pathsForMethod",
        configFile,
        pathMethod
    )

    # Configure for R
    pathMat <- pathObj$matrix
    rownames(pathMat) <- pathObj$rowNames
    colnames(pathMat) <- pathObj$columnNames

    # Return
    return(pathMat)
}



#' @title Retrieve read mapping information from PHG database.
#'
#' @description Returns an \code{S4Vectors} \code{DataFrame} object of read
#'   mapping information for a given line (i.e. taxon).
#'
#' @author Brandon Monier
#' @author Peter Bradbury
#'
#' @param configFile Path to a configuration file for your graph database.
#' @param lineName The name of the line (taxon) for which the read mapping
#'   information is to be retrieved. If there are multiple read mappings with
#'   different \code{file_group_names}, they will be combined.
#' @param readMappingMethodName The method name for the read mappings
#'   (only takes a single method).
#' @param haplotypeMethodName The haplotype method name.
#' @param fileGroup the name of the file group for the line from the database.
#'   This parameter is only necessary if the line (taxon) has more than one
#'   file group and only the reads for a specific file group are wanted.
#'
#' @importFrom rJava J
#' @importFrom S4Vectors DataFrame
#'
#' @export
readMappingsForLineName <- function(configFile,
                                    lineName,
                                    readMappingMethodName,
                                    haplotypeMethodName,
                                    fileGroup = NULL) {

    configCatcher(configFile)

    # Retrieve Java data vector object(s)
    rmObj <- rJava::J(
        "net.maizegenetics.pangenome.api/RMethods",
        "readMappingsForLineName",
        configFile,
        lineName,
        readMappingMethodName,
        haplotypeMethodName,
        fileGroup
    )

    # Configure for R
    colNum <- rmObj$dataVectors$size()
    rmDF <- lapply(seq_len(colNum), function(i) {
        rmObj$dataVectors$get(as.integer(i - 1))
    })
    rmDF <- data.frame(rmDF)
    colnames(rmDF) <- rmObj$columnNames

    # Return
    return(S4Vectors::DataFrame(rmDF))
}



#' @title Retrieve read mapping records from PHG database.
#'
#' @description Returns an \code{S4Vectors} \code{DataFrame} object of read
#'   mapping record information without \code{read_mapping} data.
#'
#' @author Brandon Monier
#' @author Peter Bradbury
#'
#' @param configFile Path to a configuration file for your graph database.
#'
#' @importFrom rJava J
#' @importFrom S4Vectors DataFrame
#'
#' @export
readMappingTableInfo <- function(configFile) {

    # Catch potential errors
    configCatcher(configFile)

    # Retrieve Java data vector object(s)
    rmObj <- rJava::J(
        "net.maizegenetics.pangenome.api/RMethods",
        "readMappingTableInfo",
        configFile
    )

    # Configure for R
    colNum <- rmObj$dataVectors$size()
    rmDF <- lapply(seq_len(colNum), function(i) {
        rmObj$dataVectors$get(as.integer(i - 1))
    })
    rmDF <- data.frame(rmDF)
    colnames(rmDF) <- rmObj$columnNames

    # Return
    return(S4Vectors::DataFrame(rmDF))
}


