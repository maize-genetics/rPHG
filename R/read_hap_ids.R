## ----
# Get reference ranges from local connection
#
# @param conObj A PHG connection object
# @param conMethod A PHG database method ID
hapIdsFromLocal <- function(conObj, conMethod) {
    pathsForMethod(
        configFilePath(conObj),
        conMethod
    )
}


## ----
# Get reference ranges from server connection
#
# @param conObj A PHG connection object
# @param conMethod A PHG database method ID
hapIdsFromSever <- function(conObj, conMethod) {
    print("WIP for `hapIdsFromServer`")
}


## ----
# Return Hap ID matrix from `HaplotypeGraph` objects
#
# @param phgObj A PHG `HaplotypeGraph` object
hapIdsFromGraphObj <- function(phgObject) {

    # Pull hap ID matrix from phg object
    hapids <- rJava::J(
        TASSEL_API$R_METHODS,
        "hapidTableAsMatrix",
        phgObject
    )
    hapidMatrix <- hapids$matrix

    # Get row and column names (if available)
    if(!rJava::is.jnull(hapids$rowNames)) {
        rownames(hapidMatrix) <- hapids$rowNames
    }
    if(!rJava::is.jnull(hapids$columnNames)) {
        colnames(hapidMatrix) <- hapids$columnNames
    }

    # Return the matrix
    return(hapidMatrix)
}


