## ----
# Build graph object from path method(s)
#
# @param configFile Path to a config file
# @param method A path method string
graphFromPaths <- function(configFile, method) {
    gbPlugin <- rJava::new(
        rJava::J(TASSEL_API$BUILD_GRAPH_FROM_PATHS)
    )
    rJava::J(TASSEL_API$PARAMETER_CACHE)$load(
        toString(configFile)
    )
    gbPlugin$pathMethod(toString(method))
    
    graphObj <- gbPlugin$build()
    
    return(graphObj)
}


## ----
# Get reference range data from graph objects
#
# @param phgObj A PHG `HaplotypeGraph` object
refRangesFromGraphObj <- function(phgObj) {
    # Get reference range object from PHG object
    refRangeObj <- rJava::J(
        TASSEL_API$R_METHODS,
        "referenceRanges",
        phgObj
    )
    
    # Get data vectors and convert to tibble
    refranges <- data.frame(
        lapply(
            X   = seq_along(refRangeObj$columnNames) - 1, 
            FUN = function(i) {
                refRangeObj$dataVectors$get(as.integer(i))
            }
        )
    )
    names(refranges) <- refRangeObj$columnNames
    
    return(refranges)
}


## ----
# Get hap ID matrix for a given path method
# 
# @param configFile Path to a config file
# @param method A path method string
pathsForMethod <- function(configFile, method) {
    
    # Retrieve Java matrix object
    pathObj <- rJava::J(
        TASSEL_API$R_METHODS,
        "pathsForMethod",
        configFile,
        method
    )
    
    # Configure for R
    pathMat <- pathObj$matrix
    rownames(pathMat) <- pathObj$rowNames
    colnames(pathMat) <- paste0("R", pathObj$columnNames)
    
    # Return
    return(pathMat)
}


