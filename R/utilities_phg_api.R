## ----
# Build graph object from haplotype method(s)
#
# @param configFile Path to a config file
# @param method A path method string
graphFromHaplotypes <- function(
    configFile,
    method,
    chrom,
    includeSequence,
    includeVariants
) {

    gbPlugin <- rJava::new(
        rJava::J(TASSEL_API$HAPLOTYPE_GRAPH_BUILDER),
        rJava::.jnull(TASSEL_API$FRAME),
        FALSE
    )
    gbPlugin$configFile(toString(configFile))
    gbPlugin$methods(toString(method))

    # Add chromosome as vector
    if (!is.null(chrom)) {
        rv <- rJava::.jnew(TASSEL_API$VECTOR)
        for (i in seq(chrom)) rv$add(chrom[i])
        gbPlugin$chromosomes(rv)
    } else {
        gbPlugin$chromosomes(chrom)
    }

    # Set sequence and variant return (ADVANCED)
    gbPlugin$setParameter("includeSequences", toString(includeSequence))
    gbPlugin$setParameter("includeVariantContexts", toString(includeVariants))

    graphObj <- gbPlugin$build()

    return(graphObj)
}


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


