#' @title Test PHG builder function
#'
#' @description R wrapper to build a PHG graph object for downstream use.
#'
#' @author Brandon Monier
#' @author Peter Bradbury
#'
#' @param configFile Path to a configuration file for your graph database.
#' @param methods Pairs of method calls - passed as string.
#' @param chrom List of chromosomes to include in graph. If NULL, defaults
#'   to all.
#' @param includeSequence Whether to include sequences in haplotype nodes.
#'   (ADVANCED)
#' @param includeVariant Whether to include variant contexts in haplotype
#'   nodes. (ADVANCED)
#'
#' @importFrom rJava .jnull
#' @importFrom rJava J
#' @importFrom rJava new
#'
#' @export
graphBuilder <- function(configFile,
                         methods,
                         chrom = NULL,
                         includeSequence = FALSE,
                         includeVariant = FALSE) {

    ## Create PHG plugin object
    phgPlugin <- rJava::new(
        rJava::J("net/maizegenetics/pangenome/api/HaplotypeGraphBuilderPlugin"),
        rJava::.jnull("java/awt/Frame"),
        FALSE
    )

    ## Set parameters
    phgPlugin$configFile(configFile)
    phgPlugin$methods(toString(methods))
    phgPlugin$chromosomes(chrom)
    # phgPlugin$includeSequences(toString(includeSequence))
    # phgPlugin$includeVariantContexts(toString(includeVariant))
    phgPlugin$setParameter("includeSequences", toString(includeSequence))
    phgPlugin$setParameter("includeVariantContexts", toString(includeVariant))
    # phgPlugin$setParameter("chromosomes", myChrom)

    ## Build the PHG...
    message("Building the graph...")
    phgObj <- phgPlugin$build()

    ## Return PHG object
    message("Finished!")
    return(phgObj)
}
