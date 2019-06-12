#' @title Test PHG builder function
#'
#' @description R wrapper to build a PHG graph object for downstream use.
#'
#' @author Brandon Monier
#' @author Peter Bradbury
#'
#' @param configFile Configuration file for graph database.
#' @param myMethods Pairs of method calls - passed as string.
#' @param myChrom List of chromosomes to include in graph. If NULL, defaults
#'   to all.
#' @param myIncludeSequence Whether to include sequences in haplotype nodes.
#'   (ADVANCED)
#' @param myIncludeVariant Whether to include variant contexts in haplotype
#'   nodes. (ADVANCED)
#'
#' @importFrom rJava .jnull
#' @importFrom rJava J
#' @importFrom rJava new
#'
#' @export
graphBuilder <- function(configFile,
                         myMethods,
                         myChrom = NULL,
                         myIncludeSequence = FALSE,
                         myIncludeVariant = FALSE) {

    ## Create PHG plugin object
    phgPlugin <- rJava::new(
        rJava::J("net.maizegenetics.pangenome.api.HaplotypeGraphBuilderPlugin"),
        rJava::.jnull(),
        FALSE
    )

    ## Set parameters
    phgPlugin$setParameter("configFile", configFile)
    phgPlugin$setParameter("methods", myMethods)
    phgPlugin$setParameter("includeSequences", toString(myIncludeSequence))
    phgPlugin$setParameter("includeVariantContexts", toString(myIncludeVariant))
    phgPlugin$setParameter("chromosomes", myChrom)

    ## Build the PHG...
    message("Building the graph...")
    phgPlugin$build()

    ## Return PHG object
    message("Finished!")
    return(phgPlugin)
}
