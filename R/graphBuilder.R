#' @title Test PHG builder function
#'
#' @description R wrapper to build a PHG graph object for downstream use.
#'
#' @param configFile Configuration file for DB.
#' @param myMethods Pairs of method calls - passed as string.
#' @param myChrom List of chromosomes to include in graph. If NULL, defaults
#'   to all.
#' @param myIncludeSequence Whether to include sequences in haplotype nodes.
#'   (ADVANCED)
#' @param myIncludeVariant Whether to include variant contexts in haplotype
#'   nodes. (ADVANCED)
#'
#' @export
graphBuilder <- function(configFile,
                         myMethods,
                         myChrom = NULL,
                         myIncludeSequence = TRUE,
                         myIncludeVariant = FALSE) {

    ## Remove later
    print("A temporary placeholder")
}
