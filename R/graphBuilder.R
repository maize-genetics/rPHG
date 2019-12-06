#' @title Test PHG builder function
#'
#' @description R wrapper to build a PHG graph object for downstream use.
#'
#' @author Brandon Monier
#' @author Peter Bradbury
#'
#' @param configFile Path to a configuration file for your graph database.
#' @param methods Pairs of method calls - passed as string.
#' @param chrom A vector of chromosomes to include in graph. If NULL, defaults
#'   to all. To specify multiple chromosome, pass as a vector of strings (i.e.
#'   \code{c("1", "2", "3")}).
#' @param includeSequence Whether to include sequences in haplotype nodes.
#'   (ADVANCED)
#' @param includeVariant Whether to include variant contexts in haplotype
#'   nodes. (ADVANCED)
#'
#' @importFrom methods new
#' @importFrom rJava .jnew
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

    ### Add chromosome as vector
    if (!is.null(chrom)) {
        rv <- rJava::.jnew("java/util/Vector")
        for (i in seq(chrom)) rv$add(chrom[i])
        phgPlugin$chromosomes(rv)
    } else {
        phgPlugin$chromosomes(chrom)
    }

    ### ADVANCED
    phgPlugin$setParameter("includeSequences", toString(includeSequence))
    phgPlugin$setParameter("includeVariantContexts", toString(includeVariant))


    ## Build the PHG...
    message("Building the graph...")
    phgObj <- phgPlugin$build()
    phgObj <- sumExpBuilder(phgObj = phgObj)
    phgObj <- methods::new(Class = "PHGDataSet", phgObj)

    ## Return PHG object
    message("Finished!")
    return(phgObj)
}


#' @importFrom GenomicRanges GRanges
#' @importFrom IRanges IRanges
#' @importFrom S4Vectors metadata
#' @importFrom SummarizedExperiment SummarizedExperiment
sumExpBuilder <- function(phgObj) {
    hapIDMat <- hapIDMatrix(phgObject = phgObj)
    phgRefRange <- refRangeTable(phgObject = phgObj)

    rr <- GenomicRanges::GRanges(
        seqnames = phgRefRange$chr,
        ranges = IRanges::IRanges(
            start = phgRefRange$start,
            end = phgRefRange$end
        ),
        refRange_id = phgRefRange$id
    )

    phgSE <- SummarizedExperiment::SummarizedExperiment(
        assays = list(hapID = t(hapIDMat)),
        rowRanges = rr
    )
    S4Vectors::metadata(phgSE)$jObj <- phgObj

    return(phgSE)
}


