## ----
#' @title Get taxa data for selected reference ranges
#'
#' @description
#' Base code to get assembly information from haplotype node objects
#'
#' @param phgObj An object of class \code{PHGDataSet}.
#' @param start Start position (bp) for reference range filtering.
#' @param end End position (bp) for reference range filtering.
#' @param seqnames Sequence name (e.g. chromosome ID) for reference range
#'    filtering.
#' @param rrSet A collection of reference range IDs. Defaults to \code{NULL}
#'    if specified with an integer vector, \code{start}, \code{end}, and
#'    \code{seqnames} parameters will be ignored.
#'
#' @importFrom IRanges IRanges
#' @importFrom IRanges subsetByOverlaps
#' @importFrom GenomicRanges GRanges
#' @importFrom rJava J
#' @importFrom rJava .jnew
#' @importFrom rJava .jcall
#' @importFrom S4Vectors metadata
#' @importFrom SummarizedExperiment rowRanges
#'
#' @export
taxaByNode <- function(
    phgObj,
    start = NULL,
    end = NULL,
    seqnames = NULL,
    rrSet = NULL
) {
    # Get valid ref ranges from PHGDataSet
    if (is.null(rrSet)) {
        if (is.null(start)) {
            stop("Genomic range parameters are needed")
        }
        if (is.null(end)) {
            stop("Genomic range parameters are needed")
        }
        if (is.null(seqnames)) {
            stop("Genomic range parameters are needed")
        }
        q <- GenomicRanges::GRanges(
            seqnames = seqnames,
            ranges = IRanges::IRanges(
                start = start,
                end = end
            )
        )

        rrSet <- gsub(
            pattern = "R",
            replacement = "",
            x = IRanges::subsetByOverlaps(
                SummarizedExperiment::rowRanges(phgObj),
                q
            )$refRange_id
        )
    }

    jGObj <- S4Vectors::metadata(phgObj)$jObj
    taxaBNDriver <- rJava::.jnew(
        rJava::J("net.maizegenetics.pangenome.utils.TaxaByNodeByRangePlugin"),
        rJava::.jnull("java/awt/Frame"),
        FALSE
    )

    dataSet <- rJava::J("net.maizegenetics.plugindef.DataSet")

    sortSet <- .jnew("java.util.TreeSet")
    for (rr in rrSet) {
        sortSet$add(.jnew("java.lang.Integer", as.integer(rr)))
    }

    taxaBNDriver$rangeIds(sortSet)
    res <- taxaBNDriver$performFunction(dataSet$getDataSet(jGObj))$
        getData(0L)$
        getData()

    rrIds <- .jcall(res$keySet(), "[Ljava/lang/Object;", "toArray") |>
        lapply(function(i) i$toString()) |>
        unlist()

    rootValArr <- .jcall(res$values(), "[Ljava/lang/Object;", "toArray")
    assemblies <- lapply(rootValArr, function(i) {
        tmp <- .jcall(i$values(), "[Ljava/lang/Object;", "toArray")
        lapply(tmp, function(j) {
            j$toArray() |>
                lapply(function(k) k$toString()) |>
                unlist()
        })
    })
    nodeIds <- lapply(rootValArr, function(i) {
        i$keySet() |>
            .jcall("[Ljava/lang/Object;", "toArray") |>
            lapply(function(j) j$toString()) |>
            unlist()
    })

    for (i in seq_along(assemblies)) {
        names(assemblies[[i]]) <- nodeIds[[i]]
    }

    names(assemblies) <- rrIds

    return(tnHashMapToTibble(assemblies))
}


