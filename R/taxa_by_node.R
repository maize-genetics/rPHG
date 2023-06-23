#' @description
#' Base code to get assembly information from haplotype node objects
#'
#' @param phgObj An object of class \code{PHGDataSet}.
#' @param rrSet A collection of reference
#'
#' @importFrom rJava J
#' @importFrom rJava .jnew
#'

taxaByNode <- function(phgObj, rrSet) {

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

    return(assemblies)
}


