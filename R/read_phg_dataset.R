## ----
# Get PHGDataSet from local connection
#
# @param conObj A PHG connection object
# @param conMethod A PHG database method ID
# @param verbose Show console log info
phgDataSetFromLocal <- function(conObj, conMethod, verbose) {
    bullet <- cli::col_grey(cli::symbol$info)
    verbInfo <- c(
        paste0(bullet, " Getting reference range data..."),
        paste0(bullet, " Getting haplotype matrix data..."),
        paste0(bullet, " Constructing PHGDataSet...")
    )

    if (verbose) message(verbInfo[1])
    gr <- refRangesFromLocal(conObj, conMethod)

    if (verbose) message(verbInfo[2])
    hm <- hapIdsFromLocal(conObj, conMethod)

    if (verbose) message(verbInfo[3])
    phgSE <- SummarizedExperiment::SummarizedExperiment(
        assays    = list(pathMatrix = t(hm)),
        rowRanges = gr
    )

    return(methods::new(Class = "PHGDataSet", phgSE))
}


## ----
# Get PHGDataSet from server connection
#
# @param conObj A PHG connection object
# @param conMethod A PHG database method ID
# @param verbose Show console log info
phgDataSetFromServer <- function(conObj, conMethod, conDemo, verbose) {
    bullet <- cli::col_red(cli::symbol$warning)
    verbInfo <- c(
        paste0(bullet, cli::style_bold(" (WIP)"), " Getting reference range data..."),
        paste0(bullet, cli::style_bold(" (WIP)"), " Getting haplotype matrix data..."),
        paste0(bullet, cli::style_bold(" (WIP)"), " Constructing PHGDataSet...")
    )

    if (verbose) message(verbInfo[1])
    gr <- refRangesFromServer(conObj, conMethod, conDemo)

    if (verbose) message(verbInfo[2])
    hm <- hapIdsFromSever(conObj, conMethod, conDemo)

    if (verbose) message(verbInfo[3])
    phgSE <- SummarizedExperiment::SummarizedExperiment(
        assays    = list(pathMatrix = t(hm)),
        rowRanges = gr
    )

    return(methods::new(Class = "PHGDataSet", phgSE))
}


## ----
# Get PHGDataSet from `HaplotypeGraph` objects
#
# @param phgObj A PHG `HaplotypeGraph` object
phgDataSetFromGraphObj <- function(phgObj, verbose) {
    bullet <- cli::col_grey(cli::symbol$info)
    verbInfo <- c(
        paste0(bullet, " Getting reference range data..."),
        paste0(bullet, " Getting haplotype matrix data..."),
        paste0(bullet, " Constructing PHGDataSet...")
    )

    if (verbose) message(verbInfo[1])
    gr <- refRangesFromGraphObj(phgObj)

    if (verbose) message(verbInfo[2])
    hm <- hapIdsFromGraphObj(phgObj)

    if (verbose) message(verbInfo[3])
    phgSE <- SummarizedExperiment::SummarizedExperiment(
        assays    = list(pathMatrix = t(hm)),
        rowRanges = gr
    )

    return(methods::new(Class = "PHGDataSet", phgSE))
}


