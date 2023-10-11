## ----
# Return number of haplotypes per ref range from a `PHGDataSet` object
#
# @param phgObj A `PHGDataSet` object
nHaploPerRefRangeFromPHGDataSet <- function(phgObj) {

    # Get number of haplotypes and store as data frame
    nHap <- apply(
        X      = SummarizedExperiment::assay(phgObj),
        MARGIN = 1,
        FUN    = function(x) {
            length(unique(stats::na.omit(x)))
        }
    )
    nHapDf <- data.frame(
        rr_id = names(nHap),
        n_hap_ids = nHap
    )

    # Get reference range coordinates
    grDf <- as.data.frame(SummarizedExperiment::rowRanges(phgObj))

    # Combine both data frames and drop "strand" column (not relevant)
    xRet <- merge(grDf, nHapDf, by = "rr_id", sort = FALSE)
    colsToDrop <- "strand"

    return(tibble::as_tibble(xRet[, !names(xRet) %in% colsToDrop]))
}


