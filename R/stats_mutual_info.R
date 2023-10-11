## ----
# Calculate mutual information from a `PHGDataSet` object
#
# @param phgObj A `PHGDataSet` object
calcMutualInfoFromPHGDataSet <- function(phgObj) {
    phgHapIDMat     <- t(SummarizedExperiment::assay(phgObj))
    refRanges       <- colnames(phgHapIDMat)
    nRanges         <- length(refRanges)
    miMat           <- matrix(NA, nrow = nRanges, ncol = nRanges)
    rownames(miMat) <- refRanges
    colnames(miMat) <- refRanges

    for (range1 in seq_len(nRanges - 1)) {
        for (range2 in (range1 + 1):nRanges) {
            miMat[range1, range2] <- mutualInfoPair(
                phgHapIDMat,
                c(refRanges[range1], refRanges[range2])
            )
        }
    }

    return(miMat)
}


