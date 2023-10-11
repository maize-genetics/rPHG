## ----
# Calculate the mutual information across a pair of reference ranges
# I(X;Y) = Sum p(x, y)log{p(x, y) / [p(x)p(y)]}
#
# NOTE: above equation is from:
#   * Shannon and Weaver (1949)
#   * Cover and Thomas (1991)
# NOTE: hap IDs are treated as categorical data (model.matrix)
#
# @param phgHapIDMat A haplotype ID matrix
# @param twoRanges A vector of length 2 containg two ref range elements
mutualInfoPair <- function(phgHapIDMat, twoRanges) {
    hapID <- phgHapIDMat[, twoRanges]

    # Remove any rows that have missing data
    completePHGCases <- function(x) {
        any(x < 1) && all(!is.na(x))
    }
    hapID <- hapID[!apply(hapID, 1, completePHGCases), ]

    # Check if any columns have only one haplotype
    anyUnique <- apply(hapID, 2, function(x) length(unique(x)) == 1)
    if (any(anyUnique)) {
        return(0)
    }

    # Calculate mutual info
    hapID <- apply(hapID, 2, as.character)
    nHap1 <- length(unique(hapID[, 1]))
    nHap2 <- length(unique(hapID[, 2]))

    # Sum p(x, y)
    mmi <- matrix(
        data = colMeans(stats::model.matrix( ~ -1 + hapID[, 1]:hapID[, 2])),
        nrow = nHap1,
        ncol = nHap2
    )

    # p(x)p(y)
    mm1 <- colMeans(stats::model.matrix( ~ -1 + hapID[, 1]))
    mm2 <- colMeans(stats::model.matrix( ~ -1 + hapID[, 2]))
    mmm <- tcrossprod(mm1, mm2)

    # Sum p(x, y) log{p(x, y) / [p(x)p(y)]}
    # Some of these will be `NaN` (removed by `na.rm = TRUE`)
    mi  <- mmi * log2(mmi / mmm)
    return(sum(mi, na.rm = TRUE))
}


