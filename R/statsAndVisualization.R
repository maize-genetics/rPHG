# === rPHG Stats Visualization Functions (WIP) ======================

#' @title Get the number of haplotypes per range in physical position segment
#'
#' @author Jean-Luc Jannink
#'
#' @param phgObject A PHG object.
#' @param chr What chromosome do you want to inspect? Defaults to \code{NULL}.
#'   If \code{NULL}, all chromsomes will be selected.
#' @param start Start position of chromosome. Defaults to \code{0}.
#' @param end End position of chromosome. Defaults to \code{NULL}. If
#'   \code{NULL}, the whole chromosome will be analyzed.
#'
#' @importFrom SummarizedExperiment as.data.frame
#' @importFrom SummarizedExperiment assays
#' @importFrom SummarizedExperiment ranges
#' @importFrom SummarizedExperiment rowRanges
#' @importFrom SummarizedExperiment seqnames
#'
#' @export
numHaploPerRange <- function(phgObject, chr = NULL, start = 0, end = NULL){
    # Get information about the reference ranges
    phgRefRange <- SummarizedExperiment::rowRanges(phgObject)

    rr <- SummarizedExperiment::ranges(phgRefRange)

    # Logic
    if (is.null(end)) {
        end <- max(end(rr))
    }

    allChr <- unique(SummarizedExperiment::seqnames(phgRefRange))
    allChr <- as.vector(allChr)
    if (is.null(chr)){
        chr <- allChr
    } else{
        if (!(chr %in% allChr)) stop("Chromosome is not in the PHG")
    }

    # Which reference ranges on the chromosome within start and end positions
    tmp <- as.numeric(SummarizedExperiment::seqnames(phgRefRange))
    keepRanges <- which(tmp %in% chr & start <= start(rr) & end(rr) <= end)

    if (length(keepRanges) == 0) {
        stop("There are no ranges with requested start and end")
    }

    # How many haplotypes are in those reference ranges
    phgHapIDMat <- t(SummarizedExperiment::assays(phgObject)$hapID)
    nHaplo <- apply(phgHapIDMat[, keepRanges], 2, function(vec) length(setdiff(unique(vec), -1)))

    # Return the numerical information
    rr <- SummarizedExperiment::as.data.frame(rr)
    return(cbind(rr[keepRanges, ], numHaplotypes = nHaplo))
}



#' @title Plot the number of haplotypes
#'
#' @description This function will plot the number of haplotypes. Its input
#'   will be the output of the \code{numHaploPerRange()} function.
#'
#' @param refRanges The output of \code{numHaploPerRange()}
#'
#' @export
plotNumHaplo <- function(refRanges){
    nRanges <- nrow(refRanges)
    # Prepare the plotting coordinates
    # Two possibilities: ranges are contiguous or not
    # If the former lines join all ranges. If the latter,
    # lines make bars starting from 1.  Pretty arbitrary.
    xcoord <- strtEnd <- c(t(refRanges[ ,c("start", "end")]))
    ycoord <- rep(refRanges$numHaplotypes, each=2)
    rangeDiff <- diff(xcoord)[1:nRanges*2]
    if (sum(rangeDiff == 1, na.rm=T) <= nRanges / 2){ # Ranges are not contiguous
      xcoord <- rep(xcoord, each=2)
      ycoord <- rep(ycoord, each=2)
      ycoord[c(1:nRanges * 4, 1:nRanges * 4 - 3)] <- 1
    }
    ylim <- c(0, max(ycoord))
    plot(xcoord, ycoord, type="l", xlab="Physical Position", ylab="Number of Haplotypes", ylim=ylim)
    # Small red vertical lines show the limits of each reference range
    lines(x=strtEnd, y=rep(1, 2*nRanges), type="h", col="red")
}



#' @title Calculate the mutual information between a set of reference ranges
#'
#' @description Mutual information quantifies the "amount of information"
#'   obtained about one random variable through observing the other random
#'   variable. Specify the gamete names over which you want to calculate and
#'   reference ranges.
#'
#' @param phgHapIDMat The output of the \code{hapIDMatrix()} function.
#' @param phgObject A PHG object.
#' @param gameteNames Specified gamete names. If \code{NULL}, gamete names will
#'   default to taxa IDs (haplottype ID matrix row names).
#' @param refRanges What reference ranges you wan to specify?
#'
#' @importFrom stats model.matrix
calcMutualInfo <- function(phgHapIDMat=NULL, phgObject=NULL, gameteNames=NULL, refRanges){
  if (is.null(phgHapIDMat)){
    if (is.null(phgObject)) stop("Must supply phgHapIDMat or phgObject")
    phgHapIDMat <- hapIDMatrix(phgObject=phgObject)
  }
  if (is.null(gameteNames)) gameteNames <- rownames(phgHapIDMat)
  phgHapIDMat <- phgHapIDMat[gameteNames, refRanges, drop=FALSE]
  # you can't do this with single gametes or ranges
  if (any(dim(phgHapIDMat) < 2)) return(NULL)
  # Calculate the mutual information across a pair of ranges
  # I(X;Y) = Sum p(x, y)log{p(x, y) / [p(x)p(y)]}
  mutualInfoPair <- function(phgHapIDMat, twoRanges){
    hapID <- phgHapIDMat[,twoRanges]
    # Remove any rows that have missing data
    hapID <- hapID[!apply(hapID, 1, function(v) any(v == -1)),]
    # Check if any columns have only one haplotype
    test1haplo <- apply(hapID, 2, function(v) length(unique(v)) == 1)
    if (any(test1haplo)) return(0)
    hapID <- apply(hapID, 2, as.character)
    nHap1 <- length(unique(hapID[,1]))
    nHap2 <- length(unique(hapID[,2]))
    mm1 <- model.matrix(~ -1 + hapID[,1]) %>% colMeans
    mm2 <- model.matrix(~ -1 + hapID[,2]) %>% colMeans
    mmm <- tcrossprod(mm1, mm2)
    mmi <- model.matrix(~ -1 + hapID[,1]:hapID[,2]) %>% colMeans %>% matrix(nHap1, nHap2)
    mi <- mmi * log2(mmi / mmm) # Some of these will be NaN, removed by na.rm=T
    return(sum(mi, na.rm=T))
  }
  # Calculate the mutual information across all pairs of ranges
  nRanges <- length(refRanges)
  miMat <- matrix(NA, nrow=nRanges, ncol=nRanges)
  rownames(miMat) <- colnames(miMat) <- refRanges
  for (range1 in 1:(nRanges - 1)){
    for (range2 in (range1 + 1):nRanges){
      miMat[range1, range2] <- mutualInfoPair(phgHapIDMat, c(refRanges[range1], refRanges[range2]))
    }
  }
  return(miMat)
}



#' @title Calculate and plot mutual information between a set of reference ranges
#'
#' @description Mutual information quantifies the “amount of information”
#'   obtained about one random variable through observing the other random
#'   variable.
#'
#' @param phgHapIDMat The output of the \code{hapIDMatrix()} function.
#' @param phgObject A PHG object.
#' @param gameteNames Specified gamete names. If \code{NULL}, gamete names will
#'   default to taxa IDs (haplottype ID matrix row names).
#' @param refRanges What reference ranges you wan to specify?
#'
#' @importFrom corrplot corrplot
#'
#' @export
plotMutualInfo <- function(phgHapIDMat = NULL, phgObject = NULL, gameteNames = NULL, refRanges){
    mi <- calcMutualInfo(phgHapIDMat, phgObject, gameteNames = NULL, refRanges)
    corrplot::corrplot(mi, type = "upper", is.corr=F)
    return(mi)
}



## Function to say if haplotypes same, discarding comparisons with -1
# gamHapIDs and targetHapIDs are both vectors of haplotype IDs.
# The output is the fraction of hapIDs that are different
# With ranges that contain -1 not included in the fraction
calcDiff <- function(gamHapIDs, targetHapIDs){
    keep <- which(gamHapIDs != -1 & targetHapIDs != -1)
    if (length(keep) == 0) return(Inf)
    return(sum(gamHapIDs[keep] != targetHapIDs[keep]) / length(keep))
}

#' @title Search for similar gamets
#'
#' @description Search for inbred lines (gametes) that are similar to a
#'   specified gamete in specified reference ranges. Supply either a haplotype
#'   ID matrix or a phgObject from which to extract it. Specify a gamete name
#'   and reference ranges. The difference between haplotypes is either 0 (same)
#'   or 1 (different). Fraction of ranges that are different has to be lower or
#'   equal to fractionDiff. Ranges with unknown haplotypes (-1) do not count in
#'   the fraction. If all pairwise range comparisons have -1 the lines are
#'   considered dissimilar.
#'
#' @param gameteName A specified gamete name
#' @param phgHapIDMat The output of the \code{hapIDMatrix()} function. If
#'   \code{NULL}, A hap ID matrix will be generated (if you have supplied a
#'   PHG object).
#' @param phgObject A PHG object.
#' @param refRanges Specifed reference ranges.
#' @param fractionDiff The difference between haplotypes (either 0 or 1). See
#'   description for further details.
#'
#' @importFrom magrittr %>%
#'
#' @export
searchSimilarGametes <- function(gameteName, phgHapIDMat = NULL, phgObject = NULL, refRanges, fractionDiff = 0){
    if (is.null(phgHapIDMat)){
        if (is.null(phgObject)) stop("Must supply phgHapIDMat or phgObject")
        phgHapIDMat <- hapIDMatrix(phgObject=phgObject)
    }
    # The row the target gamete is in
    gameteRow <- which(rownames(phgHapIDMat) == gameteName)
    if (length(gameteRow) == 0) stop(paste0("Gamete ", gameteName, " not in the PHG"))
    # Only deal with specified reference ranges
    phgHapIDMat <- phgHapIDMat[, refRanges, drop=FALSE]
    targetHapIDs <- phgHapIDMat[gameteRow, , drop=FALSE]
    # Calculate differences across all gametes in the table
    fracDiffs <- apply(phgHapIDMat, 1, calcDiff, targetHapIDs=targetHapIDs)
    areSimilar <- which(fracDiffs <= fractionDiff) %>% setdiff(gameteRow)
    # Return names of gametes that are similar to the target
    return(rownames(phgHapIDMat)[areSimilar])
}



#' @title Search for recombination
#'
#' @description Search for inbred lines (gametes) that are the same in one
#'   range but different in another. Such lines have experienced recombination
#'   in the past relative to each other. Must specify a gamete name and
#'   reference ranges.
#'
#' @param gameteName A specified gamete name
#' @param phgHapIDMat The output of the \code{hapIDMatrix()} function. If
#'   \code{NULL}, A hap ID matrix will be generated (if you have supplied a
#'   PHG object).
#' @param phgObject A PHG object.
#' @param refRangeSame See description for further details.
#' @param refRangeDiff See description for further details.
#'
#' @importFrom magrittr %>%
#'
#' @export
searchRecombination <- function(gameteName, phgHapIDMat = NULL, phgObject = NULL, refRangeSame, refRangeDiff){
  if (is.null(phgHapIDMat)){
    if (is.null(phgObject)) stop("Must supply phgHapIDMat or phgObject")
    phgHapIDMat <- hapIDMatrix(phgObject=phgObject)
  }
  gametesSame <- searchSimilarGametes(gameteName, phgHapIDMat, refRanges=refRangeSame) %>% setdiff(gameteName)
  targetDiff <- phgHapIDMat[gameteName, refRangeDiff]
  gametesDiff <- sapply(phgHapIDMat[gametesSame, refRangeDiff], calcDiff, targetHapIDs=targetDiff)
  return(gametesSame[gametesDiff == 1])
}
