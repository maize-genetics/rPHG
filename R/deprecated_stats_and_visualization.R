## # === rPHG Stats Visualization Functions (WIP) ======================
##
## #' @title Get the number of haplotypes per range in physical position segment
## #'
## #' @author Jean-Luc Jannink
## #'
## #' @param phgObject A PHG object.
## #' @param chr What chromosome do you want to inspect? Defaults to \code{NULL}.
## #'   If \code{NULL}, all chromsomes will be selected.
## #' @param start Start position of chromosome. Defaults to \code{0}.
## #' @param end End position of chromosome. Defaults to \code{NULL}. If
## #'   \code{NULL}, the whole chromosome will be analyzed.
## #'
## #' @importFrom S4Vectors DataFrame
## #' @importFrom SummarizedExperiment as.data.frame
## #' @importFrom SummarizedExperiment assays
## #' @importFrom SummarizedExperiment ranges
## #' @importFrom SummarizedExperiment rowRanges
## #' @importFrom SummarizedExperiment seqnames
## #'
## #' @export
## numHaploPerRange <- function(phgObject,
##                              chr = NULL,
##                              start = 0,
##                              end = NULL) {
##
##     # Get information about the reference ranges
##     rr <- SummarizedExperiment::rowRanges(phgObject)
##
##     # Logic
##     if (is.null(end)) {
##         end <- max(end(rr))
##     }
##
##     allChr <- unique(SummarizedExperiment::seqnames(phgObject))
##     allChr <- as.vector(allChr)
##     if (is.null(chr)) {
##         chr <- allChr
##     } else{
##         if (!all(chr %in% allChr)) {
##             warning(paste(c("The following chromosomes are not found:", setdiff(chr, allChr)), collapse=" "))
##         }
##     }
##
##     # Which reference ranges on the chromosome within start and end positions
##     tmp <- as.vector(SummarizedExperiment::seqnames(phgObject))
##     keepRanges <- which(tmp %in% chr & start <= start(rr) & end(rr) <= end)
##
##     if (length(keepRanges) == 0) {
##         stop("There are no ranges with requested start and end")
##     }
##
##     # How many haplotypes are in those reference ranges
##     phgHapIDMat <- t(SummarizedExperiment::assays(phgObject)$hapID)
##
##     if (dim(phgObject)[2] == 1) {
##         phgFilt <- phgHapIDMat[, keepRanges]
##         phgFilt <- t(as.matrix(phgFilt))
##     } else {
##         phgFilt <- phgHapIDMat[, keepRanges]
##     }
##     nHaplo <- apply(phgFilt, 2, function(vec) {
##         length(unique(vec))
##     })
##
##     # Return the numerical information
##     rr <- SummarizedExperiment::as.data.frame(rr)
##     rr <- cbind(rr[keepRanges,], numHaplotypes = nHaplo)
##     rr <- rr[, c(6, 1, 2, 3, 4, 7)]
##     return(S4Vectors::DataFrame(rr))
## }



## #' @title Plot the number of haplotypes
## #'
## #' @description This function will plot the number of haplotypes. Its input
## #'   will be the output of the \code{numHaploPerRange()} function.
## #'
## #' @param haploData The output of \code{numHaploPerRange()}
## #'
## #' @import ggplot2
## #' @importFrom rlang .data
## #' @importFrom stats median
## #'
## #' @export
## plotNumHaplo <- function(haploData) {
##     # Coerce to data frame for ggplot2
##     tmp <- as.data.frame(haploData)
##
##     # Shape proportions
##     yfrac <- 0.1
##     xfrac <- 0.001
##
##     # Add shape data
##     tmp$med <- apply(tmp[, 3:4], 1, stats::median)
##     tmp$color <- "#91baff"
##     tmp[seq(1, nrow(tmp), by = 2),]$color <- "#3e619b"
##
##     # Get limit data
##     xbeg <- min(tmp$start)
##     xend <- max(tmp$end)
##     yend <- max(tmp$numHaplotypes)
##
##     # Visualize
##     hapPlot <- ggplot(data = tmp) +
##         ylim(-(yend * yfrac), yend) +
##         scale_x_continuous(limits = c(xbeg, xend)) +
##         geom_rect(
##             mapping = aes(
##                 xmin = .data$start,
##                 xmax = .data$end,
##                 ymin = 0,
##                 ymax = -(yend * yfrac)
##             ),
##             fill = tmp$color
##         ) +
##         geom_path(aes(x = .data$med, y = .data$numHaplotypes)) +
##         geom_point(aes(x = .data$med, y = .data$numHaplotypes), size = 1) +
##         facet_grid(seqnames ~ .) +
##         xlab("Physical Position (bp)") +
##         ylab("Number of Haplotypes")
##
##     return(hapPlot)
## }



## #' @title Calculate the mutual information between a set of reference ranges
## #'
## #' @description Mutual information quantifies the "amount of information"
## #'   obtained about one random variable through observing the other random
## #'   variable. Specify the gamete names over which you want to calculate and
## #'   reference ranges.
## #'
## #' @param phgHapIDMat The output of the \code{hapIDMatrix()} function.
## #' @param phgObject A PHG object.
## #' @param gameteNames Specified gamete names. If \code{NULL}, gamete names will
## #'   default to taxa IDs (haplottype ID matrix row names).
## #' @param refRanges What reference ranges you wan to specify?
## #'
## #' @importFrom S4Vectors metadata
## #' @importFrom stats model.matrix
## calcMutualInfo <- function(phgObject = NULL,
##                            refRanges,
##                            gameteNames = NULL,
##                            phgHapIDMat = NULL) {
##     if (is.null(phgHapIDMat)) {
##         if (is.null(phgObject)) {
##             stop("Must supply phgHapIDMat or phgObject")
##         }
##         phgHapIDMat <- hapIDMatrix(phgObject = S4Vectors::metadata(phgObject)$jObj)
##     }
##
##     if (is.null(gameteNames)) {
##         gameteNames <- rownames(phgHapIDMat)
##     }
##
##     phgHapIDMat <- phgHapIDMat[gameteNames, refRanges, drop = FALSE]
##     # you can't do this with single gametes or ranges
##     if (any(dim(phgHapIDMat) < 2)) {
##         return(NULL)
##     }
##
##     # Calculate the mutual information across a pair of ranges
##     # I(X;Y) = Sum p(x, y)log{p(x, y) / [p(x)p(y)]}
##     mutualInfoPair <- function(phgHapIDMat, twoRanges) {
##         hapID <- phgHapIDMat[, twoRanges]
##
##         # Remove any rows that have missing data
##         hapID <- hapID[!apply(hapID, 1, function(v) any(v == -1)), ]
##
##         # Check if any columns have only one haplotype
##         test1haplo <- apply(hapID, 2, function(v) length(unique(v)) == 1)
##         if (any(test1haplo)) {
##             return(0)
##         }
##         hapID <- apply(hapID, 2, as.character)
##         nHap1 <- length(unique(hapID[, 1]))
##         nHap2 <- length(unique(hapID[, 2]))
##         mm1 <- model.matrix( ~ -1 + hapID[, 1]) %>% colMeans
##         mm2 <- model.matrix( ~ -1 + hapID[, 2]) %>% colMeans
##         mmm <- tcrossprod(mm1, mm2)
##         mmi <- model.matrix( ~ -1 + hapID[, 1]:hapID[, 2]) %>% colMeans %>% matrix(nHap1, nHap2)
##         mi <- mmi * log2(mmi / mmm) # Some of these will be NaN, removed by na.rm=T
##         return(sum(mi, na.rm = T))
##     }
##     # Calculate the mutual information across all pairs of ranges
##     nRanges <- length(refRanges)
##     miMat <- matrix(NA, nrow = nRanges, ncol = nRanges)
##     rownames(miMat) <- colnames(miMat) <- refRanges
##     for (range1 in 1:(nRanges - 1)) {
##         for (range2 in (range1 + 1):nRanges) {
##             miMat[range1, range2] <-
##                 mutualInfoPair(phgHapIDMat, c(refRanges[range1], refRanges[range2]))
##         }
##     }
##     return(miMat)
## }



## #' @title Calculate and plot mutual information between a set of reference ranges
## #'
## #' @description Mutual information quantifies the “amount of information”
## #'   obtained about one random variable through observing the other random
## #'   variable.
## #'
## #' @param phgHapIDMat The output of the \code{hapIDMatrix()} function.
## #' @param phgObject A PHG object.
## #' @param gameteNames Specified gamete names. If \code{NULL}, gamete names will
## #'   default to taxa IDs (haplottype ID matrix row names).
## #' @param refRanges What reference ranges you wan to specify?
## #'
## #' @importFrom corrplot corrplot
## #'
## #' @export
## plotMutualInfo <- function(phgObject = NULL,
##                            refRanges,
##                            gameteNames = NULL,
##                            phgHapIDMat = NULL) {
##     mi <- calcMutualInfo(
##         phgObject = phgObject,
##         refRanges = refRanges,
##         gameteNames = NULL,
##         phgHapIDMat
##     )
##     mi[is.na(mi)] <- 0
##     corrplot::corrplot(mi, type = "upper", is.corr = F)
##     # return(mi)
## }



## Function to say if haplotypes same, discarding comparisons with -1
# gamHapIDs and targetHapIDs are both vectors of haplotype IDs.
# The output is the fraction of hapIDs that are different
# With ranges that contain -1 not included in the fraction
calcDiff <- function(gamHapIDs, targetHapIDs) {
  keep <- which(gamHapIDs != -1 & targetHapIDs != -1)
  if (length(keep) == 0) {
    return(Inf)
  }
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
#' @importFrom S4Vectors metadata
#'
#' @export
searchSimilarGametes <- function(phgObject = NULL,
                                 refRanges,
                                 gameteName,
                                 fractionDiff = 0,
                                 phgHapIDMat = NULL) {
    if (is.null(phgHapIDMat)) {
        if (is.null(phgObject)) {
            stop("Must supply phgHapIDMat or phgObject")
        }
        phgHapIDMat <- hapIDMatrix(phgObject = S4Vectors::metadata(phgObject)$jObj)
    }

    # The row the target gamete is in
    gameteRow <- which(rownames(phgHapIDMat) == gameteName)
    if (length(gameteRow) == 0) {
        stop(paste0("Gamete ", gameteName, " not in the PHG"))
    }

    # Only deal with specified reference ranges
    phgHapIDMat <- phgHapIDMat[, refRanges, drop = FALSE]
    targetHapIDs <- phgHapIDMat[gameteRow, , drop = FALSE]

    # Calculate differences across all gametes in the table
    fracDiffs <- apply(phgHapIDMat, 1, calcDiff, targetHapIDs = targetHapIDs)
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
#' @importFrom S4Vectors metadata
#'
#' @export
searchRecombination <- function(phgObject = NULL,
                                gameteName,
                                refRangeSame,
                                refRangeDiff,
                                phgHapIDMat = NULL) {
    if (is.null(phgHapIDMat)) {
        if (is.null(phgObject)) {
            stop("Must supply phgHapIDMat or phgObject")
        }
        phgHapIDMat <- hapIDMatrix(phgObject = phgObject)
    }

    gametesSame <- searchSimilarGametes(
        gameteName,
        phgHapIDMat,
        refRanges = refRangeSame
    ) %>%
        setdiff(gameteName)

    targetDiff <- phgHapIDMat[gameteName, refRangeDiff]

    gametesDiff <- sapply(
        phgHapIDMat[gametesSame, refRangeDiff],
        calcDiff,
        targetHapIDs = targetDiff
    )

    return(gametesSame[gametesDiff == 1])
}


# ----
#' @title Visualize Graph Data
#'
#' @description
#' Generates an interactive network plot for a given set of reference ranges
#' and a set of taxa.
#'
#' @param x A \code{PHGDataSet} object
#' @param samples Samples/taxa to include in plot
#' @param sampleHighlight Sample path to highlight
#' @param seqnames A sequence (e.g. chromosome) ID
#' @param start Start position for ref ranges
#' @param end End position for ref ranges
#' @param colMajor Highlight path color
#' @param colMinor Muted path color
#' @param ... Additional parameters to pass for ref range inclusion
#'
#' @importFrom IRanges subsetByOverlaps
#' @importFrom GenomicRanges GRanges
#' @importFrom SummarizedExperiment assay
#' @importFrom visNetwork visEdges
#' @importFrom visNetwork visHierarchicalLayout
#' @importFrom visNetwork visNetwork
#'
#' @export
plotGraph <- function(
    x,
    samples = NULL,
    sampleHighlight = NULL,
    seqnames = NULL,
    start = NULL,
    end = NULL,
    colMajor = "maroon",
    colMinor = "lightgrey",
    ...
) {
    # # Testing
    # start <- 100
    # end   <- 1000000
    # seqnames <- "1"
    # # samples <- c("Z001E0001", "Z001E0028", "Z001E0080")
    # # samples <- NULL
    # set.seed(123)
    # samples <- sample(colnames(x), 100)
    # # sampleHighlight <- c("Z001E0001")
    # sampleHighlight <- sample(samples, 1)

    # Filter by taxa and ref ranges
    if (is.null(samples)) samples <- colnames(x)
    hapTableMini <- x[, colnames(x) %in% samples]
    hapTableMini <- IRanges::subsetByOverlaps(
        hapTableMini,
        GenomicRanges::GRanges(seqnames = seqnames, ranges = start:end)
    )

    # Get hap ID matrix
    currentMatrix <- t(SummarizedExperiment::assay(hapTableMini))
    currentMatrix[is.na(currentMatrix)] <- -128
    colnames(currentMatrix) <- gsub("R", "", colnames(currentMatrix)) |>
        as.numeric()

    # Get ref range data frame
    refRangeDataMini <- rowRanges(hapTableMini) |> as.data.frame()

    # Group taxa by hap ID and ref range
    taxaGroups <- lapply(seq_len(ncol(currentMatrix)), function(i) {
        split(rownames(currentMatrix), currentMatrix[, i])
    })

    # Generate distinct IDs (hap ID + ref range ID)
    hapIds     <- currentMatrix |> apply(2, unique, simplify = FALSE)
    hapLevels  <- rep(names(hapIds), vapply(hapIds, length, integer(1))) |> as.numeric()
    fullHapIds <- paste0(
        lapply(hapIds, function(i) i[order(i)]) |> unlist(),
        "_", hapLevels
    )

    # HTML tooltip processing
    taxaToHtml <- function(x) {
        vapply(x, function(i) {
            paste0("<b>Taxa: </b>", paste(i, collapse = ", "), "</p>")
        }, character(1))
    }
    tooltipVec <- lapply(taxaGroups, taxaToHtml) |> unlist()

    refRangeHtml <- lapply(hapLevels, function(i) {
        paste0(
            "<p><b>Chr: </b>",
            refRangeDataMini[i, ]$seqnames,
            "<br>",
            "<b>Range: </b>",
            refRangeDataMini[i, ]$start,
            " - ",
            refRangeDataMini[i, ]$end,
            "<br>"
        )
    }) |> unlist()

    # Final graph data (nodes)
    nodes <- data.frame(
        id    = seq_along(fullHapIds),
        label = fullHapIds,
        level = hapLevels,
        title = paste0(refRangeHtml, tooltipVec)
    )

    if (!is.null(sampleHighlight)) {
        for (i in sampleHighlight) {
            nodes$group <- ifelse(grepl(i, nodes$title), i, NA)
            nodes$color <- ifelse(grepl(i, nodes$title), colMajor, colMinor)
        }
        nodes$title <- gsub(i, paste0("<b>", i, "</b>"), nodes$title)
    } else {
        nodes$color <- colMajor
    }

    # Final graph data (edges)
    lne <- c()
    rne <- c()
    for (i in seq_len(ncol(currentMatrix) - 1)) {
        ln <- paste0(currentMatrix[, i], "_", i)
        rn <- paste0(currentMatrix[, i + 1], "_", i + 1)

        cnxn <- paste0(ln, "+", rn) |> unique()

        for (c in cnxn) {
            splits <- strsplit(c, "\\+") |> unlist()
            f <- which(fullHapIds == splits[1])
            t <- which(fullHapIds == splits[2])
            lne <- c(lne, f)
            rne <- c(rne, t)
        }
    }

    edges <- data.frame(
        from = lne,
        to = rne
    )

    # Return vis.js object
    visNetwork::visNetwork(nodes, edges) |>
        visNetwork::visEdges(arrows = "to") |>
        visNetwork::visHierarchicalLayout(direction = "LR")
}


## ----
#' @title Retrieve read mapping information from PHG database.
#'
#' @description Returns an \code{S4Vectors} \code{DataFrame} object of read
#'   mapping information for a given line (i.e. taxon).
#'
#' @author Brandon Monier
#' @author Peter Bradbury
#'
#' @param configFile Path to a configuration file for your graph database.
#' @param lineName The name of the line (taxon) for which the read mapping
#'   information is to be retrieved. If there are multiple read mappings with
#'   different \code{file_group_names}, they will be combined.
#' @param readMappingMethodName The method name for the read mappings
#'   (only takes a single method).
#' @param haplotypeMethodName The haplotype method name.
#' @param fileGroup the name of the file group for the line from the database.
#'   This parameter is only necessary if the line (taxon) has more than one
#'   file group and only the reads for a specific file group are wanted.
#'
#' @importFrom rJava J
#' @importFrom S4Vectors DataFrame
#'
#' @export
readMappingsForLineName <- function(configFile,
                                    lineName,
                                    readMappingMethodName,
                                    haplotypeMethodName,
                                    fileGroup = NULL) {

  configCatcher(configFile)

  # Retrieve Java data vector object(s)
  rmObj <- rJava::J(
    "net.maizegenetics.pangenome.api/RMethods",
    "readMappingsForLineName",
    configFile,
    lineName,
    readMappingMethodName,
    haplotypeMethodName,
    fileGroup
  )

  # Configure for R
  colNum <- rmObj$dataVectors$size()
  rmDF <- lapply(seq_len(colNum), function(i) {
    rmObj$dataVectors$get(as.integer(i - 1))
  })
  rmDF <- data.frame(rmDF)
  colnames(rmDF) <- rmObj$columnNames

  # Return
  return(S4Vectors::DataFrame(rmDF))
}


## ----
#' @title Retrieve read mapping records from PHG database.
#'
#' @description Returns an \code{S4Vectors} \code{DataFrame} object of read
#'   mapping record information without \code{read_mapping} data.
#'
#' @author Brandon Monier
#' @author Peter Bradbury
#'
#' @param configFile Path to a configuration file for your graph database.
#'
#' @importFrom rJava J
#' @importFrom S4Vectors DataFrame
#'
#' @export
readMappingTableInfo <- function(configFile) {

  # Catch potential errors
  configCatcher(configFile)

  # Retrieve Java data vector object(s)
  rmObj <- rJava::J(
    "net.maizegenetics.pangenome.api/RMethods",
    "readMappingTableInfo",
    configFile
  )

  # Configure for R
  colNum <- rmObj$dataVectors$size()
  rmDF <- lapply(seq_len(colNum), function(i) {
    rmObj$dataVectors$get(as.integer(i - 1))
  })
  rmDF <- data.frame(rmDF)
  colnames(rmDF) <- rmObj$columnNames

  # Return
  return(tibble::as_tibble(rmDF))
}
