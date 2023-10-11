# ----
# @title Visualize Graph Data
#
# @description
# Generates an interactive network plot for a given set of reference ranges
# and a set of taxa.
#
# @param x A \code{PHGDataSet} object
# @param samples Samples/taxa to include in plot
# @param sampleHighlight Sample path to highlight
# @param seqnames A sequence (e.g. chromosome) ID
# @param start Start position for ref ranges
# @param end End position for ref ranges
# @param colMajor Highlight path color
# @param colMinor Muted path color
# @param ... Additional parameters to pass for ref range inclusion
#
# @importFrom IRanges subsetByOverlaps
# @importFrom GenomicRanges GRanges
# @importFrom SummarizedExperiment assay
# @importFrom visNetwork visEdges
# @importFrom visNetwork visHierarchicalLayout
# @importFrom visNetwork visNetwork
plotGraphCore <- function(
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
    colnames(currentMatrix) <- as.numeric(gsub("R", "", colnames(currentMatrix)))

    # Get ref range data frame
    refRangeDataMini <- as.data.frame(SummarizedExperiment::rowRanges(hapTableMini))

    # Group taxa by hap ID and ref range
    taxaGroups <- lapply(seq_len(ncol(currentMatrix)), function(i) {
        split(rownames(currentMatrix), currentMatrix[, i])
    })

    # Generate distinct IDs (hap ID + ref range ID)
    hapIds     <- apply(currentMatrix, 2, unique, simplify = FALSE)
    hapLevels  <- as.numeric(rep(names(hapIds), vapply(hapIds, length, integer(1))))
    fullHapIds <- paste0(
        unlist(lapply(hapIds, function(i) i[order(i)])),
        "_", hapLevels
    )

    # HTML tooltip processing
    taxaToHtml <- function(x) {
        vapply(x, function(i) {
            paste0("<b>Taxa: </b>", paste(i, collapse = ", "), "</p>")
        }, character(1))
    }
    tooltipVec <- unlist(lapply(taxaGroups, taxaToHtml))

    refRangeHtml <- unlist(lapply(hapLevels, function(i) {
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
    }))

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

        cnxn <- unique(paste0(ln, "+", rn))

        for (c in cnxn) {
            splits <- unlist(strsplit(c, "\\+"))
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
    network <- visNetwork::visNetwork(nodes, edges)
    edges   <- visNetwork::visEdges(network, arrows = "to")
    layout  <- visNetwork::visHierarchicalLayout(network, direction = "LR")
    return(layout)
}
