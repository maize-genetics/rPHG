## ----
# @title Get taxa data for selected reference ranges
#
# @description
# Returns taxa (e.g. sample) information for a select set of reference ranges.
# Reference ranges are identified by a user defined genomic range consisting
# of a sequence (e.g. chromosome) ID, and start and stop positions.
#
# @param x A \code{PHGDataSet} object
# @param samples Samples/taxa to include in plot
# @param seqnames A sequence (e.g. chromosome) ID
# @param start Start position for ref ranges
# @param end End position for ref ranges
taxaByNodeCore <- function(
    x,
    samples = NULL,
    seqnames,
    start,
    end
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
    colnames(currentMatrix) <- gsub("R", "", colnames(currentMatrix)) |>
        as.numeric()

    # Get ref range data frame
    refRangeDataMini <- SummarizedExperiment::rowRanges(hapTableMini) |> as.data.frame()

    # Group taxa by hap ID and ref range
    taxaGroups <- lapply(seq_len(ncol(currentMatrix)), function(i) {
        split(rownames(currentMatrix), currentMatrix[, i])
    })

    return(taxaGroups)
}


