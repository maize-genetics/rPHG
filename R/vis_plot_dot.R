## ----
#' @title
#' Generate dot plot from Anchor files
#'
#' @description
#' This function will generate a dot plot (collinearity) from an Anchor
#' File generated from the program
#' [AnchorWave](https://github.com/baoxingsong/AnchorWave). This is one of the
#' preliminary steps in creating a PHG database.
#'
#' @param anchorPath
#' Path to anchor file.
#' @param querySeqId
#' Vector of sequence IDs (query).
#' @param refSeqId
#' Vector of sequence IDs (reference).
#' @param queryLab
#' Optional label for query axis.
#' @param refLab
#' Optional label for reference axis.
#' @param colorId
#' How to color plots (\code{strand} or \code{score})
#'
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 facet_grid
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 scale_color_viridis_c
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#'
#' @export
plotDot <- function(
    anchorPath,
    querySeqId = NULL,
    refSeqId = NULL,
    queryLab = NULL,
    refLab = NULL,
    colorId = c("strand", "score")
) {

    if (is.null(queryLab)) queryLab <- "Query"
    if (is.null(refLab)) refLab <- "Reference"

    colorId <- match.arg(colorId)
    if (colorId == "score") {
        scaleUnit <- ggplot2::scale_color_viridis_c()
    } else if (colorId == "strand") {
        scaleUnit <- ggplot2::scale_color_manual(
            values = c(
                "+" = "#DA897C",
                "-" = "#0D6A82"
            )
        )
    } else {
        scaleUnit <- NULL
    }

    tmpData <- utils::read.table(file = anchorPath, head = TRUE)

    if (!is.null(refSeqId)) tmpData <- tmpData[which(tmpData$refChr   %in% refSeqId), ]
    if (!is.null(querySeqId)) tmpData <- tmpData[which(tmpData$queryChr %in% querySeqId), ]

    toMb <- function(x) x / 1e6

    p <- ggplot2::ggplot(data = tmpData) +
        ggplot2::aes(
            x = rlang::.data[["queryStart"]],
            y = rlang::.data[["referenceStart"]],
            color = rlang::.data[[colorId]]
        ) +
        ggplot2::geom_point(size = 0.3) +
        ggplot2::scale_y_continuous(labels = toMb) +
        ggplot2::scale_x_continuous(labels = toMb) +
        ggplot2::facet_grid(
            rows   = ggplot2::vars(rlang::.data[["refChr"]]),
            col    = ggplot2::vars(rlang::.data[["queryChr"]]),
            scales = "free",
            space  = "free"
        ) +
        scaleUnit +
        ggplot2::xlab(paste(queryLab, "(Mbp)")) +
        ggplot2::ylab(paste(refLab, "(Mbp)"))

    return(p)
}


