## ----
#' @title A PHGDataSet class
#'
#' @description A class to represent practical haplotype graph data which is
#'   wrapped in a \code{RangedSummarizedExperiment} class.
#'
#' @importFrom methods setClass
#'
#' @export
setClass(
    Class = "PHGDataSet",
    contains = "RangedSummarizedExperiment"
)



# /// Methods (general) /////////////////////////////////////////////

## ----
#' @rdname calcMutualInfo
#' @export
setMethod(
    f = "calcMutualInfo",
    signature = signature(object = "PHGDataSet"),
    definition = function(object) {
        return(calcMutualInfoFromPHGDataSet(object))
    }
)


## ----
#' @rdname numHaploPerRefRange
#' @export
setMethod(
    f = "numHaploPerRefRange",
    signature = signature(object = "PHGDataSet"),
    definition = function(object) {
        return(nHaploPerRefRangeFromPHGDataSet(object))
    }
)


## ----
#' @param object A \code{PHGDataSet} object
#' @param samples Samples/taxa to include in plot
#' @param sampleHighlight Sample path to highlight
#' @param seqnames A sequence (e.g. chromosome) ID
#' @param start Start position for ref ranges
#' @param end End position for ref ranges
#' @param colMajor Highlight path color
#' @param colMinor Muted path color
#' @param ... Additional parameters to pass for ref range inclusion
#'
#' @rdname plotGraph
#' @export
setMethod(
    f = "plotGraph",
    signature = signature(object = "PHGDataSet"),
    definition = function(
        object,
        samples = NULL,
        sampleHighlight = NULL,
        seqnames = NULL,
        start = NULL,
        end = NULL,
        colMajor = "maroon",
        colMinor = "lightgrey"
    ) {
        return(
            plotGraphCore(
                object,
                samples,
                sampleHighlight,
                seqnames,
                start,
                end,
                colMajor,
                colMinor
            )
        )
    }
)


## ----
#' @param object A \code{PHGDataSet} object
#'
#' @rdname plotMutualInfo
#' @export
setMethod(
    f = "plotMutualInfo",
    signature = signature(object = "PHGDataSet"),
    definition = function(object) {
        return(
            plotMutualInfoFromPHGDataSet(object)
        )
    }
)




## ----
#' @param object A \code{PHGDataSet} object
#' @param samples Samples/taxa to include in plot
#' @param seqnames A sequence (e.g. chromosome) ID
#' @param start Start position for ref ranges
#' @param end End position for ref ranges
#'
#' @rdname taxaByNode
#' @export
setMethod(
    f = "taxaByNode",
    signature = signature(object = "PHGDataSet"),
    definition = function(
        object,
        samples = NULL,
        seqnames,
        start,
        end
    ) {
        return(
            taxaByNodeCore(
                object,
                samples,
                seqnames,
                start,
                end
            )
        )
    }
)


