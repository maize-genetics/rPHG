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


