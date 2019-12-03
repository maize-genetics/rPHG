#' @title phgDataSet
#'
#' @description A class to represent a practical haplotype graph which is
#'   wrapped in a \code{RangedSummarizedExperiment} class.
#'
#' @importFrom methods setClass
#'
#' @export
setClass(
    Class = "PHGDataSet",
    contains = "RangedSummarizedExperiment"
)
