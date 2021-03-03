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


#' @title BrapiPHG Class
#'
#' @description Class \code{BrapiPHG} defines a \code{rPHG}
#'    Class for storing BrAPI connection data.
#'
#' @name BrapiPHG-class
#' @rdname BrapiPHG-class
#' @exportClass BrapiPHG
setClass(
    Class = "BrapiPHG",
    representation = representation(
        name = "character",
        url = "character"
    )
)


