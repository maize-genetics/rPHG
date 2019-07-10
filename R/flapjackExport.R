#' @title Export a PHG object to Flapjack file formats.
#'
#' @description This function will take a PHG object and export specified
#'   ranges to a Flapjack file format. Take note that in order for output to
#'   be generated, you will have to build your PHG with a the parameter
#'   \code{includeVariant} to \code{TRUE}.
#'
#' @param phgObject A PHG object.
#' @param outputName A specified output name for your Flapjack files. Defaults
#'   to \code{NULL}. If \code{NULL} file name will be \code{phg_output}
#'
#' @importFrom rJava J
#'
#' @export
flapjackExport <- function(phgObject, outputName = NULL) {

    ## Logic
    if (missing(phgObject)) {
        stop("Function needs a PHG object variable to work.")
    }
    if (is.null(outputName)) {
        outputName <- "phg_output"
    }

    ## Get exporter and create Flapjack files
    rJava::J(
        "net.maizegenetics.pangenome.api/RMethods",
        "exportPHGToFlapjack",
        phgObject,
        outputName
    )
}
