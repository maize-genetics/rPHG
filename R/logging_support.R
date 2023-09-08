## ----
#' @title Start PHG logging information
#'
#' @description This function will create a file for storing logging output
#'    from the PHG.
#'
#' @param path full working path of log file location. If \code{NULL},
#'    logging file will be added to current working directory.
#' @param verbose Print messages to console? Defaults \code{FALSE}.
#'
#' @export
startLogger <- function(path = NULL, verbose = TRUE) {
    if (is.null(path)) {
        path <- "rPHG_log"
    }

    path <- suppressWarnings(normalizePath(path))

    rJava::.jcall(TASSEL_API$LOGGING_UTILS, "V", "setupLogfile", path)

    if (verbose) {
        bullet <- cli::col_grey(cli::symbol$info)
        msg <- paste0(bullet, " PHG logging file created at: ", path)
        message(msg)
    }
}


