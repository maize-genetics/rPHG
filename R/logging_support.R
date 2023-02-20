# === Logging Support ===============================================

## ----
#' @title Start PHG logging information
#'
#' @description This function will create a file for storing logging output
#'    from the PHG.
#'
#' @param path full working path of log file location. If \code{NULL},
#'    logging file will be added to current working directory.
#'
#' @export
startLogger <- function(path = NULL) {
    if (is.null(path)) {
        path <- "rPHG_log"
    }

    if (grepl(pattern = "^~", x = path)) {
        stop(
            paste0(
                "It seems that you are using a '~' instead of your full",
                " home directory path.\n",
                "  Consider using: ", Sys.getenv("HOME")
            )
        )
    }

    rJava::.jcall(
        "net.maizegenetics/util/LoggingUtils",
        "V",
        "setupLogfile",
        path
    )

    message("PHG logging file created at: ", path)
}


