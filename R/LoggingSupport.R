#' @title Start PHG logging information
#'
#' @description This function will create a file for storing logging output
#'    from the PHG.
#'
#' @param fullPath full working path of log file location. If \code{NULL},
#'    logging file will be added to current working directory.
#' @param fileName name of logging file. If \code{NULL}, filename will resort
#'    to "rPHG_log".
#' @param ... additional parameters to be added
#'
#' @export
startLogger <- function(fullPath = NULL, fileName = NULL, ...) {
    if (is.null(fileName)) {
        fileName <- "rPHG_log"
    }

    ## Remove slash from end of path (for purely aesthetics purposes only)
    #if (grepl(pattern = "/$", x = fullPath)) {
    #    fullPath <- gsub(pattern = "/$", replacement = "", x = fullPath)
    #}

    if (is.null(fullPath)) {
        file.create(fileName)
        rtlog <- file.path(getwd(), fileName)
    } else {
        if (grepl(pattern = "~", x = fullPath)) {
            stop(
                paste0(
                    "It seems that you are using a '~' instead of your full",
                    " home directory path.\n",
                    "  Consider using: ", Sys.getenv("HOME")
                )
            )
        }
        file.create(file.path(fullPath, fileName))
        rtlog <- file.path(fullPath, fileName)
    }

    rJava::.jcall(
        "net.maizegenetics/util/LoggingUtils",
        "V",
        "setupLogfile",
        rtlog
    )

    message("PHG logging file created at: ", rtlog)
}



#' @title Create PHG config file (WIP)
#'
#' @description This function will create a config file on the fly for basic
#'    PHG databases built via SQLite. This is a \strong{work in progress.}
#'    \strong{FOR ADVANCED USE ONLY.}
#'
#' @param dbName Database name (\code{*.db}).
#' @param dbType What type of database is this?
#' @param fileName Configuration file name (\code{*.txt}).
#' @param exportPath Specify a specific export path.
#' @param user Username for database.
#' @param password Password for database.
configFileMaker <- function(dbName,
                            dbType = c("sqlite", "postgresql"),
                            fileName = NULL,
                            exportPath = NULL,
                            user = NULL,
                            password = NULL) {
    # Logic (1)
    if(missing(dbType) || !dbType %in% dbType) {
        stop("Please specify correct DB type: 'sqlite' or 'postgresql'")
    }

    # Logic (2)
    if (is.null(fileName)) fileName <- "configFilePHG.txt"
    if (is.null(exportPath)) exportPath <- getwd()
    if (is.null(user)) user <- dbType
    if (is.null(password)) password <- dbType

    # Write to file
    fileConn <- file(paste0(exportPath, "/", fileName))
    writeLines(
        text = c(
            "host=localHost",
            paste0("user=", user),
            paste0("password=", password),
            paste0("DB=", exportPath, "/", dbName),
            paste0("DBtype=", dbType)
        ),
        con = fileConn
    )
    close(fileConn)
}


