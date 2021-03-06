#' @title Logic support for config files
#'
#' @description Provides logic checking for config files used in PHG creation.
#'
#' @author Brandon Monier
#'
#' @param configFile Path to a configuration file for your graph database.
configCatcher <- function(configFile) {

    if (!file.exists(configFile)) {
        stop ("Path to config file does not exist.")
    }

    tmpLines <- readLines(configFile)
    dbParam <- tmpLines[grepl("DB=", tmpLines)]
    credParam <- tmpLines[grepl("user=|password=", tmpLines)]

    if (length(credParam) != 2) {
        stop("Missing credentials (user= and/or password=) in config file.")
    }

    if (length(dbParam) == 0) {
        stop("Database parameter (DB=) in config file does not exist.")
    }

    if (length(dbParam) > 1) {
        stop("Config file contains more than one database path parameter (DB=).")
    }

    dbParam <- gsub("DB=", "", dbParam)

    if (!file.exists(dbParam)) {
        stop("Path to database (DB=) in config file does not exist.")
    }
}

