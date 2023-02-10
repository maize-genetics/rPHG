#' @title Logic support for config files
#'
#' @description Provides logic checking for config files used in PHG creation.
#'
#' @author Brandon Monier
#'
#' @param configFile Path to a configuration file for your graph database.
configCatcher <- function(configFile) {

    if (!file.exists(configFile)) {
        stop ("Path to config file does not exist.", call. = FALSE)
    }

    tmpLines <- readLines(configFile)
    dbParam <- tmpLines[grepl("DB=", tmpLines)]
    credParam <- tmpLines[grepl("user=|password=", tmpLines)]
    dbType <- tmpLines[grepl("DBtype=", tmpLines)]

    if (!grepl("=postgres$|=sqlite$", dbType)) {
        stop("Only postgres or SQLite database types are allowed.", call. = FALSE)
    }

    if (length(credParam) != 2) {
        stop("Missing credentials (user= and/or password=) in config file.", call. = FALSE)
    }

    if (length(dbParam) == 0) {
        stop("Database parameter (DB=) in config file does not exist.", call. = FALSE)
    }

    if (length(dbParam) > 1) {
        stop("Config file contains more than one database path parameter (DB=).", call. = FALSE)
    }

    dbParam <- gsub("DB=", "", dbParam)

    if (!file.exists(dbParam) && grepl("sqlite", dbType)) {
        stop("Path to database (DB=) in SQLite config file does not exist.", call. = FALSE)
    }
}

