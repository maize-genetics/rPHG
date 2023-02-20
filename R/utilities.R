# === Miscellaneous utilities for rPHG methods ======================

## ----
#' @title Create mock config file
#'
#' @description Creates a temporary PHG configuration file to access the
#'    provided example database. Mainly for debugging and educational
#'    purposes.
#'
#' @param file User defined output file
#' @param host Host service for database
#' @param user User ID for database access
#' @param password Password for database access
#' @param dbType Database architecture used
#' @param dbPath P
createConfigFile <- function(
    file,
    host = "localhost",
    user = "user",
    password = "sqlite",
    dbType = "sqlite",
    dbPath = NULL
) {
    myFile <- file(file, "w")

    if (is.null(dbPath)) {
        dbPath <- system.file(
            "extdata",
            "phg_smallseq_test.db",
            package = "rPHG"
        )
    }

    writeLines(sprintf("host=%s", host), myFile, sep = "\n")
    writeLines(sprintf("user=%s", user), myFile, sep = "\n")
    writeLines(sprintf("password=%s", password), myFile, sep = "\n")
    writeLines(sprintf("DB=%s", dbPath), myFile, sep = "\n")
    writeLines(sprintf("DBtype=%s", dbType), myFile, sep = "\n")

    close(myFile)
}


## ----
#' @title Logic support for config files
#'
#' @description Provides logic checking for config files used in PHG creation.
#'
#' @param configFile Path to a configuration file for your graph database.
configCatcher <- function(configFile) {

    if (!file.exists(configFile)) {
        stop ("Path to config file does not exist.", call. = FALSE)
    }

    tmpLines  <- readLines(configFile)
    dbParam   <- tmpLines[grepl("DB=", tmpLines)]
    credParam <- tmpLines[grepl("user=|password=", tmpLines)]
    dbType    <- tmpLines[grepl("DBtype=", tmpLines)]

    if (!grepl("=postgres$|=sqlite$", dbType)) {
        stop("Only postgres or SQLite database types are allowed.", call. = FALSE)
    }

    if (length(credParam) != 2) {
        stop("Missing credentials (user= and/or password=) in config file.", call. = FALSE)
    }

    if (length(dbParam) > 1) {
        stop("Config file contains more than one database path parameter (DB=).", call. = FALSE)
    }

    dbParam <- gsub("DB=", "", dbParam)

    if (!file.exists(dbParam) && grepl("sqlite", dbType)) {
        stop("Path to database (DB=) in SQLite config file does not exist.", call. = FALSE)
    }
}


