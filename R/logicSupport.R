#' @title Logic support for config files
#'
#' @description Provides logic checking for config files used in PHG creation.
#'
#' @author Brandon Monier
#'
#' @param configFile Path to a configuration file for your graph database.
configCatcher <- function(configFile) {
    
    if (!file.exists(configPath)) {
        stop ("Path to config file does not exist.")
    }
    
    tmpLines <- readLines(configPath)
    dbParam <- tmpLines[grepl("DB=", tmp)]
    
    if (length(dbParam) == 0) {
        stop("DB parameter does not exist.")
    }
    
    if (length(dbParam) > 1) {
        stop("Config file contains more than one DB path.")
    }
    
    dbParam <- gsub("DB=", "", dbParam)
    
    if (!file.exists(dbParam)) {
        stop("Path to database does not exist.")
    }
}

