# === Miscellaneous utilities for rPHG methods ======================

## ----
#' @title Create mock config file
#'
#' @param file User defined output file
createConfigFile <- function(file) {
    myFile <- file(file, "w")

    dbPath <- system.file(
        "extdata",
        "phg_smallseq_test.db",
        package = "rPHG"
    )

    writeLines("host=localhost", myFile, sep = "\n")
    writeLines("user=sqlite", myFile, sep = "\n")
    writeLines("password=sqlite", myFile, sep = "\n")
    writeLines(paste0("DB=", dbPath), myFile, sep = "\n")
    writeLines("DBtype=sqlite", myFile, sep = "\n")

    close(myFile)
}

