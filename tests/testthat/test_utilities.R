# === Tests for utility methods =====================================

test_that("createConfigFile() generates correct output", {
    tmpFile <- tempfile(fileext = ".txt")
    createConfigFile(tmpFile)

    testObj <- readLines(tmpFile)
    unlink(tmpFile)

    expect_equal(length(testObj), 5)
})


test_that("configCatcher() returns correct data and exceptions", {
    tmpFile <- tempfile(fileext = ".txt")
    createConfigFile(tmpFile, dbType = "neo4j")
    expect_error(configCatcher(tmpFile))

    createConfigFile(tmpFile,  user = NULL)
    expect_error(
        object = configCatcher(tmpFile),
        regexp = "Missing credentials (user= and/or password=) in config file.",
        fixed = TRUE
    )

    createConfigFile(tmpFile,  password = NULL)
    expect_error(
        object = configCatcher(tmpFile),
        regexp = "Missing credentials (user= and/or password=) in config file.",
        fixed = TRUE
    )

    createConfigFile(tmpFile)
    myFile <- file(tmpFile, "a")
    writeLines("DB=another/path", myFile, sep = "\n")
    close(myFile)
    expect_error(
        object = configCatcher(tmpFile),
        regexp = "Config file contains more than one database path parameter (DB=).",
        fixed = TRUE
    )

    createConfigFile(tmpFile, dbPath = "not/here", dbType = "sqlite")
    expect_error(
        object = configCatcher(tmpFile),
        regexp = "Path to database (DB=) in SQLite config file does not exist.",
        fixed = TRUE
    )
})


