test_that("Basic tests", {
    logFile       <- tempfile(fileext = ".txt")
    configFile    <- tempfile()
    configFileBad <- "not/a/path"
    # configFileBad <- tempfile()
    # writeLines(
    #     c(
    #         "host=localhost",
    #         "DBtype=sqlite",
    #         "DB=phg_db",
    #         "user=user",
    #         "password=pass"
    #     ),
    #     con = configFileBad
    # )

    startLogger(logFile)
    createConfigFile(configFile)

    phgLocCon <- PHGLocalCon(configFile)
    phgLocConOutput <- utils::capture.output(phgLocCon)

    expect_true(inherits(phgLocCon, "PHGCon"))
    expect_true(is(phgLocCon, "PHGLocalCon"))
    expect_error(
        object = PHGLocalCon(configFileBad),
        regexp = "Path to config file does not exist"
    )
    expect_equal(length(phgLocConOutput), 4)

    expect_true(is(configFilePath(phgLocCon), "character"))
    expect_true(is(dbName(phgLocCon), "character"))
    expect_true(is(dbType(phgLocCon), "character"))
    expect_true(is(showPHGMethods(phgLocCon), "tbl"))
})


