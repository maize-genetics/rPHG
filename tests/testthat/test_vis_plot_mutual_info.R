test_that("Basic tests", {
    logFile    <- tempfile(fileext = ".txt")
    configFile <- tempfile()

    startLogger(logFile)
    createConfigFile(configFile)

    testPDS <- readPHGDataSet(
        PHGMethod(
            PHGLocalCon(configFile),
            "PATH_METHOD"
        )
    )

    plotResults <- plotMutualInfo(testPDS)

    expect_true(is(plotResults, "gg"))
})


