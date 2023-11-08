test_that("Basic tests", {
    logFile    <- tempfile(fileext = ".txt")
    configFile <- tempfile()

    startLogger(logFile)
    createConfigFile(configFile)
    phgLocCon <- PHGLocalCon(configFile)
    phgMethod <- PHGMethod(phgLocCon, "PATH_METHOD")
    phgDataSet <- readPHGDataSet(phgMethod)


    tbnOutput <- taxaByNode(phgDataSet, seqnames = "1", start = 1, end = 350000)

    expect_true(is(tbnOutput, "list"))
})


