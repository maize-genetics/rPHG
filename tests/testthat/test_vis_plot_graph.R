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
    plotRes <- plotGraph(testPDS, start = 1, end = 350000, seqnames = "1")
    expect_true(is(plotRes, "visNetwork"))
    plotRes <- plotGraph(testPDS, start = 1, end = 350000, seqnames = "1", sampleHighlight = "RecLineB1RefA1gco4_wgs")
    expect_true(is(plotRes, "visNetwork"))
})


