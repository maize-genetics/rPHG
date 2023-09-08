test_that("Basic tests.", {
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

    expect_true(is(testPDS, "PHGDataSet"))

    expect_true(is(calcMutualInfo(testPDS), "matrix"))
    expect_true(is(numHaploPerRefRange(testPDS), "tbl"))

    expect_equal(
        object = dim(calcMutualInfo(testPDS)),
        expected = c(10, 10)
    )
    expect_equal(
        object = colnames(numHaploPerRefRange(testPDS)),
        expected = c("rr_id", "seqnames", "start", "end", "width", "n_hap_ids")
    )
    expect_equal(
        object = dim(numHaploPerRefRange(testPDS)),
        expected = c(10, 6)
    )

})

