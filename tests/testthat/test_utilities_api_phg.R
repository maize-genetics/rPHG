test_that("Basic tests", {
    logFile    <- tempfile(fileext = ".txt")
    configFile <- tempfile()

    startLogger(logFile)
    createConfigFile(configFile)

    testOutput <- graphFromHaplotypes(
        configFile = configFile,
        method = "CONSENSUS",
        chrom = "1",
        includeSequence = FALSE,
        includeVariants = FALSE
    )

    expect_true(is(testOutput, "jobjRef"))
})


