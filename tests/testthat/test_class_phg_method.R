test_that("Basic tests.", {
    logFile    <- tempfile(fileext = ".txt")
    configFile <- tempfile()

    startLogger(logFile)
    createConfigFile(configFile)

    testUrl <- "demo.hub.maizegenetics.net"

    phgLocCon <- PHGLocalCon(configFile)
    phgSrvCon <- PHGServerCon(testUrl)

    phgMethod1 <- PHGMethod(phgLocCon, "CONSENSUS")
    phgMethod2 <- PHGMethod(phgLocCon, "PATH_METHOD")
    phgMethod3 <- PHGMethod(phgSrvCon, "NAM_GBS_Alignments_PATHS")

    phgMethod1Output <- utils::capture.output(phgMethod1)
    phgMethod2Output <- utils::capture.output(phgMethod2)
    phgMethod3Output <- utils::capture.output(phgMethod3)

    expect_true(is(phgMethod1, "PHGMethod"))
    expect_true(is(phgMethod2, "PHGMethod"))
    expect_true(is(phgMethod3, "PHGMethod"))
    expect_true(any(grepl("PHGLocalCon",  phgMethod1Output)))
    expect_true(any(grepl("PHGLocalCon",  phgMethod2Output)))
    expect_true(any(grepl("PHGServerCon", phgMethod3Output)))
    expect_true(is(readSamples(phgMethod2),      "character"))
    expect_true(is(readRefRanges(phgMethod2),    "GRanges"))
    expect_true(is(readHaplotypeIds(phgMethod2), "matrix"))
    expect_true(is(readPHGDataSet(phgMethod2),   "PHGDataSet"))

    expect_equal(length(phgMethod1Output), 2)
    expect_equal(length(phgMethod2Output), 2)
    expect_equal(length(phgMethod3Output), 2)
    expect_equal(phgMethodId(phgMethod1), "CONSENSUS")
    expect_equal(phgMethodId(phgMethod2), "PATH_METHOD")
    expect_equal(phgMethodId(phgMethod3), "NAM_GBS_Alignments_PATHS")


})