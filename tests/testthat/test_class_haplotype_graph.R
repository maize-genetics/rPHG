test_that("Basic tests", {
    logFile    <- tempfile(fileext = ".txt")
    configFile <- tempfile()

    startLogger(logFile)
    createConfigFile(configFile)

    # testUrl <- "phg.maizegdb.org"
    testUrl <- "demo.hub.maizegenetics.net"

    phgLocCon <- PHGLocalCon(configFile)
    phgSrvCon <- PHGServerCon(testUrl)

    phgMethod1 <- PHGMethod(phgLocCon, "CONSENSUS")
    phgMethod2 <- PHGMethod(phgLocCon, "PATH_METHOD")
    phgMethod3 <- PHGMethod(phgSrvCon, "NAM_GBS_Alignments_PATHS")

    myGraph1 <- buildHaplotypeGraph(phgMethod1)
    myGraph2 <- buildHaplotypeGraph(phgMethod2)

    expect_true(is(myGraph1, "HaplotypeGraph"))
    expect_true(is(myGraph2, "HaplotypeGraph"))
    expect_error(buildHaplotypeGraph(phgMethod3), regexp = "Graphs can only")

    myGraph1Output <- utils::capture.output(myGraph1)
    expect_equal(length(myGraph1Output), 6)

    expect_true(is(javaMemoryAddress(myGraph1), "character"))
    expect_true(is(javaRefObj(myGraph1), "jobjRef"))

    expect_true(is(numberOfChromosomes(myGraph1), "numeric"))
    expect_equal(numberOfChromosomes(myGraph1), 1)

    expect_true(is(numberOfNodes(myGraph1), "numeric"))
    expect_equal(numberOfNodes(myGraph1), 30)

    expect_true(is(numberOfRefRanges(myGraph1), "numeric"))
    expect_equal(numberOfRefRanges(myGraph1), 10)

    expect_true(is(numberOfTaxa(myGraph1), "numeric"))
    expect_equal(numberOfTaxa(myGraph1), 6)

    expect_true(is(phgMethodId(myGraph1), "character"))
    expect_equal(phgMethodId(myGraph1), "CONSENSUS")

    expect_true(is(phgMethodType(myGraph1), "character"))
    expect_equal(phgMethodType(myGraph1), "CONSENSUS_ANCHOR_SEQUENCE")

    expect_true(is(readHaplotypeIds(myGraph1), "matrix"))
    expect_equal(dim(readHaplotypeIds(myGraph1)), c(6, 10))

    expect_true(is(readSamples(myGraph1), "character"))
    expect_equal(length(readSamples(myGraph1)), 6)

    expect_true(is(readRefRanges(myGraph1), "GRanges"))
    expect_true(is(readPHGDataSet(myGraph1), "PHGDataSet"))
})


