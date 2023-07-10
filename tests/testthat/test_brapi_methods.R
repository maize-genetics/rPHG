# === Test BrAPI methods ============================================

test_that("BrapiCon() constructor returns correct data", {
    testObj <- capture.output(BrapiCon(host = "test-server.brapi.org"))
    expect_equal(length(testObj), 5)

    testCon <- BrapiCon(host = "test-server.brapi.org")

    testObj <- serverInfo(testCon)
    expect_true(inherits(testObj, "data.frame"))

    testObj <- references(testCon)
    expect_true(inherits(testObj, "data.frame"))

    testObj <- referenceSets(testCon)
    expect_true(inherits(testObj, "data.frame"))
})


test_that("availablePHGMethods() returns correct data", {
    urlTest <- "phg.maizegdb.org"
    testCon <- BrapiCon(urlTest)

    expect_true(is(availablePHGMethods(testCon), "tbl"))
    expect_true(is(availablePHGMethods(testCon), "tbl_df"))
    expect_true(is(availablePHGMethods(testCon), "data.frame"))

    expect_equal(
        object = colnames(availablePHGMethods(testCon)),
        expected = c("variantTableDbId","numVariants", "numSamples", "additionalInfo")
    )
})


test_that("BrapiConPHG() constructor returns correct data", {
    urlTest <- "test-server.brapi.org"
    testCon <- BrapiCon(urlTest)
    bcPHG   <- capture.output(PHGMethod(testCon, "test_method"))
    expect_equal(length(bcPHG), 4)
})


test_that("filterRefRanges() returns correct data", {
    urlTest <- "test-server.brapi.org"
    testMethod <- "test_method"

    testCon <- BrapiCon(urlTest)
    bcPHG   <- PHGMethod(testCon, testMethod)

    testGR <- GenomicRanges::GRanges(
        seqnames = "1",
        ranges = IRanges::IRanges(5, 10)
    )

    # Equality
    expect_equal(
        object = filterRefRanges(bcPHG, chromosome = 1)@refRangeFilter,
        expected = "ranges=1"
    )

    expect_equal(
        object = filterRefRanges(
            x          = bcPHG,
            chromosome = 1,
            start      = 10,
            end        = 50
        )@refRangeFilter,
        expected = "ranges=1:10-50"
    )

    expect_equal(
        object = filterRefRanges(
            x          = bcPHG,
            chromosome = c(1, 2),
            start      = c(10, 23),
            end        = c(50, 70)
        )@refRangeFilter,
        expected = "ranges=1:10-50,2:23-70"
    )


    # Error checks
    expect_error(
        object = filterRefRanges(mtcars, chromosome = "1"),
        regexp = "A `BrapiConPHG` object is needed"
    )

    expect_error(
        object = filterRefRanges(
            x     = bcPHG,
            start = c(10, 23),
            end   = c(50, 70)
        ),
        regexp = "Incorrect filtration"
    )

    expect_error(
        object = filterRefRanges(
            x          = bcPHG,
            chromosome = c(1, 2),
            start      = c(10, 23)
        ),
        regexp = "Incorrect filtration"
    )

    expect_error(
        object = filterRefRanges(
            x          = bcPHG,
            chromosome = c(1, 2),
            start      = c(10, 23),
            end        = c(50, 100, 150)
        ),
        regexp = "Range vectors do not have the same"
    )


    # GRanges tests
    expect_equal(
        object = filterRefRanges(
            x  = bcPHG,
            gr = testGR
        )@refRangeFilter,
        expected = "ranges=1:5-10"
    )

    expect_equal(
        object = filterRefRanges(
            x          = bcPHG,
            gr         = testGR,
            chromosome = "3"
        )@refRangeFilter,
        expected = "ranges=3,1:5-10"
    )

    expect_error(
        object = filterRefRanges(
            x  = bcPHG,
            gr = mtcars,
        ),
        regexp = "Not a valid GRanges"
    )
})


test_that("filterSamples() returns correct data", {
    urlTest <- "test-server.brapi.org"
    testMethod <- "test_method"

    testCon <- BrapiCon(urlTest)
    bcPHG   <- PHGMethod(testCon, testMethod)

    expect_error(
        object = filterSamples(mtcars, samples = "taxa_A"),
        regexp = "A `BrapiConPHG` object is needed"
    )

    expect_error(
        object = filterSamples(bcPHG, samples = mtcars),
        regexp = "`samples` argument must be an"
    )
})


test_that("readRefRanges() returns correct data", {
    urlTest    <- "cbsudc01.biohpc.cornell.edu"
    testMethod <- "NonMergedReadMapping_AllNamParents_Haploid"

    testCon       <- BrapiCon(urlTest)
    bcPHGNoFilter <- PHGMethod(testCon, testMethod)
    bcPHGFilter   <- filterRefRanges(
        x = PHGMethod(testCon, testMethod),
        chromosome = "1",
        start = "1",
        end = "500000"
    )

    grRes <- readRefRanges(bcPHGFilter)
    expect_true(inherits(grRes, "GRanges"))

    expect_true(all(as.data.frame(grRes)$seqnames == 1))
})


test_that("readSamples() returns correct data", {
    urlTest    <- "phg.maizegdb.org"
    testMethod <- "anchorwave_gapfilled_assembly_PATH"

    testCon       <- BrapiCon(urlTest)
    bcPHGNoFilter <- PHGMethod(testCon, testMethod)
    sampleRes <- readSamples(bcPHGNoFilter)

    expect_true(inherits(sampleRes, "data.frame"))
    expect_equal(nrow(sampleRes), 5)
})


test_that("readTable() returns correct data", {
    urlTest    <- "phg.maizegdb.org"
    testMethod <- "DEMO"

    testCon       <- BrapiCon(urlTest)
    bcPHGNoFilter <- PHGMethod(testCon, testMethod)
    # bcPHGFilter   <- filterSamples(
    #     x = PHGMethod(testCon, testMethod),
    #     samples = c("Z001E0001-628NHAAXX_1", "Z001E0001-D10RTACXX_5")
    # )

    expect_message(readTable(bcPHGNoFilter))
})


test_that("readPHGDatasetFromBrapi() returns correct data", {
    urlTest    <- "phg.maizegdb.org"
    testMethod <- "DEMO"

    testCon       <- BrapiCon(urlTest)
    bcPHGNoFilter <- PHGMethod(testCon, testMethod)

    expect_message(readPHGDatasetFromBrapi(bcPHGNoFilter))
})


