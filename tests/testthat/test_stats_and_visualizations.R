# === Tests for stats and visualization =============================

tmpFile <- tempfile(fileext = ".txt")
startLogger(tmpFile)


test_that("numHaploPerRange() returns correct data", {
    tmpFile <- tempfile(fileext = ".txt")
    createConfigFile(tmpFile)

    testPhgObj <- graphBuilder(tmpFile, "CONSENSUS")

    testReturn <- numHaploPerRange(testPhgObj)

    expect_true(inherits(testReturn, "DataFrame"))
    expect_equal(
        object = colnames(testReturn),
        expected = c("refRange_id", "seqnames", "start", "end", "width", "numHaplotypes")
    )

    expect_error(numHaploPerRange(testPhgObj, chr = "1", start = 1, end = 10))
})


test_that("plotNumHaplo() returns correct data", {
    tmpFile <- tempfile(fileext = ".txt")
    createConfigFile(tmpFile)

    testPhgObj <- graphBuilder(tmpFile, "CONSENSUS")

    testReturn <- numHaploPerRange(testPhgObj)

    expect_true(inherits(plotNumHaplo(testReturn), "ggplot"))

})


test_that("calcMutualInfo() returns correct data", {
    tmpFile <- tempfile(fileext = ".txt")
    createConfigFile(tmpFile)

    testPhgObj <- graphBuilder(tmpFile, "CONSENSUS")

    expect_error(calcMutualInfo(mtcars))
    expect_error(calcMutualInfo(phgObject = NULL, phgHapIDMat = NULL))
    expect_error(calcMutualInfo(testPhgObj))

    expect_true(inherits(calcMutualInfo(testPhgObj, 1:10), "matrix"))
})


test_that("plotMutualInfo() returns correct data", {
    tmpFile <- tempfile(fileext = ".txt")
    createConfigFile(tmpFile)

    testPhgObj <- graphBuilder(tmpFile, "CONSENSUS")

    expect_true(inherits(plotMutualInfo(testPhgObj, 1:10), "list"))
})


test_that("calcDiff() returns correct data", {
    tmpFile <- tempfile(fileext = ".txt")
    createConfigFile(tmpFile)

    testPhgObj <- graphBuilder(tmpFile, "CONSENSUS")

    expect_equal(calcDiff(c(1, 5, 10), c(1, 5, 10)), 0)

})


test_that("searchSimilarGametes() returns correct data", {
    tmpFile <- tempfile(fileext = ".txt")
    rPHG:::createConfigFile(tmpFile)

    testPhgObj <- graphBuilder(tmpFile, "CONSENSUS")

    expect_error(searchSimilarGametes(mtcars))
    expect_error(searchSimilarGametes(phgObject = NULL, phgHapIDMat = NULL))
    expect_error(searchSimilarGametes(testPhgObj, 1:3, "LineA111"))

    expect_equal(
        searchSimilarGametes(testPhgObj, 1:3, "LineA1"),
        "LineA"
    )
})


test_that("searchSimilarGametes() returns correct data", {
    tmpFile <- tempfile(fileext = ".txt")
    createConfigFile(tmpFile)

    testPhgObj <- graphBuilder(tmpFile, "CONSENSUS")

    expect_error(searchRecombination(mtcars))
    expect_error(searchRecombination(phgObject = NULL, phgHapIDMat = NULL))
})


