# === Tests for path retrieval methods ==============================

tmpFile <- tempfile(fileext = ".txt")
startLogger(tmpFile)

test_that("pathsForMethod() returns correct data", {
    tmpFile <- tempfile(fileext = ".txt")
    rPHG:::createConfigFile(tmpFile)
    testPath <- "GATK_PIPELINE_PATH"

    expect_error(pathsForMethod(mtcars))

    expect_true(inherits(pathsForMethod(tmpFile, testPath), "matrix"))

    expect_equal(
        object = dim(pathsForMethod(tmpFile, testPath)),
        expected = c(6, 10)
    )
})


test_that("readMappingsForLineName() returns correct data", {
    tmpFile <- tempfile(fileext = ".txt")
    rPHG:::createConfigFile(tmpFile)
    lineName <- "RefA1_gbs"
    readMappingMethodName <- "HAP_COUNT_METHOD"
    haplotypeMethodName <- "CONSENSUS"

    expect_true(
        inherits(
            readMappingsForLineName(
                tmpFile,
                lineName,
                readMappingMethodName,
                haplotypeMethodName
            ),
            "DataFrame"
        )
    )
})


test_that("readMappingTableInfo() returns correct data", {
    tmpFile <- tempfile(fileext = ".txt")
    rPHG:::createConfigFile(tmpFile)
    expect_true(inherits(readMappingTableInfo(tmpFile), "DataFrame"))
})


