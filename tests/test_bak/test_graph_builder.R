# === Tests for building graph objects ==============================

tmpFile <- tempfile(fileext = ".txt")
startLogger(tmpFile)


test_that("graphBuilder() returns correct data", {
    tmpFile <- tempfile(fileext = ".txt")
    createConfigFile(tmpFile)

    expect_error(graphBuilder("does/not/exist"))

    expect_message(graphBuilder(tmpFile, methods = "CONSENSUS"))
    expect_message(graphBuilder(tmpFile, methods = "CONSENSUS", chrom = "1"))
    expect_message(graphBuilder(tmpFile, methods = "PATH_METHOD", buildType = "path"))
})

