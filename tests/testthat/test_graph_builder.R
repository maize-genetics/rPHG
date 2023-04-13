# === Tests for building graph objects ==============================

test_that("graphBuilder() returns correct data", {
    tmpFileLog <- tempfile(fileext = ".log")
    startLogger(tmpFileLog)
    
    tmpFile <- tempfile(fileext = ".txt")
    createConfigFile(tmpFile)

    expect_error(graphBuilder("does/not/exist"))

    expect_message(graphBuilder(tmpFile, methods = "CONSENSUS"))
    expect_message(graphBuilder(tmpFile, methods = "CONSENSUS", chrom = "1"))
    expect_message(graphBuilder(tmpFile, methods = "PATH_METHOD", buildType = "path"))
})


