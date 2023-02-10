# === Tests for utility methods =====================================

test_that("createConfigFile() generates correct output", {
    tmpFile <- tempfile(fileext = ".txt")
    createConfigFile(tmpFile)

    testObj <- readLines(tmpFile)
    unlink(tmpFile)

    expect_equal(length(testObj), 5)
})


