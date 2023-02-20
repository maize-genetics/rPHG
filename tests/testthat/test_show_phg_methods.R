# === Tests to display available PHG methods ========================

tmpFile <- tempfile(fileext = ".txt")
startLogger(tmpFile)


test_that("showPHGMethods() returns correct data", {
    tmpFile <- tempfile(fileext = ".txt")
    rPHG:::createConfigFile(tmpFile)

    testReturn <- showPHGMethods(tmpFile)

    expect_true(inherits(testReturn, "data.frame"))
    expect_equal(
        object = colnames(testReturn),
        expected = c("method_id", "method_type", "type_name", "method_name", "description")
    )
})


