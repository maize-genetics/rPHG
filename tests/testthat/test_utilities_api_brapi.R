test_that("Basic tests", {
    testUrl1 <- "https://www.google.com"
    testUrl2 <- "https://phg.maizegdb.org/brapi/v2/serverinfo"

    expect_null(parseJSON(testUrl1))
    expect_message(parseJSON(testUrl1, verbose = TRUE))
    expect_message(parseJSON(testUrl1, verbose = TRUE))
    expect_message(parseJSON(testUrl2, verbose = TRUE))
})



