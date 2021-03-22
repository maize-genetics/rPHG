# === Test rPHG/BrAPI utitlities ====================================

## NOTE: make sure you are connected to Cornell's network

test_that("Utility functions return correct info.", {
    urlGood <- "http://cbsudc01.biohpc.cornell.edu:8080/brapi/v2/callsets"
    urlBad  <- "fail"

    ## Test 1 ----
    expect_error(
        object = parseJSON(urlBad),
        regexp = "BrAPI endpoint could not be parsed."
    )

    ## Test 2 ----
    res <- parseJSON(urlGood)
    expect_true(is.data.frame(res) || is.list(res))

    ## Test 3 ----
    myCon <- BrapiCon(
        host = "cbsudc01.biohpc.cornell.edu",
        port = 8080
    )
    res <- class(json2tibble(myCon, "callsets"))
    expect_equal(
        object = res,
        expected = c("tbl_df", "tbl", "data.frame")
    )

    ## Test 4 ----
    myCon <- BrapiCon(
        host = "localhost",
        port = 8080
    )
    res <- class(json2igraph(myCon, "CONSENSUS"))
    expect_equal(
        object = res,
        expected = "igraph"
    )
})



