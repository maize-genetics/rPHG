# === Test rPHG/BrAPI utitlities ====================================

test_that("parseJSON() returns correct exceptions and data", {
    urlGood <- "https://test-server.brapi.org/brapi/v2/serverinfo"
    urlBad  <- "fail"

    res <- parseJSON(urlGood)
    expect_true(is.data.frame(res) || is.list(res))

    expect_message(
        object = parseJSON(urlGood, verbose = TRUE),
        regexp = "Attempting to read endpoint"
    )

    expect_message(
        object = parseJSON(urlBad, verbose = TRUE),
        regexp = "URL could not be processed"
    )

    expect_silent(parseJSON(urlGood))

    expect_equal(
        object = length(parseJSON(urlGood)),
        expected = 3
    )

    expect_true(is.null(parseJSON(urlBad)))
})


test_that("json2tible() returns correct expections and data", {
    myCon <- BrapiCon(
        host = "test-server.brapi.org"
    )
    res <- class(json2tibble(myCon, "callsets"))
    expect_equal(
        object = res,
        expected = c("tbl_df", "tbl", "data.frame")
    )
})


test_that("getVTList() returns correct exceptions and data", {
    testCon <- BrapiCon("test-server.brapi.org", protocol = "https")
    bcPHG   <- PHGMethod(testCon, "test_method")

    expect_error(
        object = getVTList(mtcars),
        regexp = "A `BrapiConPHG` object is needed"
    )

    expect_equal(
        object = length(getVTList(bcPHG)),
        expected = 3
    )

    expect_equal(
        object = names(getVTList(bcPHG)),
        expected = c("rangeURL", "sampleURL", "tableURL")
    )

    expect_equal(
        object = getVTList(bcPHG)$rangeURL,
        expect = "https://test-server.brapi.org:443/brapi/v2/variantTables/test_method/variants"
    )

    expect_equal(
        object = getVTList(bcPHG)$sampleURL,
        expect = "https://test-server.brapi.org:443/brapi/v2/variantTables/test_method/samples"
    )

    expect_equal(
        object = getVTList(bcPHG)$tableURL,
        expect = "https://test-server.brapi.org:443/brapi/v2/variantTables/test_method/table"
    )
})




