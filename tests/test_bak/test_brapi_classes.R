# === Tests for BrAPI-related classes ===============================

test_that("BrapiCon() constructuor returns correct exceptions and data", {
    expect_error(
        object = BrapiCon(),
        regexp = "A URL host is needed to make this class.",
        fixed = TRUE
    )


    expect_error(
        object = BrapiCon(host = "test-server.brapi.org", port = 80.5),
        regexp = "Invalid port number. Must be a whole number.",
        fixed = TRUE
    )
})


test_that("PHGMethod() constructure returns correct expections and data", {
    tmpBrapiCon <- BrapiCon(host = "test-server.brapi.org")
    expect_no_message(PHGMethod(tmpBrapiCon, "custom-method"))
})


