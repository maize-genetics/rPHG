# === Test BrAPI methods ============================================

test_that("BrapiCon methods return correct data", {
    testObj <- capture.output(BrapiCon(host = "test-server.brapi.org"))
    expect_equal(length(testObj), 5)

    testCon <- BrapiCon(host = "test-server.brapi.org")

    testObj <- serverInfo(testCon)
    expect_true(inherits(testObj, "data.frame"))

    testObj <- references(testCon)
    expect_true(inherits(testObj, "data.frame"))

    testObj <- referenceSets(testCon)
    expect_true(inherits(testObj, "data.frame"))
})


