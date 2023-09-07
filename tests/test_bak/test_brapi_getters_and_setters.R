# === Test getter and setter methods for BrAPI classes ==============

testCon <- BrapiCon("test-server.brapi.org", protocol = "https")

test_that("getters return correct data", {
    expect_equal(host(testCon), "test-server.brapi.org")
    expect_equal(port(testCon), 443)
    expect_equal(protocol(testCon), "https")
    expect_equal(version(testCon), "v2")
    expect_true(is.na(token(testCon)))
})


