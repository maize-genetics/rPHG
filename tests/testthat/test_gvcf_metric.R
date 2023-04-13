# === Tests for `gvcfMetrics()` function ============================

test_that("gvcfMetrics returns correct exceptions and data", {
    startLogger()
    gvcfDir <- system.file("extdata", "gvcf", package = "rPHG")
    
    testObj1 <- gvcfMetrics(gvcfDir)
    testObj2 <- gvcfMetrics(gvcfDir, indelReport = TRUE)
    
    expect_true(is(testObj1, "data.frame"))
    expect_true(is(testObj2, "list"))
})


