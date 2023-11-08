# === Tests for `anchorDotPlot()` function ==========================

test_that("gvcfMetrics returns correct exceptions and data", {

    anchorFile <- system.file(
        "extdata", "dummy_anchors_small.anchorspro",
        package = "rPHG"
    )

    expect_true(is(plotDot(anchorFile), "ggplot"))
    expect_true(is(plotDot(anchorFile, colorId = "score"), "ggplot"))
    expect_true(is(plotDot(anchorFile, refSeqId = c("1", "2")), "ggplot"))
    expect_true(is(plotDot(anchorFile, querySeqId = c("1", "2")), "ggplot"))
    expect_error(plotDot(anchorFile, colordId = "not_a_column"))

})


