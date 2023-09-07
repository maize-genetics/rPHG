# === Tests for logging support =====================================

test_that("startLogger() will return correct exceptions and data", {
    tmpFile <- tempfile(fileext = ".txt")

    expect_message(
        startLogger(tmpFile),
        regexp = "PHG logging file created at: "
    )

    expect_error(
        startLogger("~/test_log.txt"),
        regexp = "It seems that you are using"
    )

    startLogger()
    expect_true(file.exists("rPHG_log"))
})


