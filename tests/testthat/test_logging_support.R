test_that("Basic tests.", {
    expect_message(
        object = startLogger(),
        regexp = "PHG logging file created at"
    )
    expect_no_message(object = startLogger(verbose = FALSE))

})



