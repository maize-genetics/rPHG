test_that("Basic tests", {
    phgHapIDMat <- matrix(
        data = "111/111",
        nrow = 5,
        ncol = 5
    )
    colnames(phgHapIDMat) <- paste0("R", seq_len(ncol(phgHapIDMat)))
    rownames(phgHapIDMat) <- paste0("sample_", letters[seq_len(nrow(phgHapIDMat))])

    miResults <- mutualInfoPair(phgHapIDMat, c("R1", "R2"))

    expect_equal(miResults, 0)
})


