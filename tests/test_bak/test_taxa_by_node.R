# === Tests for taxaByNode() ========================================

tmpFile <- tempfile(fileext = ".txt")
startLogger(tmpFile)


test_that("graphBuilder() returns correct data", {
    tmpFile <- tempfile(fileext = ".txt")
    createConfigFile(tmpFile)

    myGraph <- graphBuilder(tmpFile, "CONSENSUS")

    rangeIds <- c(1, 3)

    qTaxa <- taxaByNode(myGraph, rrSet = rangeIds)

    expect_true(is(qTaxa, "tbl_df"))
    expect_equal(nrow(qTaxa), 6)
    expect_equal(colnames(qTaxa), c("ref_range_id", "hap_id", "taxa_id"))
    expect_equal(unique(qTaxa$ref_range_id), c("1", "3"))

    expect_error(
        object = taxaByNode(myGraph, start = NULL, end = 35000, seqnames = "1"),
        regexp = "Genomic range parameters are needed"
    )
    expect_error(
        object = taxaByNode(myGraph, start = 1, end = NULL, seqnames = "1"),
        regexp = "Genomic range parameters are needed"
    )
    expect_error(
        object = taxaByNode(myGraph, start = 1, end = 35000, seqnames = NULL),
        regexp = "Genomic range parameters are needed"
    )

    qTaxa2 <- taxaByNode(myGraph, start = 1, end = 35000, seqnames = "1")
    expect_equal(nrow(qTaxa2), 18)
    expect_equal(
        unique(qTaxa2$ref_range_id),
        c("1", "2", "3", "4", "5", "6")
    )

    qTaxa3 <- taxaByNode(myGraph, 1, 35000, NULL, rrSet = rangeIds)
    expect_true(is(qTaxa3, "tbl_df"))
    expect_equal(nrow(qTaxa3), 6)
    expect_equal(colnames(qTaxa3), c("ref_range_id", "hap_id", "taxa_id"))
    expect_equal(unique(qTaxa3$ref_range_id), c("1", "3"))
})


