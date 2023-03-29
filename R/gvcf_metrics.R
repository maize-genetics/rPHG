## ----
#' @title Generate metrics for GVCF data
#'
#' @param gvcfDir Directory containing GVCF file(s) to process.
#' @param indelReport Do you want to generate an indel size report? Defaults
#'    to \code{FALSE}.
#'
#' @importFrom rJava .jnew
#'
#' @export
gvcfMetrics <- function(gvcfDir = NULL, indelReport = FALSE) {
    rJC <- rJava::.jnew("net.maizegenetics.pangenome.hapCalling.VCFMetricsPlugin")

    myVcfStatFile <- tempfile(".tsv")
    myIndelStatFile <- tempfile("*.tsv")

    rJC$vcfDir(gvcfDir)
    rJC$outFile(myVcfStatFile)
    rJC$run()

    vcfStatsDf <- read.table(myVcfStatFile, header = TRUE)

    return(vcfStatsDf)
}


# ## Basic testing ----
# library(rJava)
# 
# if (Sys.info()["sysname"] == "Windows") {
#     rPHG::startLogger("c:/Users/brand/Downloads/gvcf_test/debug_log.txt")
#     gvcfDir <- "c:/Users/brand/Downloads/gvcf_test/"
# } else {
#     rPHG::startLogger("/home/bm646/Downloads/gvcf_metric_test/debug_log")
#     gvcfDir <- "/home/bm646/Downloads/gvcf_metric_test/"
# }
# 
# 
# myStats <- gvcfMetrics(gvcfDir = gvcfDir)


