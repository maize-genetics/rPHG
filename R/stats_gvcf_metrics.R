## ----
#' @title
#' Generate metrics for GVCF data
#'
#' @param gvcfDir
#' Directory containing GVCF file(s) to process.
#' @param indelReport
#' Do you want to generate an indel size report? Defaults to \code{FALSE}.
#'
#' @importFrom rJava .jnew
#'
#' @export
gvcfMetrics <- function(gvcfDir, indelReport = FALSE) {
    rJC <- rJava::.jnew("net.maizegenetics.pangenome.hapCalling.VCFMetricsPlugin")

    myVcfStatFile <- tempfile(".tsv")
    myIndelStatFile <- tempfile(".tsv")

    rJC$vcfDir(gvcfDir)
    rJC$outFile(myVcfStatFile)

    if (indelReport) {
        rJC$indelFile(myIndelStatFile)
    }

    rJC$run()

    vcfStatsDf <- utils::read.table(myVcfStatFile, header = TRUE)

    if (indelReport) {
        indelStatsDf <- utils::read.table(myIndelStatFile, header = FALSE, fill = TRUE)
    }

    if (indelReport) {
        return(
            list(
                "gvcf_stats"  = vcfStatsDf,
                "indel_stats" = indelStatsDf
            )
        )
    } else {
        return(vcfStatsDf)
    }
}


