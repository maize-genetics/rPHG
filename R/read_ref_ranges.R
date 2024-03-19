## ----
# Get reference ranges from local connection
#
# @param conObj A PHG connection object
# @param conMethod A PHG database method ID
refRangesFromLocal <- function(conObj, conMethod) {
    phgObj <- graphFromPaths(configFilePath(conObj), conMethod)

    return(refRangesFromGraphObj(phgObj))
}


## ----
# Get reference ranges from server connection
#
# @param conObj A PHG connection object
# @param conMethod A PHG database method ID
# @param conDemo Is this method of type 'DEMO'
refRangesFromServer <- function(conObj, conMethod, conDemo) {
    finalUrl <- file.path(
        brapiURL(conObj),
        BRAPI_ENDPOINTS$VARIANT_TABLES,
        conMethod,
        sprintf(
            "%s?%s",
            BRAPI_ENDPOINTS$VARIANTS,
            sprintf(
                BRAPI_PARAMS$PAGE_SIZE,
                if (conDemo) {
                    BRAPI_PARAMS$DEMO_N_RR_TOTAL
                } else {
                    BRAPI_PARAMS$MAX_N_RR_TOTAL
                }
            )
        )
    )

    rrDf <- parseJSON(finalUrl)$result$data

    gr <- GenomicRanges::GRanges(
        seqnames = rrDf$referenceName,
        ranges = IRanges::IRanges(
            start = rrDf$start,
            end   = rrDf$end
        ),
        rr_id = paste0("R", rrDf$variantDbId)
    )

    return(gr)
}


## ----
# Get reference range data from `HaplotypeGraph` objects
#
# @param phgObj A PHG `HaplotypeGraph` object
refRangesFromGraphObj <- function(phgObj) {

    # Get reference range object from PHG object
    refRangeObj <- rJava::J(
        TASSEL_API$R_METHODS,
        "referenceRanges",
        phgObj
    )

    # Get data vectors and convert to tibble
    rrDf <- data.frame(
        lapply(
            X   = seq_along(refRangeObj$columnNames) - 1,
            FUN = function(i) {
                refRangeObj$dataVectors$get(as.integer(i))
            }
        )
    )
    names(rrDf) <- refRangeObj$columnNames

    gr <- GenomicRanges::GRanges(
        seqnames = rrDf$chr,
        ranges = IRanges::IRanges(
            start = rrDf$start,
            end   = rrDf$end
        ),
        rr_id = rrDf$id
    )

    return(gr)
}



