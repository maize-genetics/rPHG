## ----
# Get reference ranges from local connection
#
# @param conObj A PHG connection object
# @param conMethod A PHG database method ID
refRangesFromLocal <- function(conObj, conMethod) {
    phgObj <- graphFromPaths(configFilePath(conObj), conMethod)
    rrDf   <- refRangesFromGraphObj(phgObj)
    
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


## ----
# Get reference ranges from server connection
#
# @param conObj A PHG connection object
# @param conMethod A PHG database method ID
refRangesFromServer <- function(conObj, conMethod) {
    finalUrl <- file.path(
        brapiURL(conObj),
        BRAPI_ENDPOINTS$VARIANT_TABLES,
        conMethod,
        sprintf("%s?pageSize=%i", BRAPI_ENDPOINTS$VARIANTS, 150000)
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


