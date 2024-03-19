## ----
# Get reference ranges from local connection
#
# @param conObj A PHG connection object
# @param conMethod A PHG database method ID
hapIdsFromLocal <- function(conObj, conMethod) {
    pathsForMethod(
        configFilePath(conObj),
        conMethod
    )
}


## ----
# Get reference ranges from server connection
#
# @param conObj A PHG connection object
# @param conMethod A PHG database method ID
# @param conDemo Is this method of type 'DEMO'
hapIdsFromSever <- function(conObj, conMethod, conDemo) {
    bullet <- cli::col_grey(cli::symbol$info)
    verbInfo <- c(
        paste0("  ", bullet, " Determining page size..."),
        paste0("  ", bullet, " Retrieving data..."),
        paste0("  ", bullet, " Cleaning up data...")
    )

    brapiUrl <- brapiURL(conObj)

    maxRRSize <- ifelse(
        test = conDemo,
        yes  = BRAPI_PARAMS$DEMO_N_RR_SIZE,
        no   = BRAPI_PARAMS$MAX_N_RR_SIZE
    )
    maxSampleSize <- ifelse(
        test = conDemo,
        yes  = BRAPI_PARAMS$DEMO_N_SAMPLES,
        no   = BRAPI_PARAMS$MAX_N_SAMPLES
    )

    initRespUrl <- file.path(
        brapiUrl,
        amUrlContextStringBuilder(
            methodId       = conMethod,
            rrPageSize     = maxRRSize,
            rrPage         = 0,
            samplePageSize = maxSampleSize,
            samplePage     = 0
        )
    )

    message(verbInfo[1])
    initResp <- parseJSON(initRespUrl)
    pageSizeDf <- initResp$result$pagination

    totalPages <- ifelse(
        test = conDemo,
        yes  = BRAPI_PARAMS$DEMO_N_RR_TOTAL / BRAPI_PARAMS$DEMO_N_RR_SIZE,
        no   = pageSizeDf[pageSizeDf$dimension == "VARIANTS", "totalPages"]
    )

    respVector <- vector("list", length = totalPages)
    respVector[[1]] <- initResp

    respUrls <- file.path(
        brapiUrl,
        amUrlContextStringBuilder(
            methodId       = conMethod,
            rrPageSize     = maxRRSize,
            rrPage         = seq(2, totalPages) - 1,
            samplePageSize = maxSampleSize,
            samplePage     = 0
        )
    )


    message(verbInfo[2])
    pb <- utils::txtProgressBar(
        min     = 0,
        max     = totalPages,
        initial = 1,
        width   = 30,
        char    = "=",
        style   = 3
    )
    i <- 2
    for (url in respUrls) {
        respVector[[i]] <- parseJSON(url)
        utils::setTxtProgressBar(pb, i)
        i <- i + 1
    }
    close(pb)

    message(verbInfo[3])
    fullResp <- do.call(
        what = "rbind",
        args = lapply(
            X   = respVector,
            FUN = function(x) {
                x$result$dataMatrices$dataMatrix[[1]]
            }
        )
    )

    rownames(fullResp) <- paste0("R", seq_len(nrow(fullResp)))
    colnames(fullResp) <- samplesFromServer(conObj, conMethod, conDemo)

    return(t(fullResp))
}


## ----
# Return Hap ID matrix from `HaplotypeGraph` objects
#
# @param phgObj A PHG `HaplotypeGraph` object
hapIdsFromGraphObj <- function(phgObject) {

    # Pull hap ID matrix from phg object
    hapids <- rJava::J(
        TASSEL_API$R_METHODS,
        "hapidTableAsMatrix",
        phgObject
    )
    hapidMatrix <- hapids$matrix

    # Get row and column names (if available)
    if(!rJava::is.jnull(hapids$rowNames)) {
        rownames(hapidMatrix) <- hapids$rowNames
    }
    if(!rJava::is.jnull(hapids$columnNames)) {
        colnames(hapidMatrix) <- hapids$columnNames
    }

    # Return the matrix
    return(hapidMatrix)
}


