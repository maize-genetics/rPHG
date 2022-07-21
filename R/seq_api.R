#####################################################################
##
## Overview:
##  This file house methods to obtain sequence information from
##  either a local PHG instance (`PHGDataSet`) or a BrAPI/PHG
##  pointer object (`BrapiConPHG`)
##
#####################################################################


## ----
#' @title Retrieve sequence data from rPHG data objects by reference range
#'
#' @description Returns sequence information for a user-defined set of
#'    reference ranges from either a local PHG database object
#'    (\code{PHGDataSet}) or a BrAPI connection pointing to a specific PHG
#'    method (\code{BrapiConPHG}).
#'
#' @param object A \code{PHGDataSet} or \code{BrapiConPHG} object.
#' @param refRanges A numeric vector of reference range IDs.
#' @param gRanges A \code{GRanges} object.
#'
#' @rdname getSequencesByRefRange
#'
#' @return A \code{Biostrings::DNAStringSet} object
#'
#' @export
setGeneric(
    name = "getSequencesByRefRange",
    def  = function(object, refRanges = NULL, gRanges = NULL) {
        standardGeneric("getSequencesByRefRange")
    }
)

#' @rdname getSequencesByRefRange
#' @export
setMethod(
    f = "getSequencesByRefRange",
    signature = "PHGDataSet",
    definition = function(object, refRanges = NULL, gRanges = NULL) {
        jPhgObj <- S4Vectors::metadata(object)$jObj

        if (is.null(refRanges) && is.null(gRanges)) {
            stop("Need to specify either specific ref ranges or a GRanges object")
        }

        if (is.null(gRanges)) {
            refRanges <- as.integer(refRanges)
        } else {
            if (class(gRanges) != "GRanges") {
                stop("gRanges parameter needs to be a GRanges object")
            }
            expectRanges <- rowRanges(phgObj)
            validRows <- unique(
                S4Vectors::subjectHits(
                    IRanges::findOverlaps(
                        query = testRanges,
                        subject = expectRanges
                    )
                )
            )
            if (length(validRows) == 0) {
                stop("No data returned")
            }
            filterRanges <- expectRanges[validRows]
            refRanges <- as.integer(gsub("R", "", filterRanges$refRange_id))
            # print(refRanges)
        }

        # Get Java method
        rJC <- rJava::.jnew("net/maizegenetics/pangenome/pipelineTests/GenerateRForPHG")
        seqRes <- rJC$graphToHapsInRefRangeVectors(
            jPhgObj, rJava::.jarray(refRanges),
            TRUE,
            FALSE
        )

        # Get ref range metadata
        hapRR <- rowRanges(myPHG)
        rrDf <- as.data.frame(hapRR[hapRR$refRange_id %in% paste0("R", seqRes$refRangeIds)])

        # Return Biostrings data
        if (length(seqRes$hapIds) == 0) {
            message("No sequences found for given parameters.")
            return(NULL)
        } else {
            myStrings <- Biostrings::DNAStringSet(seqRes$sequence)
            names(myStrings) <- paste0(
                seqRes$hapIds, " ",
                seqRes$taxa, " ",
                as.character(rrDf$seqnames), ":",
                rrDf$start, "-", rrDf$end
            )
            return(myStrings)
        }
    }
)


#' @rdname getSequencesByRefRange
#' @export
setMethod(
    f = "getSequencesByRefRange",
    signature = "BrapiConPHG",
    definition = function(object, refRanges = NULL, gRanges = NULL) {
        cat("Placeholder for a BrapiConPHG object\n")
        cat("  RR: ", refRanges, "\n")
        cat("  GR: ", gRanges, "\n")
    }
)



## ----
#' @title Retrieve sequence data from rPHG data objects by haplotype ID
#'
#' @description Returns sequence information for a user-defined set of
#'    haplotype IDs from either a local PHG database object
#'    (\code{PHGDataSet}) or a BrAPI connection pointing to a specific PHG
#'    method (\code{BrapiConPHG}).
#'
#' @param object A \code{PHGDataSet} or \code{BrapiConPHG} object.
#' @param hadId A numeric vector of haplotype IDs.
#'
#' @rdname getSequencesByHapId
#'
#' @return A \code{Biostrings::DNAStringSet} object
#'
#' @export
setGeneric(
    name = "getSequencesByHapId",
    def  = function(object, hapId) {
        standardGeneric("getSequencesByHapId")
    }
)

#' @rdname getSequencesByHapId
#' @export
setMethod(
    f = "getSequencesByHapId",
    signature = "PHGDataSet",
    definition = function(object, hapId) {
        jPhgObj <- S4Vectors::metadata(object)$jObj

        if (!is.numeric(hapId)) {
            stop("Haplotype ID values must be numeric.")
        }
        hapId <- as.integer(hapId)

        rJC <- rJava::.jnew("net/maizegenetics/pangenome/api/RPHGMethodsKotlin")
        seqRes <- rJC$getHapIdSeq(jPhgObj, .jarray(hapId))
        hapAssay <- SummarizedExperiment::assay(object)
        hapRR <- SummarizedExperiment::rowRanges(object)
        metaDf <- data.frame(
            hap_id = NA, taxa = NA, seqname = NA, start = NA, end = NA, rr_id = NA
        )

        if (seqRes$size() == 0) {
            message("No sequences found for given parameters.")
            return(NULL)
        } else {
            # @TODO - get method to parse data class...
            # @TODO - vectorize this on the JVM end...
            myStringSet <- Biostrings::DNAStringSet()
            for (i in seq_len(seqRes$size())) {
                tmpRow <- seqRes$get(as.integer(i - 1))

                # Get taxa (col names) from assay
                taxaVal <- colnames(hapAssay)[which(hapAssay == tmpRow$getHapId(), arr.ind = TRUE)[, 2]]
                taxaVal <- paste(taxaVal, collapse = ";")
                # Get ref range (row names) values from assay
                rrVal   <- rownames(hapAssay)[which(hapAssay == tmpRow$getHapId(), arr.ind = TRUE)[, 1]]
                rrDf    <- as.data.frame(hapRR[hapRR$refRange_id ==  rrVal])

                myStringSet[[i]] <- tmpRow$getSequence()
                names(myStringSet)[i] <- paste0(
                    tmpRow$getHapId(), " ",
                    taxaVal, " ",
                    as.character(rrDf$seqnames), ":",
                    rrDf$start, "-", rrDf$end
                )

                # add metadata
                metaDf[i, "hap_id"]  <- tmpRow$getHapId()
                metaDf[i, "taxa"]    <- taxaVal
                metaDf[i, "seqname"] <- as.character(rrDf$seqnames)
                metaDf[i, "start"]   <- rrDf$start
                metaDf[i, "end"]     <- rrDf$end
                metaDf[i, "rr_id"]   <- gsub("R", "", rrVal)
            }
            S4Vectors::metadata(myStringSet)$seqInfo <- metaDf
            return(myStringSet)
        }
    }
)

#' @rdname getSequencesByHapId
#' @export
setMethod(
    f = "getSequencesByHapId",
    signature = "BrapiConPHG",
    definition = function(object, hapId = NULL) {
        cat("Placeholder for a BrapiConPHG object\n")
        cat("  Hap ID: ", hapId, "\n")
    }
)


