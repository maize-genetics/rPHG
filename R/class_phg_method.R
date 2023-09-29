## ----
#' @title A PHGMethod Class
#'
#' @description
#' Class \code{PHGMethod} defines a \code{rPHG} Class for storing
#' a "committed" PHG method to return data against.
#'
#' @slot methodID A PHG method identifier.
#' @slot phgConObj A \code{\linkS4class{PHGCon}} object
#'
#' @name PHGMethod-class
#' @rdname PHGMethod-class
#' @exportClass PHGMethod
setClass(
    Class = "PHGMethod",
    slots = c(
        methodID  = "character",
        phgConObj = "PHGCon",
        isDemo    = "logical"
    ),
    prototype = list(
        methodID  = "test",
        phgConObj = new("PHGCon", phgType = "local", host = "localHost"),
        isDemo    = FALSE
    )
)


## ----
#' @title PHGMethod validation
#'
#' @name PHGMethod-validity
#'
#' @description Checks if \code{PHGMethod} class objects are valid.
#'
#' @param object A \code{PHGMethod} object.
#'
#' @importFrom curl has_internet
setValidity("PHGMethod", function(object) {
    errors <- character()

    methodIds <- showPHGMethods(
        object = phgConObj(object),
        showAdvancedMethods = TRUE
    )$method_name
    methodId  <- phgMethodId(object)

    if (!methodId %in% methodIds && !isDemo(object)) {
        msg <- "Method ID not found in database."
        errors <- c(errors, msg)
    }

    if (phgType(phgConObj(object)) != "server" && isDemo(object)) {
        msg <- "DEMO method can only be used for server connections"
        errors <- c(errors, msg)
    }

    if (length(errors) == 0) TRUE else errors
})


## ----
#' @title Helper function to construct PHGMethod object
#'
#' @description
#' Creates a \code{\linkS4class{PHGMethod}} object to be used to read and
#' filter data from a given PHG connection object using a verified PHG method.
#'
#' @param phgConObj A \code{\linkS4class{PHGCon}} object.
#' @param methodID A PHG method identifier.
#'
#' @export
PHGMethod <- function(phgConObj, methodId) {

    demoMethodId <- "DEMO"

    # For demo purposes only! (useful for workshops)
    trueMethodId <- ifelse(
        test = methodId == demoMethodId,
        yes  = "NAM_GBS_Alignments_PATHS",
        no   = methodId
    )

    methods::new(
        Class     = "PHGMethod",
        methodID  = trueMethodId,
        phgConObj = phgConObj,
        isDemo    = methodId == demoMethodId
    )
}



# /// Methods (show) ////////////////////////////////////////////////

## ----
#' @title Show method for PHGMethod objects
#'
#' @description
#' Prints out information regarding properties from the \code{PHGMethod}
#' class to the console
#'
#' @param object a \code{\linkS4class{PHGMethod}} object.
#'
#' @docType methods
#' @name show
#' @rdname show
#' @aliases show,PHGMethod-method
setMethod(
    f = "show",
    signature = "PHGMethod",
    definition = function(object) {
        conType <- phgType(phgConObj(object))

        conMsg <- switch (conType,
            "server" = cli::style_bold(cli::col_green("PHGServerCon")),
            "local"  = cli::style_bold(cli::col_green("PHGLocalCon"))
        )

        methodId <- cli::style_bold(
            cli::col_blue(
                if (isDemo(object)) "DEMO Method" else phgMethodId(object)
            )
        )

        msg <- c(
            paste0("A ", cli::style_bold("PHGMethod"), " promise object:"),
            paste0("  <", conMsg, "> --- <", methodId, ">")
        )

        cat(msg, sep = "\n")
    }
)



# /// Methods (general) /////////////////////////////////////////////

## ----
#' @rdname isDemo
#' @export
setMethod(
    f = "isDemo",
    signature = signature(object = "PHGMethod"),
    definition = function(object) {
        return(object@isDemo)
    }
)


## ----
#' @rdname phgConObj
#' @export
setMethod(
    f = "phgConObj",
    signature = signature(object = "PHGMethod"),
    definition = function(object) {
        return(object@phgConObj)
    }
)


## ----
#' @rdname phgMethodId
#' @export
setMethod(
    f = "phgMethodId",
    signature = signature(object = "PHGMethod"),
    definition = function(object) {
        return(object@methodID)
    }
)


## ----
#' @rdname readRefRanges
#' @export
setMethod(
    f = "readRefRanges",
    signature = signature(object = "PHGMethod"),
    definition = function(object) {
        conObj    <- phgConObj(object)
        conType   <- phgType(conObj)
        conMethod <- phgMethodId(object)
        conDemo   <- isDemo(object)

        if (conType == "local") {
            refRangesFromLocal(conObj, conMethod)
        } else if (conType == "server") {
            refRangesFromServer(conObj, conMethod, conDemo)
        }
    }
)


## ----
#' @rdname readSamples
#' @export
setMethod(
    f = "readSamples",
    signature = signature(object = "PHGMethod"),
    definition = function(object) {
        conObj    <- phgConObj(object)
        conType   <- phgType(conObj)
        conMethod <- phgMethodId(object)
        conDemo   <- isDemo(object)

        if (conType == "local") {
            samplesFromLocal(conObj, conMethod)
        } else if (conType == "server") {
            samplesFromServer(conObj, conMethod, conDemo)
        }
    }
)


## ----
#' @rdname readHaplotypeIds
#' @export
setMethod(
    f = "readHaplotypeIds",
    signature = signature(object = "PHGMethod"),
    definition = function(object) {
        conObj    <- phgConObj(object)
        conType   <- phgType(conObj)
        conMethod <- phgMethodId(object)
        conDemo   <- isDemo(object)

        if (conType == "local") {
            hapIdsFromLocal(conObj, conMethod)
        } else if (conType == "server") {
            hapIdsFromSever(conObj, conMethod, conDemo)
        }
    }
)


## ----
#' @rdname readPHGDataSet
#' @export
setMethod(
    f = "readPHGDataSet",
    signature = signature(object = "PHGMethod"),
    definition = function(object, verbose = TRUE) {
        conObj    <- phgConObj(object)
        conType   <- phgType(conObj)
        conMethod <- phgMethodId(object)
        conDemo   <- isDemo(object)

        if (conType == "local") {
            phgDataSetFromLocal(conObj, conMethod, verbose)
        } else if (conType == "server") {
            phgDataSetFromServer(conObj, conMethod, verbose, conDemo)
        }
    }
)


