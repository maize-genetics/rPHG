## ----
# Return method table from local PHG object via Java/Kotlin API
#
# @param configFile A configuration file for a local PHG DB
# @param showAdvancedMethods Do you want to return all possible method IDs
#    from the database? Defaults to `FALSE`.
methodTableFromLocal <- function(configFile, showAdvancedMethods) {
    # Get TableReport object from TASSEL jar and convert to data.frame
    plugin <- rJava::new(
        rJava::J(TASSEL_API$METHOD_TABLE_REPORT)
    )
    plugin <- plugin$configFile(configFile)
    ds <- plugin$performFunction(
        rJava::.jnull(TASSEL_API$DATA_SET)
    )
    tabRep <- ds$getDataSet()$get(0L)$getData()
    tabRepDf <- tableReportToDF(tabRep)

    # Convert description field to column of parsed lists (key = value)
    tabRepDf$description <- lapply(
        X   = tabRepDf$description,
        FUN = descriptionStringToList
    )

    # Remove method table DB ids (not relevant to user)
    tabRepDf$num_refranges <- NA
    tabRepDf$num_samples <- NA
    colsToKeep <- c(
        "type_name",
        "method_name",
        # "num_refranges",
        # "num_samples",
        "description"
    )
    tabRepDf <- tabRepDf[, colsToKeep]

    # Return only PATHS or all data
    if (showAdvancedMethods) {
        return(tabRepDf)
    } else {
        return(tabRepDf[tabRepDf$type_name == "PATHS", ])
    }
}


## ----
# Return method table from for PHG server using BrAPI endpoints
#
# @param url A URL to a PHG server
# @param showAdvancedMethods Do you want to return all possible method IDs
#    from the database? Defaults to `FALSE`.
methodTableFromServer <- function(url, showAdvancedMethods) {
    tableUrl <- file.path(url, BRAPI_ENDPOINTS$METHOD_TABLE)
    jsonObj  <- parseJSON(tableUrl)
    methodDf <- jsonObj$result$data$additionalInfo

    # Bandage before public PHG is updated...
    if (any(is.na(methodDf))) {
        methodDf <- jsonObj$result$data
        methodDf$description <- NA
        methodDf$type_name <- NA

        # Make consistent names with local method table call
        idOrderAndMapping <- c(
            "type_name"        = "type_name",
            "variantTableDbId" = "method_name",
            "additionalInfo"   = "description"
        )
    } else {
        idOrderAndMapping <- c(
            "type_name"      = "type_name",
            "variantSetDbId" = "method_name",
            "description"    = "description"
        )
    }

    # Process column names
    for (oldName in names(methodDf)) {
        if (oldName %in% names(idOrderAndMapping)) {
            newName <- idOrderAndMapping[oldName]
            names(methodDf)[names(methodDf) == oldName] <- newName
        }
    }
    methodDf <- tibble::as_tibble(methodDf[, idOrderAndMapping])

    # Convert description field to column of parsed lists (key = value)
    if (!any(is.na(jsonObj$result$data$additionalInfo))) {
        methodDf$description <- lapply(
            X   = methodDf$description,
            FUN = descriptionStringToList
        )
    }

    if (showAdvancedMethods) {
        return(methodDf)
    } else {
        if (!any(is.na(jsonObj$result$data$additionalInfo))) {
            return(methodDf[methodDf$type_name == "PATHS", ])
        } else {
            return(methodDf[grepl("_PATH$|_PATHS$", methodDf$method_name), ])
        }
    }
}


