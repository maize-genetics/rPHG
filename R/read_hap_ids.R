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
hapIdsFromSever <- function(conObj, conMethod) {
    print("WIP for `hapIdsFromServer`")
}


